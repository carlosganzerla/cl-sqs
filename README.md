# cl-sqs

The intent of this project is to implement a lightweight queue API akin to [AWS
SQS](https://aws.amazon.com/sqs/). The idea is to provide a fast API with fewer
constraints that's entirely on the user control. The back-end server is
[`woo`](https://github.com/fukamachi/woo) and the persistence medium is a
Postgres instance. I decided to use CL because I wanted to give a shot on a
project with a broader scope, and test web servers and containers for CL. It
should be easy enough to port to another back-end, although Woo seemed pretty
fast (and is according to their benchmark).

The concepts were borrowed from SQS, but the design of the API is slightly
different.  A single table was designed and a few other support constructs are
provided to allow the storage of messages, and the usage of indexes provide
fast concurrent enqueue/dequeue operations. Unlike SQS, there aren't built-in
queue-wide configurations. Any customizations can be made by the user at will.

The API is unopinionated about input/output format. So all input/outputs are
considered plain text. It's up to the user define which type of message they'll
send to the queue. This allows users to use XML or even S-Expressions as
message formats. Additional message data is sent on the response headers.

## How the queue works

The queue is very similar to SQS. In fact, it can be viewed as various queues.
Messages are posted by consumers, and the ordering of the messages is the
creation time stamp on the persistence medium (unlike SQS, which uses the
sender time stamp). When enqueueing a message, each producer must specify a
group id and a de-duplication id. Messages belonging to a specific group id can
be viewed as a queue on their own. The de-duplication id prevents
adding duplicate messages to the queue, for a particular group, until that
message is deleted (SQS, on the other hand, allows choosing the de-duplication
scope and the de-duplication id is only valid on a particular time window).

Since the queue is meant to be used on distributed systems, when a consumer
dequeues a message it's not deleted automatically. It becomes invisible for
other consumers, until the visibility timeout has passed. If the timeout has
passed, and the message was not deleted, other consumers may see it. Also, if a
message for a given group is received, but not deleted, no messages from the
same group can be retrieved, until that message is deleted. Each time a message
is received, the API returns a receipt id. This id changes each time a given
message is read. This receipt is used to delete the message. This ensures that
only the consumer which last received the message can delete it. Also, the
receipt id can be used to change the visibility timeout of a given message.
This can be useful if the message is meant to be read again by other consumers,
after some processing finishes successfully.

## API

The API is very simple. One path `/queue` is used on all requests. Each of
the four operations uses a semantically correspondent HTTP method. To
authenticate requests, use the `api-key` header on the request (see more on the
section below). This header can be omitted if no `API_KEY` is set. Only the
payload is sent on the request body, as plain text (`text/plain`). All
parameters are sent via URL params.

The API is unopinionated about message format. Everything is seen as plain
text. There some downsides, which most of you already know, but let me talk
about the upsides:

- No time spent on validation (this can make a difference on larger payloads).
- User can choose the format that bests suits them (S-Expressions is a
 classical example).
- Users can use multiple formats (this is very rare, and likely to be a bad
 idea, but sometimes it can be useful due to some external constraint).

Since the API is unopinionated about the format, the are two types of data
returned by a request:

- On the body: The payload (only on dequeue)
- On the headers: additional message data (timestamps, message id, receipt id,
 etc.)

Data is returned only on a successful code `200` response. To differentiate
between errors, empty responses and good responses, the
semantic HTTP status codes are used:

- `200`: Response returned data successfully. Additional headers should be
 present.
- `204`: Response returned successfully, but with no data.
- `401`: Unauthorized request (wrong `api-key` request header).
- `404`: URL not found.
- `405`: Method not allowed.
- `422`: Invalid URL params.
- `500`: Error on the app. See the logs.

Since the URL params are very simple, only unexpected errors are logged.

### `POST`: Enqueue

URL Parameters:

- `group-id`: Required. Can be any string from 1 to 128 characters.
- `deduplication-id`. Optional. Can be any string from 1 to 128 characters. If
 not supplied, the de-duplication id is auto-generated making the SHA-1 hash
 of the payload

Request Body: The payload text.

Response headers:

- `Message-Id`: The id of the message on the database.
- `Message-Md5`. The MD5 hash of the payload,
- `Message-Timestamp`: The creation time stamp of the message on the
 persistence medium. Use this to assert ordering if desired.


Example:

```shell
curl 'http://localhost:5000/queue?group-id=my-fancy-group' -d 'Waddap' -v

< HTTP/1.1 200 OK
< Date: Wed, 27 Apr 2022 01:45:31 GMT
< Connection: keep-alive
< Content-Type: text/plain
< Message-Id: ebb87403-bdf1-4608-b810-c5d56d8b0c66
< Message-Md5: c59688cd1ef3ed3c377f240939d63a5a
< Message-Timestamp: 1651024563183145
< Transfer-Encoding: chunked
```

Try the same request again, and we would have:

```shell
< HTTP/1.1 204 No Content
< Date: Wed, 27 Apr 2022 01:46:44 GMT
< Connection: keep-alive
< Content-Type: text/plain
< Transfer-Encoding: chunked
```

But if we add the `deduplication-id`, then we'd have:

```shell
curl 'http://localhost:5000/queue?group-id=my-fancy-group&deduplication-id=no-dupes' -d 'Waddap' -v

< HTTP/1.1 200 OK
< Date: Wed, 27 Apr 2022 01:47:19 GMT
< Connection: keep-alive
< Content-Type: text/plain
< Message-Id: fe5d0964-af81-435b-9e36-8809260e1716
< Message-Md5: c59688cd1ef3ed3c377f240939d63a5a
< Message-Timestamp: 1651024577831246
< Transfer-Encoding: chunked
```

### `GET`: Dequeue

URL Parameters:

- `visibility-timeout`: Optional. Any integer between 0 and 86400. The default
 is 60. Note that if you set to 0, the dequeue operation will always return
 the same message.

```shell
curl 'http://localhost:5000/queue?visibility-timeout=10' -v

< HTTP/1.1 200 OK
< Date: Wed, 27 Apr 2022 01:56:40 GMT
< Connection: keep-alive
< Content-Type: text/plain
< Message-Receipt-Id: 2af18f7d-6475-5e64-87f3-e8e7334810ac
< Message-Id: 24648dca-7cdc-4c76-a2bf-94e50bb949f3
< Message-Timestamp: 1651024563183145
< Transfer-Encoding: chunked
...
Waddap
```

If we retry the curl before 10 seconds have elapsed, we'd have:

```shell
< HTTP/1.1 204 No Content
< Date: Wed, 27 Apr 2022 01:56:43 GMT
< Connection: keep-alive
< Content-Type: text/plain
< Transfer-Encoding: chunked
```

After this period, the first response would repeat.

Note that the second enqueued message didn't show up in the second request.
That's because they have the same group id. If they were different, it would
show up.

### `DELETE`: Delete message

### `PATCH`: Change visibility

## Security

### Authentication

There's an optional `API_KEY` environment variable that lets the user define
an authentication key. Any crypto-safe randomly generated string of a decent
size could do the trick (if you have SSL. See below.). I decided to keep
authentication very simple, since the program would run on a controlled
infrastructure. A big enough api key could be really safe.

### Database

All messages are stored on the `message` table, along with their payloads and
other data. More details on the [database](db/db.sql) file. Check the
[queries](db/queries) folder, also. It's recommended to create a user with the
appropriate permissions on production. I didn't create one to not make an
opinion about password generation/parameterization.

### SSL

`woo` does not have SSL, support, but so does a good number of back-ends. SSL
can be added using proxy servers like [ngnix](https://nginx.org/en/) or
[haproxy](https://www.haproxy.org/). I didn't test those myself yet, but I plan
to do it some time soon.

## Executing

### Env vars

The following env vars are used to configure the app:

- `DB_HOST`: Host name of the Postgres instance (defaults to `localhost`).
- `DB_PORT`: Port of the Postgres instance (defaults to `5432`).
- `DB_USER`: User of the Postgres instance (defaults to `postgres`)
- `DB_PASSWORD`: Password of the Postgres instance (defaults to `postgres`).
- `DB_NAME`: Name of the Postgres instance database (defaults to `postgres`).
- `API_KEY`: The API key used to authenticate requests. Defaults to `nil`, in
 which case no authentication is used.
- `WOO_ARGS`: A prop-list containing the keyword args of `woo:run`. Defaults to
 `nil`. The values on `docker-compose.yml` seem satisfactory for basic
 production usage.

### Running the app

`cl-sqs` is meant to run as a Docker container. A `docker-compose.yml` is
provided to create an easy-to-use development environment. Usage on production
would better have a separate DB instance. To use `docker-compose`, simply:

```shell
docker-compose up
```

And it should build the image automatically.

Alternatively, build the image with:

```shell
docker build -t cl-sqs .
```

And run the container with the desired options (see the [`docker
run`](https://docs.docker.com/engine/reference/commandline/run/) for more
information.)

**Note**: This image is by no means an optimized image. It's fast enough, but I
could not find a slim Common Lisp image.

To run on the REPL:

```lisp
(ql:quickload :cl-sqs)
(cl-sqs:start)
```

Make sure that the folder you've cloned the code to is registered on quicklisp
local projects repository.

To run the tests, it's recommend to use the `docker-compose.yml`. I didn't use
a unit-test framework, since there aren't unit tests, just "integration" tests:

```lisp
(ql:quickload :cl-sqs-test)
;; ensures that the queues works under concurrency
(cl-sqs-test:multi-threaded-test) 
;; benchmarks the database
(cl-sqs-test:database-benchmark) 
```

Check the [tests code](t/) to see more details.

## Benchmarks

Benchmarks on distributed systems must be taken with a grain of salt, specially
if the conditions of the experiment are not well known. I benchmarked locally,
so the numbers may vary a lot for each person. The queries are optimized with
indexes, which are working well. `woo` has it's own benchmark, which I'm
assuming has valid results. I didn't test on an actual distributed scenario. I
plan to do that some other time.

For 1 million messages, the average time of each operation is pretty much
similar, something like ~3ms. This goes up, until the query index points to a
new page, and then go slightly down. For 10 million messages I didn't do
extensive testing (takes too long to insert all that lol), but the results were
more or less the same.

I didn't test  concurrent `cl-sqs` instances, but it's assumed to work if they
point to the same Postgres instance (as the DB manages all concurrency). A load
balancer could be used to achieve good performance on a high concurrency
multiple-instance scenario.
