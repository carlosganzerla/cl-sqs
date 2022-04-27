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
different.  A single table was and a few other support constructs are provided
to allow the storage of messages, and the usage of indexes provide fast
concurrent enqueue/dequeue operations. Unlike SQS, there aren't built-in
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

## Security

### Database

All messages are stored on the `message` table, along with their payloads and
other data. More details on the [database](db/db.sql) file. Check the
[queries](db/queries) folder, also. It's recommended to create a user with the
appropriate permissions on production. I didn't create one to not make an
opinion about password generation/parameterization.

### Authentication

There's an optional `API_KEY` environment variable that lets the user define
an authentication key. Any crypto-safe randomly generated string of a decent
size could do the trick (if you have SSL. See below). I decided to keep
authentication very simple, since the program would run on your controlled
infrastructure.

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

### Running the code

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
