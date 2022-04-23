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

## API

## Security

### Database

### Authentication

### SSL

## Executing

## Benchmarks
