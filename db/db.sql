CREATE EXTENSION IF NOT EXISTS pgplsql;
CREATE EXTENSION IF NOT EXISTS uuid_ossp;

CREATE TABLE queue (
    id bigserial NOT NULL PRIMARY KEY,
    deduplication_id varchar(128) NOT NULL UNIQUE,
    payload varchar(65535) NOT NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    visible_at timestamptz NOT NULL, 
    expires_at timestamptz NOT NULL,
    read_count integer NOT NULL DEFAULT 0
);

CREATE INDEX queue_created_at_idx ON queue USING btree (created_at);

CREATE OR REPLACE FUNCTION f_dequeue (
    p_visibility_timeout integer
)
    RETURNS SETOF point_transaction
    LANGUAGE 'plpgsql'
AS $BODY$
    next_message_id
BEGIN
    SELECT
        queue.id
    INTO
        next_message_id
    FROM
        queue
    WHERE
        expires_at > NOW() AND
        visible_at <= NOW()
    ORDER BY
        created_at
    LIMIT 1
    FOR UPDATE
)
UPDATE
    queue
SET
    visible_at = NOW() + $1 * INTERVAL '1 SECOND',
    read_count = next_message.read_count + 1
FROM
    next_message
WHERE
    next_message.id = queue.id
RETURNING 
    queue.payload,
    (extract(EPOCH from queue.created_at) * 1000)::bigint "message-timestamp";


    RETURN;
END
$BODY$\

