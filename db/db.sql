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
    RETURNS SETOF queue
    LANGUAGE 'plpgsql'
AS $BODY$
DECLARE
    next_message_id integer;
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
    FOR UPDATE;

    RETURN QUERY
        UPDATE
            queue
        SET
            visible_at = NOW() + p_visibility_timeout * INTERVAL '1 SECOND',
            read_count = read_count + 1
        WHERE
            queue.id = next_message_id
        RETURNING 
            *;
END
$BODY$

CREATE OR REPLACE FUNCTION f_enqueue (
    p_payload text,
    p_deduplication_id varchar(65535),
    p_visibility_timeout integer,
    p_retention_timeout integer
)
    RETURNS SETOF queue
    LANGUAGE 'plpgsql'
AS $BODY$
BEGIN
    PERFORM (
        SELECT
            queue.id
        FROM
            queue
        WHERE
            expires_at > NOW() AND
            visible_at <= NOW()
        ORDER BY
            created_at
        LIMIT 1
        FOR UPDATE
    );

    RETURN QUERY
        INSERT INTO
            queue (
                payload,
                deduplication_id,
                visible_at,
                expires_at
            )
            VALUES (
                p_payload,
                COALESCE(
                    p_deduplication_id,
                    encode(sha256(p_payload::bytea)::bytea, 'hex')),
                NOW() + p_visibility_timeout * INTERVAL '1 SECOND',
                NOW() + p_retention_timeout * INTERVAL '1 HOUR'
            )
            ON CONFLICT (deduplication_id)
            DO NOTHING
            RETURNING 
                *;
END
$BODY$
