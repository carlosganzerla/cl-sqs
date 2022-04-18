CREATE TABLE message (
    id uuid NOT NULL PRIMARY KEY,
    group_id varchar(128) NOT NULL,
    deduplication_id varchar(128) NOT NULL,
    group_head boolean NOT NULL,
    payload varchar(65535) NOT NULL,
    visible_at timestamptz NOT NULL, 
    created_at timestamptz NOT NULL DEFAULT NOW(),
    read_count integer NOT NULL DEFAULT 0,
    receipt_id uuid,
    UNIQUE (group_id, deduplication_id)
);

CREATE UNIQUE INDEX message_receipt_id_key ON message (receipt_id)
    WHERE receipt_id IS NOT NULL;

CREATE INDEX message_created_at_idx ON message (created_at);

CREATE UNIQUE INDEX message_group_id_group_head_key 
    ON message (group_id, group_head)
    WHERE group_head = true;

CREATE OR REPLACE FUNCTION f_enqueue (
    p_message_group_id varchar(128),
    p_payload varchar(65535),
    p_deduplication_id varchar(128)
)
    RETURNS SETOF message
    LANGUAGE 'plpgsql'
    ROWS 1
AS $BODY$
BEGIN
    PERFORM pg_advisory_xact_lock(hashtextextended(p_message_group_id, 0));
    RETURN QUERY
        INSERT INTO
            message (
                id,
                group_id,
                payload,
                deduplication_id,
                created_at,
                visible_at,
                group_head
            )
        VALUES (
            gen_random_uuid(),
            p_message_group_id,
            p_payload,
            COALESCE(
                p_deduplication_id,
                encode(sha256(p_payload::bytea)::bytea, 'hex')
            ),
            NOW(),
            NOW(),
            NOT EXISTS (
                SELECT group_id FROM message WHERE group_id = p_message_group_id
            )
        )
    ON CONFLICT (group_id, deduplication_id) DO NOTHING
    RETURNING *;
END
$BODY$
