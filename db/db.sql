CREATE EXTENSION IF NOT EXISTS pgplsql;

CREATE TABLE message_group (
    id varchar(128) NOT NULL PRIMARY KEY,
    created_at timestamptz NOT NULL DEFAULT NOW()
);

CREATE TABLE message (
    id uuid NOT NULL PRIMARY KEY,
    message_group_id varchar(128) NOT NULL REFERENCES message_group (id),
    deduplication_id varchar(128) NOT NULL,
    payload varchar(65535) NOT NULL,
    visible_at timestamptz NOT NULL, 
    group_head boolean NOT NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    read_count integer NOT NULL DEFAULT 0,
    receipt_id uuid,
    UNIQUE (message_group_id, deduplication_id)
);

CREATE UNIQUE INDEX message_receipt_id_key ON message (receipt_id)
    WHERE message.receipt_id IS NOT NULL;

CREATE INDEX message_created_at_idx ON message (created_at);
