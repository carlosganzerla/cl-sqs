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
