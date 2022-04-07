CREATE EXTENSION IF NOT EXISTS pgplsql;

CREATE TABLE message_group (
    id varchar(128) NOT NULL PRIMARY KEY,
    front_message_id uuid NOT NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    updated_at timestamptz,
    message_count integer NOT NULL DEFAULT 1
);

CREATE TABLE message (
    id uuid NOT NULL PRIMARY KEY,
    message_group_id varchar(128) NOT NULL REFERENCES message_group (id),
    deduplication_id varchar(128) NOT NULL,
    payload varchar(65535) NOT NULL,
    visible_at timestamptz NOT NULL, 
    created_at timestamptz NOT NULL DEFAULT NOW(),
    read_count integer NOT NULL DEFAULT 0,
    UNIQUE (group_id, deduplication_id)
);

SELECT
    *
FROM
    message_group
INNER JOIN
   queue 
ON
    queue.id = message_group.front_id
WHERE
    queue.visible_at >= NOW()
ORDER BY
    queue.created_at
