CREATE EXTENSION IF NOT EXISTS pgplsql;
CREATE EXTENSION IF NOT EXISTS uuid-ossp;

CREATE TABLE queue (
    id uuid NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
    deduplication_id NOT NULL UNIQUE,
    payload jsonb NOT NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    visible_at timestamptz NOT NULL DEFAULT NOW(),
    retry_count integer NOT NULL DEFAULT 0
);


-- DEQUEUE
WITH next_message AS (
    SELECT
        *
    FROM
        queue
    WHERE
        visible_at <= NOW()
    ORDER BY
        created_at
    LIMIT 1
    FOR SHARE
)
UPDATE
    queue
SET
    visible_at = NOW() + INTERVAL '300 SECONDS'
FROM
    next_message
WHERE
    queue.id = next_message.id
RETURNING
    queue.*;

-- ENQUEUE
INSERT INTO
    queue (
        payload,
        deduplication_id
    )
    VALUES (
        $1,
        COALESCE($2, encode(sha256($1::bytea), 'base64'))
    )
RETURNING *;
