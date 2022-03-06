CREATE EXTENSION IF NOT EXISTS pgplsql;
CREATE EXTENSION IF NOT EXISTS uuid-ossp;

CREATE TABLE queue (
    id uuid NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
    deduplication_id varchar(128) NOT NULL UNIQUE,
    payload jsonb NOT NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    visible_at timestamptz NOT NULL, 
    expirest_at timestamptz NOT NULL,
    read_count integer NOT NULL DEFAULT 0
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
    visible_at = NOW() + $1 * INTERVAL '1 SECOND',
    read_count = read_count + 1
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
        deduplication_id,
        visible_at,
        expirest_at
    )
    VALUES (
        $1,
        COALESCE($2, encode(sha256($1::jsonb::text::bytea), 'base64')),
        visible_at = NOW() + $3 * INTERVAL '1 SECOND',
        expirest_at = NOW() + $4 * INTERVAL '1 HOUR'
    )
RETURNING *;

-- DELETE
DELETE FROM queue WHERE id = $1;

-- PATCH
UPDATE
    queue
SET
    visible_at = NOW() + $2 * INTERVAL '1 SECOND'
WHERE
    id = $1
RETURNING *;
