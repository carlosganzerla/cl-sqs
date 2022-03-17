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
    read_count = next_message.read_count + 1
FROM
    next_message
WHERE
    queue.id = next_message.id
RETURNING json_build_object(
    'id', queue.id,
    'payload', queue.payload
);
