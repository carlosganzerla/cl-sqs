WITH next_message AS (
    SELECT
        *
    FROM
        queue
    WHERE
        expires_at > now() AND
        visible_at <= now()
    ORDER BY
        visible_at
    LIMIT 1
    FOR UPDATE
)
UPDATE
    queue
SET
    visible_at = clock_timestamp() + $1 * INTERVAL '1 SECOND',
    read_count = next_message.read_count + 1
FROM
    next_message
WHERE
    next_message.id = queue.id
RETURNING 
    queue.payload,
    (extract(EPOCH from queue.visible_at) * 1000000) "message-timestamp";

