WITH next_message AS (
    SELECT
        *,
        NOW() "timestamp"
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
    visible_at = next_message.timestamp + $1 * INTERVAL '1 SECOND',
    read_count = next_message.read_count + 1
FROM
    next_message
WHERE
    next_message.id = queue.id
RETURNING 
    queue.payload,
    (extract(EPOCH from queue.created_at) * 1000)::bigint "message-timestamp";

