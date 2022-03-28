WITH timestamp AS (
    SELECT NOW()
),
next_message AS (
    SELECT
        queue.*,
        timestamp.now
    FROM
        queue
    LEFT JOIN
        timestamp
    ON
        true
    WHERE
        expires_at > timestamp.now AND
        visible_at <= timestamp.now
    ORDER BY
        created_at
    LIMIT 1
    FOR UPDATE

)
UPDATE
    queue
SET
    visible_at = next_message.now + $1 * INTERVAL '1 SECOND',
    read_count = next_message.read_count + 1
FROM
    next_message
WHERE
    next_message.id = queue.id
RETURNING 
    queue.payload,
    (extract(EPOCH from queue.created_at) * 1000)::bigint "message-timestamp";

