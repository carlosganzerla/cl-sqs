WITH visible_messages AS (
    SELECT
        id,
        created_at
    FROM
        message
    WHERE
        group_head = true AND
        visible_at <= NOW()
    ORDER BY
        group_id
),
next_message AS (
    SELECT
        id
    FROM
        message
    WHERE
        group_head = true AND
        visible_at <= NOW()
    ORDER BY
        created_at
    LIMIT 1
    FOR UPDATE
)
UPDATE
    message
SET
    visible_at = NOW() + $1 * INTERVAL '1 SECOND',
    read_count = read_count + 1,
    receipt_id = uuid_generate_v5(
        next_message.id,
        concat(NOW()::text, message.group_id)
    )
FROM
    next_message
WHERE
    next_message.id = message.id
RETURNING
    message.payload,
    message.receipt_id "message-receipt-id",
    message.id "message-id",
    (extract(EPOCH from message.created_at) * 1000000) "message-timestamp";
