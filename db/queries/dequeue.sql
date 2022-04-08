WITH next_message AS (
    SELECT
        message.id,
        message.message_group_id
    FROM
        message
    INNER JOIN
        message_group 
    ON
        message.id = message_group.front_message_id AND
        message.message_group_id = message_group.id
    WHERE
        message.visible_at <= NOW()
    ORDER BY
        message.created_at
    LIMIT 1
    FOR UPDATE
)
UPDATE
    message
SET
    visible_at = NOW() + $1 * INTERVAL '1 SECOND',
    receipt_id = uuid_generate_v5(
        next_message.id,
        concat(NOW()::text, message.message_group_id)
    )
FROM
    next_message
WHERE
    next_message.id = message.id
RETURNING
    message.id "message-id",
    message.payload,
    (extract(EPOCH from message.created_at) * 1000000) "message-timestamp";
