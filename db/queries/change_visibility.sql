UPDATE
    message
SET
    visible_at = NOW() + $2 * INTERVAL '1 SECOND'
WHERE
    receipt_id = $1 AND
    group_head = true
RETURNING
    message.id "message-id";

