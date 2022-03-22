UPDATE
    queue
SET
    visible_at = NOW() + $2 * INTERVAL '1 SECOND'
WHERE
    id = $1
RETURNING queue.id "message-id";

