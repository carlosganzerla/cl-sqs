UPDATE
    queue
SET
    visible_at = NOW() + $2 * INTERVAL '1 SECOND'
WHERE
    id = $1
RETURNING json_build_object('id', queue.id);

