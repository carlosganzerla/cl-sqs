DELETE FROM 
    queue
WHERE
    id = $1
RETURNING queue.id "message-id";
