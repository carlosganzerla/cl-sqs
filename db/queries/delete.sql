DELETE FROM 
    queue
WHERE
    id = $1
RETURNING json_build_object('id', queue.id);
