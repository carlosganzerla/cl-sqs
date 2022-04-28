SELECT 
    f_delete "message-id"
FROM
    f_delete($1)
WHERE
    f_delete IS NOT NULL;

