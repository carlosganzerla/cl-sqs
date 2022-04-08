WITH previous_message AS (
    DELETE FROM
        message
    WHERE
        receipt_id = '6a3f3d49-dc19-483b-b1fa-a3c0d1abd7bf' AND
        group_head = true
    RETURNING group_id
),
next_message AS (
    SELECT 
        *
    FROM
        message
    INNER JOIN
        previous_message
    ON
        previous_message.group_id = message.group_id
    WHERE
        group_head = false
    ORDER BY
        created_at
    LIMIT 1
)
UPDATE
    message
SET
    group_head = true
FROM
    next_message
WHERE
    message.id = next_message.id
RETURNING *;
