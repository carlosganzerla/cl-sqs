WITH previous_message AS (
    DELETE FROM
        message
    WHERE
        receipt_id = $1 AND
        group_head = true
    RETURNING id, group_id
),
next_message AS (
    SELECT 
        message.id
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
),
updated_message AS (
    UPDATE
        message
    SET
        group_head = true
    FROM
        next_message
    WHERE
        message.id = next_message.id
    RETURNING *
)
SELECT
    previous_message.id "message-id"
FROM
    previous_message
LEFT JOIN
    updated_message
ON
    true;
