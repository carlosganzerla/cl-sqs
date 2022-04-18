SELECT
    max(group_head),
    count(*)
FROM
    message
GROUP BY
    group_id
