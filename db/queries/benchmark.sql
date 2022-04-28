INSERT INTO
    message (
        id,
        group_id,
        payload,
        deduplication_id,
        created_at,
        visible_at,
        group_head
    )
SELECT
    gen_random_uuid(),
    (series % $1)::text,
    series::text,
    series::text,
    NOW() + (series % $1) * INTERVAL '1 SECOND',
    NOW() + (RANDOM() * 30 - 15) * INTERVAL '1 MINUTE',
    CASE
        WHEN series < $1 THEN true
        ELSE false
    END
FROM
    generate_series(0, $2 - 1) series;
