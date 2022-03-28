INSERT INTO 
    queue (
        deduplication_id,
        payload,
        created_at,
        visible_at,
        expires_at
    ) 
SELECT 
    series::text, 
    series::text,
    CURRENT_TIMESTAMP + (random()* 50 - 30 ) * INTERVAL '1 hour',
    CURRENT_TIMESTAMP + (random()* 10 - 2 ) * INTERVAL '1 hour',
    current_timestamp
    FROM 
        generate_series(1,2000000) series

WITH ranks AS (
    SELECT
        *,
        RANK() OVER (ORDER BY created_at) created_rank,
        RANK() OVER (ORDER BY visible_at) visibility_rank
    FROM 
        queue
    ORDER BY created_at
)
SELECT
    id,
    payload,
    created_at,
    created_rank,
    visible_at,
    visibility_rank
FROM
    ranks
WHERE created_rank <> visibility_rank
ORDER BY abs(created_rank - visibility_rank) DESC
