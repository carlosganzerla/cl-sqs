INSERT INTO 
    queue (
        deduplication_id,
        payload,
        visible_at,
        expires_at
    ) 
SELECT 
    series::text, 
    series::text,
    CURRENT_TIMESTAMP + (random() * 50 - 30 ) * INTERVAL '1 hour',
    NOW() + INTERVAL '1 YEAR'
    FROM 
        generate_series(1,$1) series;
