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
