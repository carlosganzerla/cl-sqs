BEGIN;
    DO
    $$
        DECLARE
            group_count integer := 10000;
            total_messages integer := 10000000;
        BEGIN
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
                (series % group_count)::text,
                series::text,
                series::text,
                NOW() + (series % group_count) * INTERVAL '1 SECOND',
                NOW() + (RANDOM() * 30 - 15) * INTERVAL '1 MINUTE',
                CASE
                    WHEN series < group_count THEN true
                    ELSE false
                END
            FROM 
                generate_series(0, total_messages) series;
        END;
    $$;
COMMIT;
