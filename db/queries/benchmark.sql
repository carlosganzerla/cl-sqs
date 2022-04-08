BEGIN;
    DO
    $$
        DECLARE
            group_count integer := 10000;
            total_messages integer := 10000000;
        BEGIN
            WITH new_rows AS (
                SELECT 
                    series::text payload,
                    (series % group_count)::text message_group_id,
                    gen_random_uuid() message_id,
                    NOW() + (series % group_count) * INTERVAL '1 SECOND' creation,
                    NOW() + (RANDOM() * 30 - 15) * INTERVAL '1 MINUTE' visibility
                FROM 
                    generate_series(0, total_messages - 1) series
            ),
            incoming_group AS (
                INSERT INTO
                    message_group (
                        id,
                        front_message_id,
                        group_visible_at
                    )
                SELECT
                    message_group_id,
                    message_id,
                    new_rows.visibility
                FROM
                    new_rows
                ON CONFLICT (id) DO NOTHING
                RETURNING *
            )
            INSERT INTO
                message (
                    id,
                    message_group_id,
                    payload,
                    deduplication_id,
                    created_at,
                    visible_at
                )
            SELECT
                new_rows.message_id,
                incoming_group.id,
                new_rows.payload,
                new_rows.payload,
                new_rows.creation,
                new_rows.visibility
            FROM
                incoming_group
            INNER JOIN
                new_rows
            ON
                new_rows.message_group_id = incoming_group.id;
            
            UPDATE
                message_group
            SET
                message_count = group_count;

        END;
    $$;
COMMIT;
