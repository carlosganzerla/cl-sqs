WITH RECURSIVE messages AS (
    SELECT 
        *,
        pg_try_advisory_xact_lock(next_message.id) has_lock
    FROM (
        SELECT
            *
        FROM
            queue
        WHERE
            visible_at <= NOW()
			expires_at <= NOW()
        ORDER BY
            created_at
        LIMIT 1
    ) next_message
    UNION ALL
    SELECT
        rest.*,
        pg_try_advisory_xact_lock(rest.id) has_lock
    FROM (
        SELECT
            queue.*
        FROM
            messages
        LEFT JOIN LATERAL (
            SELECT
                *
            FROM
                queue
            WHERE
                visible_at <= NOW() AND
				expires_at <= NOW()
                created_at > messages.created_at
            ORDER BY
                created_at
            LIMIT 1
        ) queue
        ON
            true
        WHERE
            queue.id IS NOT NULL
        LIMIT 1
    ) rest
),
next_message AS (
    SELECT 
        * 
    FROM 
        messages 
    WHERE 
        has_lock
    LIMIT 1
)
UPDATE
    queue
SET
    visible_at = NOW() + $1 * INTERVAL '1 SECOND',
    read_count = next_message.read_count + 1
FROM
    next_message
WHERE
    queue.id = next_message.id
RETURNING 
    queue.payload,
    (extract(EPOCH from queue.created_at) * 1000)::bigint "message-timestamp";
