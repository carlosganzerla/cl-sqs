INSERT INTO
    queue (
        payload,
        deduplication_id,
        visible_at,
        expires_at
    )
    VALUES (
        $1::text,
        COALESCE($2, encode(sha256($1::bytea)::bytea, 'hex')),
        NOW() + $3 * INTERVAL '1 SECOND',
        NOW() + $4 * INTERVAL '1 HOUR'
    )
ON CONFLICT (deduplication_id)
DO NOTHING
RETURNING 
    md5(queue.payload) "message-md5",
    (extract(EPOCH from queue.visible_at) * 1000000) "message-timestamp";
