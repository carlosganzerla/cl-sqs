INSERT INTO
    queue (
        payload,
        deduplication_id,
        visible_at,
        expires_at
    )
    VALUES (
        $1::text,
        COALESCE($2, encode(sha256($1::bytea), 'base64')),
        NOW() + $3 * INTERVAL '1 SECOND',
        NOW() + $4 * INTERVAL '1 HOUR'
    )
ON CONFLICT (deduplication_id)
DO NOTHING
RETURNING json_build_object(
    'id', queue.id,
    'payload_md5', md5(payload)
);
