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
VALUES (
    gen_random_uuid(),
    $1::text,
    $2::text,
    COALESCE($3, encode(sha256($2::text::bytea)::bytea, 'hex')),
    NOW(),
    NOW(),
    NOT EXISTS (SELECT group_id FROM message WHERE group_id = $1::text)

)
ON CONFLICT (group_id, deduplication_id) DO NOTHING
RETURNING 
    message.id "message-id",
    md5(message.payload) "message-md5",
    (extract(EPOCH from message.created_at) * 1000000) "message-timestamp";
