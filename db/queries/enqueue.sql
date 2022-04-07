WITH uuid AS (
    SELECT uuid_generate_v4() value
),
incoming_group AS (
    INSERT INTO
        message_group (
            id,
            front_message_id
        )
    SELECT
        $1,
        value
    FROM
        uuid
    ON CONFLICT (id) DO UPDATE SET
        message_count = EXCLUDED.message_count + 1,
        updated_at = NOW()
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
    uuid.value,
    incoming_group.id,
    $2::text,
    COALESCE(null, encode(sha256($2::bytea)::bytea, 'hex')),
    NOW(),
    NOW()
FROM
    incoming_group
INNER JOIN
    uuid
ON
    true
ON CONFLICT (deduplication_id, message_group_id) DO NOTHING
RETURNING 
    message.id "message-id",
    md5(message.payload) "message-md5",
    (extract(EPOCH from message.created_at) * 1000000) "message-timestamp";
