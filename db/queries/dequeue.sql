WITH uuid AS (
    SELECT uuid_generate_v4() value
),
new_group AS (
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
    ON CONFLICT (id) DO NOTHING
    RETURNING *
),
incoming_group AS (
    SELECT
        id
    FROM
        new_group
    UNION
    SELECT
        id
    FROM
        message_group
    WHERE
        id = $1
)
INSERT INTO
    message (
        id,
        payload,
        message_group_id,
        deduplication_id,
        visible_at
    )
SELECT
    uuid.value,
    $2,
    incoming_group.id,
    COALESCE($3, encode(sha256($2::bytea)::bytea, 'hex')),
    NOW()
FROM
    incoming_group
INNER JOIN
    uuid
ON
    true
ON CONFLICT (deduplication_id, message_group_id) DO NOTHING
RETURNING 
    md5(message.payload) "message-md5",
    (extract(EPOCH from message.created_at) * 1000000) "message-timestamp";
