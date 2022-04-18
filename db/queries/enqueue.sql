SELECT
    message.id "message-id",
    md5(message.payload) "message-md5",
    (extract(EPOCH from message.created_at) * 1000000) "message-timestamp"
FROM
    f_enqueue($1, $2, $3) message;
