SELECT
    message.id "message-id",
    message.payload,
    message.receipt_id "message-receipt-id",
    (extract(EPOCH from message.created_at) * 1000000) "message-timestamp"
FROM
    f_dequeue($1) message;
