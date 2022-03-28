SELECT
    payload,
    (extract(EPOCH from created_at) * 1000)::bigint "message-timestamp"
FROM
    f_dequeue($1);
