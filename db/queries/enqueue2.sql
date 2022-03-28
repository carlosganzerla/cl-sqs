SELECT
    md5(payload) "message-md5",
    (extract(EPOCH from created_at) * 1000)::bigint "message-timestamp"
FROM
    f_enqueue($1,$2,$3,$4);
