CREATE EXTENSION IF NOT EXISTS pgplsql;
CREATE EXTENSION IF NOT EXISTS uuid_ossp;

CREATE TABLE queue (
    id bigserial NOT NULL PRIMARY KEY,
    deduplication_id varchar(128) NOT NULL UNIQUE,
    payload varchar(65535) NOT NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    visible_at timestamptz NOT NULL, 
    expires_at timestamptz NOT NULL,
    read_count integer NOT NULL DEFAULT 0
);

CREATE INDEX queue_created_at_idx ON queue USING btree (created_at);

