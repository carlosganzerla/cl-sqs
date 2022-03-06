CREATE EXTENSION IF NOT EXISTS pgplsql;
CREATE EXTENSION IF NOT EXISTS uuid-ossp;

CREATE TABLE queue (
    id uuid NOT NULL PRIMARY KEY DEFAULT uuid_generate_v4(),
    deduplication_id varchar(128) NOT NULL UNIQUE,
    payload varchar(65535) NOT NULL,
    created_at timestamptz NOT NULL DEFAULT NOW(),
    visible_at timestamptz NOT NULL, 
    expires_at timestamptz NOT NULL,
    read_count integer NOT NULL DEFAULT 0
);
