-- migrate:up
CREATE TABLE guilds (
       id BIGINT PRIMARY KEY,
       last_seen TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- migrate:down

DROP TABLE guilds;
