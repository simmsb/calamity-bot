-- migrate:up
CREATE TABLE guilds (
       id BIGINT PRIMARY KEY,
       last_seen TIMESTAMPTZ NOT NULL
);

-- migrate:down

DROP TABLE guilds;
