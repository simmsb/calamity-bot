-- migrate:up
CREATE TABLE guilds (
       id BIGINT PRIMARY KEY,
       last_seen TIMESTAMPTZ
);

-- migrate:down

DROP TABLE guilds;
