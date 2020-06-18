-- migrate:up
CREATE TABLE prefixes (
       guild__id BIGINT NOT NULL REFERENCES guilds (id) ON DELETE CASCADE,
       pre TEXT NOT NULL,
       PRIMARY KEY(guild__id, pre)
);

-- migrate:down
DROP TABLE prefixes;
