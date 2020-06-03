-- migrate:up
CREATE TABLE prefixes (
       guild_id BIGINT NOT NULL REFERENCES guilds (id) ON DELETE CASCADE,
       pre TEXT NOT NULL,
       PRIMARY KEY(guild_id, pre)
);

-- migrate:down
DROP TABLE prefixes;
