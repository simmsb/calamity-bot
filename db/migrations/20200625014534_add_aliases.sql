-- migrate:up
CREATE TABLE aliases (
      user_id        BIGINT NOT NULL,
      name           TEXT NOT NULL,
      value          TEXT NOT NULL,
      PRIMARY KEY(user_id, NAME)
);

CREATE INDEX "aliases_uid_name_desc_idx" ON aliases (user_id, name DESC);

-- migrate:down
DROP TABLE aliases;
