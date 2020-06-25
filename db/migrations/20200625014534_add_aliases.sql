-- migrate:up
CREATE TABLE aliases (
      user_id        BIGINT NOT NULL,
      name           TEXT NOT NULL,
      value          TEXT NOT NULL,
      PRIMARY KEY(user_id, name)
);

-- migrate:down
DROP TABLE aliases;
