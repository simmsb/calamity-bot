-- migrate:up
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE FUNCTION random_text(INTEGER) RETURNS TEXT LANGUAGE SQL AS $$
SELECT array_to_string(array(
  SELECT SUBSTRING('23456789abcdefghjkmnpqrstuvwxyz'
    FROM floor(random()*31)::int+1 FOR 1)
  FROM generate_series(1, $1)), '');
$$;

CREATE TABLE reminders (
       id              text NOT NULL DEFAULT random_text(16),
       user_id         BIGINT NOT NULL,
       channel_id      BIGINT NOT NULL,
       message         TEXT NOT NULL,
       created         TIMESTAMPTZ NOT NULL,
       target          TIMESTAMPTZ NOT NULL,
       PRIMARY KEY(id, user_id)
);

CREATE INDEX "reminder_target_idx" ON reminders (target ASC);
CREATE INDEX "reminder_uid_target_id_idx" ON reminders (user_id, target, id);
CREATE INDEX "reminder_uid_target_id_desc_idx" ON reminders (user_id, target desc, id desc);

-- migrate:down

DROP TABLE reminders;
DROP FUNCTION random_text;
