-- migrate:up
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";


CREATE TABLE reminders (
       id              uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
       user_id         BIGINT NOT NULL,
       channel_id      BIGINT NOT NULL,
       message         TEXT NOT NULL,
       created         TIMESTAMPTZ NOT NULL,
       target          TIMESTAMPTZ NOT NULL
);

CREATE INDEX "reminder_target_idx" ON reminders (target ASC);

-- migrate:down

DROP TABLE reminders;
