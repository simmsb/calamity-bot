SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: random_text(integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.random_text(integer) RETURNS text
    LANGUAGE sql
    AS $_$
SELECT array_to_string(array(
  SELECT SUBSTRING('23456789abcdefghjkmnpqrstuvwxyz'
    FROM floor(random()*31)::int+1 FOR 1)
  FROM generate_series(1, $1)), '');
$_$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: aliases; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.aliases (
    user_id bigint NOT NULL,
    name text NOT NULL,
    value text NOT NULL
);


--
-- Name: guilds; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.guilds (
    id bigint NOT NULL,
    last_seen timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: prefixes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.prefixes (
    guild__id bigint NOT NULL,
    pre text NOT NULL
);


--
-- Name: reminders; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.reminders (
    id text DEFAULT public.random_text(16) NOT NULL,
    user_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    message text NOT NULL,
    created timestamp with time zone NOT NULL,
    target timestamp with time zone NOT NULL
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: aliases aliases_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.aliases
    ADD CONSTRAINT aliases_pkey PRIMARY KEY (user_id, name);


--
-- Name: guilds guilds_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.guilds
    ADD CONSTRAINT guilds_pkey PRIMARY KEY (id);


--
-- Name: prefixes prefixes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.prefixes
    ADD CONSTRAINT prefixes_pkey PRIMARY KEY (guild__id, pre);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: reminder_target_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reminder_target_idx ON public.reminders USING btree (target);


--
-- Name: reminder_uid_id_pkey; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reminder_uid_id_pkey ON public.reminders USING btree (id, user_id);


--
-- Name: prefixes prefixes_guild__id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.prefixes
    ADD CONSTRAINT prefixes_guild__id_fkey FOREIGN KEY (guild__id) REFERENCES public.guilds(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20200602214607'),
    ('20200602214609'),
    ('20200612224716'),
    ('20200625014534');
