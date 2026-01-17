--
-- PostgreSQL database dump
--

\restrict UnvDtAvLF7C8akbkxsldf988qfzmAX4mR8wticpkiC1SY578tLKVSfBZmK8SnPa

-- Dumped from database version 17.6
-- Dumped by pg_dump version 17.7 (Ubuntu 17.7-3.pgdg24.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: public; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA public;


--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA public IS 'standard public schema';


--
-- Name: reason_side_enum; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.reason_side_enum AS ENUM (
    'support',
    'refute'
);


--
-- Name: TYPE reason_side_enum; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TYPE public.reason_side_enum IS 'Whether the reason claim is for (support) or against (refute) the target claim.';


--
-- Name: vote_type_enum; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.vote_type_enum AS ENUM (
    'certainty',
    'relevance',
    'like',
    'dislike'
);


--
-- Name: audit_trigger_func(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.audit_trigger_func() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $_$
DECLARE
  col_name text;
  old_val text;
  new_val text;
  rec_id text;
  user_id uuid;
BEGIN
  -- Get the user ID (from auth.uid() or use the created_by/updated_by field)
  user_id := COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid);
  
  -- Convert record ID to text
  IF TG_OP = 'DELETE' THEN
    rec_id := OLD.id::text;
  ELSE
    rec_id := NEW.id::text;
  END IF;

  -- Handle INSERT
  IF TG_OP = 'INSERT' THEN
    -- Loop through all columns in NEW
    FOR col_name IN 
      SELECT column_name 
      FROM information_schema.columns 
      WHERE table_schema = TG_TABLE_SCHEMA 
        AND table_name = TG_TABLE_NAME
        AND column_name NOT IN ('id') -- Skip ID field
    LOOP
      EXECUTE format('SELECT ($1).%I::text', col_name) INTO new_val USING NEW;
      
      -- Only log if new value is not null
      IF new_val IS NOT NULL THEN
        INSERT INTO history (changed_by, changed_at, table_name, field_name, record_id, old_value, new_value, operation)
        VALUES (user_id, now(), TG_TABLE_NAME, col_name, rec_id, NULL, new_val, 'INSERT');
      END IF;
    END LOOP;
    
    RETURN NEW;
  END IF;

  -- Handle UPDATE
  IF TG_OP = 'UPDATE' THEN
    -- Loop through all columns
    FOR col_name IN 
      SELECT column_name 
      FROM information_schema.columns 
      WHERE table_schema = TG_TABLE_SCHEMA 
        AND table_name = TG_TABLE_NAME
        AND column_name NOT IN ('id', 'updated_at') -- Skip ID and auto-updated fields
    LOOP
      EXECUTE format('SELECT ($1).%I::text', col_name) INTO old_val USING OLD;
      EXECUTE format('SELECT ($1).%I::text', col_name) INTO new_val USING NEW;
      
      -- Only log if values actually changed
      IF old_val IS DISTINCT FROM new_val THEN
        INSERT INTO history (changed_by, changed_at, table_name, field_name, record_id, old_value, new_value, operation)
        VALUES (user_id, now(), TG_TABLE_NAME, col_name, rec_id, old_val, new_val, 'UPDATE');
      END IF;
    END LOOP;
    
    RETURN NEW;
  END IF;

  -- Handle DELETE
  IF TG_OP = 'DELETE' THEN
    -- Loop through all columns in OLD
    FOR col_name IN 
      SELECT column_name 
      FROM information_schema.columns 
      WHERE table_schema = TG_TABLE_SCHEMA 
        AND table_name = TG_TABLE_NAME
        AND column_name NOT IN ('id')
    LOOP
      EXECUTE format('SELECT ($1).%I::text', col_name) INTO old_val USING OLD;
      
      -- Only log if old value is not null
      IF old_val IS NOT NULL THEN
        INSERT INTO history (changed_by, changed_at, table_name, field_name, record_id, old_value, new_value, operation)
        VALUES (user_id, now(), TG_TABLE_NAME, col_name, rec_id, old_val, NULL, 'DELETE');
      END IF;
    END LOOP;
    
    RETURN OLD;
  END IF;

  RETURN NULL;
END;
$_$;


--
-- Name: FUNCTION audit_trigger_func(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.audit_trigger_func() IS 'Generic trigger function to capture all table changes to history table';


--
-- Name: claim_sets_search_vector_update(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.claim_sets_search_vector_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  new.search_vector := 
    setweight(to_tsvector('english', coalesce(new.name, '')), 'A') ||
    setweight(to_tsvector('english', coalesce(new.description, '')), 'B');
  return new;
end;
$$;


--
-- Name: claims_search_vector_update(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.claims_search_vector_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$begin
  new.search_vector := 
    setweight(to_tsvector('english', coalesce(new.claim_code, '')), 'A') ||
    setweight(to_tsvector('english', coalesce(new.name, '')), 'A') ||
    setweight(to_tsvector('english', coalesce(new.statement, '')), 'C') ||
    setweight(to_tsvector('english', coalesce(new.statement_tags, '')), 'D');
  return new;
end;$$;


--
-- Name: cleanup_expired_verification_tokens(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.cleanup_expired_verification_tokens() RETURNS integer
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
DECLARE
  rows_updated INTEGER;
BEGIN
  UPDATE public.users
  SET 
    email_verification_token = NULL,
    email_verification_token_expires_at = NULL
  WHERE 
    email_verification_token IS NOT NULL 
    AND email_verification_token_expires_at < NOW();
  
  GET DIAGNOSTICS rows_updated = ROW_COUNT;
  
  RETURN rows_updated;
END;
$$;


--
-- Name: FUNCTION cleanup_expired_verification_tokens(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.cleanup_expired_verification_tokens() IS 'Cleans up expired email verification tokens. Can be run manually or via pg_cron.';


--
-- Name: generate_unique_claim_code(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.generate_unique_claim_code() RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
  chars TEXT := 'ABCDEFGHJKLMNPQRSTUVWXYZ23456789'; -- 32 characters (no I, O, 1, 0)
  result TEXT;
  collision BOOLEAN;
BEGIN
  LOOP
    -- Generate a random 4-character code
    result := '';
    FOR i IN 1..4 LOOP
      result := result || substr(chars, floor(random() * 32 + 1)::int, 1);
    END LOOP;
    
    -- Check if the code already exists
    SELECT EXISTS(SELECT 1 FROM claims WHERE claim_code = result) INTO collision;
    
    -- If no collision, exit the loop
    EXIT WHEN NOT collision;
  END LOOP;
  
  RETURN result;
END;
$$;


--
-- Name: handle_new_user(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.handle_new_user() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  insert into public.users (id, email, full_name, username, alias, country_iso_code)
  values (
    new.id,
    new.email,
    coalesce(new.raw_user_meta_data->>'full_name', new.email),
    lower(regexp_replace(split_part(new.email, '@', 1), '[^a-zA-Z0-9]', '', 'g')),
    coalesce(new.raw_user_meta_data->>'alias', ''),
    coalesce(new.raw_user_meta_data->>'country_iso_code', '')
  )
  on conflict (id) do nothing;
  
  return new;
exception
  when unique_violation then
    -- Handle username collision
    insert into public.users (id, email, full_name, username, alias, country_iso_code)
    values (
      new.id,
      new.email,
      coalesce(new.raw_user_meta_data->>'full_name', new.email),
      lower(regexp_replace(split_part(new.email, '@', 1), '[^a-zA-Z0-9]', '', 'g')) || '_' || substr(md5(random()::text), 1, 4),
      coalesce(new.raw_user_meta_data->>'alias', ''),
      coalesce(new.raw_user_meta_data->>'country_iso_code', '')
    )
    on conflict (id) do nothing;
    return new;
end;
$$;


--
-- Name: handle_update_defaults(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.handle_update_defaults() RETURNS trigger
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
begin
  new.updated_at = now();
  -- Use special system UUID when auth.uid() is null (Supabase Portal updates)
  -- '71853be9-7882-4c0f-9c6c-08a3445b67cf' = System/Portal updates
  new.updated_by = coalesce(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid);
  return new;
end;
$$;


--
-- Name: numeric_mul_nullsafe(numeric, numeric); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.numeric_mul_nullsafe(a numeric, b numeric) RETURNS numeric
    LANGUAGE sql IMMUTABLE
    AS $$
    SELECT COALESCE(a, 1) * COALESCE(b, 1);
$$;


--
-- Name: purge_old_history(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.purge_old_history() RETURNS void
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
BEGIN
  DELETE FROM history 
  WHERE changed_at < NOW() - INTERVAL '6 months';
  
  RAISE NOTICE 'Purged history records older than 6 months';
END;
$$;


--
-- Name: FUNCTION purge_old_history(); Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON FUNCTION public.purge_old_history() IS 'Purges audit history records older than 6 months. To be run on a schedule.';


--
-- Name: search_query_to_tsquery(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.search_query_to_tsquery(search_text text) RETURNS tsquery
    LANGUAGE plpgsql IMMUTABLE
    AS $$
declare
  words text[];
  tsquery_string text;
begin
  -- Return empty tsquery if input is empty
  if search_text is null or trim(search_text) = '' then
    return to_tsquery('english', '');
  end if;
  
  -- Split into words, filter out empty strings
  words := array(
    select word 
    from regexp_split_to_table(lower(trim(search_text)), '\s+') as word
    where word != ''
  );
  
  -- If no valid words, return empty
  if array_length(words, 1) is null then
    return to_tsquery('english', '');
  end if;
  
  -- Join words with AND operator
  tsquery_string := array_to_string(words, ' & ');
  
  -- Return tsquery
  return to_tsquery('english', tsquery_string);
exception
  when others then
    -- If tsquery parsing fails, return empty query
    return to_tsquery('english', '');
end;
$$;


--
-- Name: product(numeric); Type: AGGREGATE; Schema: public; Owner: -
--

CREATE AGGREGATE public.product(numeric) (
    SFUNC = public.numeric_mul_nullsafe,
    STYPE = numeric,
    INITCOND = '1'
);


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: claims; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.claims (
    id integer NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    statement text DEFAULT ''::text NOT NULL,
    statement_tags text DEFAULT ''::text NOT NULL,
    set_id integer,
    is_active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    moderated_at timestamp with time zone,
    moderation_reason text,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    moderated_by uuid,
    claim_code text DEFAULT public.generate_unique_claim_code(),
    search_vector tsvector
);


--
-- Name: levels; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.levels (
    id integer NOT NULL,
    value numeric(9,6) DEFAULT 0 NOT NULL,
    certainty_name text DEFAULT ''::text NOT NULL,
    certainty_description text DEFAULT ''::text,
    relevance_name text DEFAULT ''::text NOT NULL,
    relevance_description text DEFAULT ''::text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    level_percentage text,
    or_logic_multiplier numeric(9,6),
    and_logic_multiplier numeric(9,6)
);


--
-- Name: COLUMN levels.or_logic_multiplier; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.levels.or_logic_multiplier IS 'The number you multiply across multiple OR terms of claim certainty / reason relevance';


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id uuid NOT NULL,
    username text NOT NULL,
    email text NOT NULL,
    full_name text NOT NULL,
    country_iso_code text DEFAULT ''::text NOT NULL,
    alias text DEFAULT ''::text NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    default_domain_id integer,
    voting_weight numeric(3,1) DEFAULT 1.0 NOT NULL,
    bible_confidence_level integer,
    is_active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    email_verification_token character varying(255),
    email_verification_token_expires_at timestamp with time zone,
    password_reset_token character varying(255),
    password_reset_token_expires_at timestamp with time zone
);


--
-- Name: COLUMN users.email_verification_token; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.email_verification_token IS 'Temporary token used for email verification. Should be cleared after verification.';


--
-- Name: COLUMN users.email_verification_token_expires_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.users.email_verification_token_expires_at IS 'Expiration timestamp for the verification token. Tokens expire after 24 hours.';


--
-- Name: votes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.votes (
    id integer NOT NULL,
    claim_id integer,
    post_id integer,
    reason_id integer,
    vote_type public.vote_type_enum NOT NULL,
    vote_level_id integer,
    is_active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    CONSTRAINT votes_level_for_certainty_relevance_check CHECK ((((vote_type = ANY (ARRAY['certainty'::public.vote_type_enum, 'relevance'::public.vote_type_enum])) AND (vote_level_id IS NOT NULL)) OR ((vote_type = ANY (ARRAY['like'::public.vote_type_enum, 'dislike'::public.vote_type_enum])) AND (vote_level_id IS NULL)))),
    CONSTRAINT votes_single_target_check CHECK ((((claim_id IS NOT NULL) AND (post_id IS NULL) AND (reason_id IS NULL)) OR ((claim_id IS NULL) AND (post_id IS NOT NULL) AND (reason_id IS NULL)) OR ((claim_id IS NULL) AND (post_id IS NULL) AND (reason_id IS NOT NULL))))
);


--
-- Name: claim_certainty; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.claim_certainty AS
 SELECT c.id AS claim_id,
    cv.total_assessments AS certainty_vote_count,
    cv.weighted_average_level_id AS certainty_average_level_id,
    levels.value AS certainty_level_probability,
    levels.certainty_name AS certainty_level_name,
    levels.certainty_description AS certainty_level_description,
    levels.level_percentage AS certainty_level_percentage,
    levels.value AS certainty_level_multiplier
   FROM ((public.claims c
     LEFT JOIN ( SELECT claims.id AS claim_id,
            count(votes.id) AS total_assessments,
            COALESCE((round((sum(((votes.vote_level_id)::numeric * COALESCE(users.voting_weight, 1.00))) / sum(COALESCE(users.voting_weight, 1.00)))))::integer, 0) AS weighted_average_level_id,
            count(votes.id) AS vote_count
           FROM ((public.claims
             LEFT JOIN public.votes ON (((votes.claim_id = claims.id) AND (votes.vote_type = 'certainty'::public.vote_type_enum) AND (votes.is_active = true))))
             LEFT JOIN public.users ON ((users.id = votes.created_by)))
          WHERE (claims.is_active = true)
          GROUP BY claims.id) cv ON ((cv.claim_id = c.id)))
     LEFT JOIN public.levels ON ((levels.id = cv.weighted_average_level_id)))
  WHERE (c.is_active = true);


--
-- Name: reasons; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.reasons (
    id integer NOT NULL,
    target_claim_id integer NOT NULL,
    reason_claim_id integer NOT NULL,
    is_active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    moderated_at timestamp with time zone,
    moderation_reason text,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    moderated_by uuid,
    reason_side public.reason_side_enum DEFAULT 'support'::public.reason_side_enum NOT NULL,
    CONSTRAINT reasons_no_self_reference_check CHECK ((target_claim_id <> reason_claim_id))
);


--
-- Name: COLUMN reasons.reason_side; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.reasons.reason_side IS 'Whether the reason claim is for (support) or against (refute) the target claim (position).';


--
-- Name: reason_relevance; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.reason_relevance AS
 SELECT r.id AS reason_id,
    r.target_claim_id,
    r.reason_claim_id,
    r.reason_side,
    vote_counts.total_assessments AS relevance_vote_count,
    vote_counts.weighted_average_level_id AS relevance_average_level_id,
    levels.value AS relevance_level_probability,
    levels.relevance_name AS relevance_level_name,
    levels.relevance_description AS relevance_level_description,
    levels.level_percentage AS relevance_level_percentage,
    levels.or_logic_multiplier AS relevance_level_multiplier
   FROM ((public.reasons r
     LEFT JOIN ( SELECT reasons.id AS reason_id,
            count(votes.id) AS total_assessments,
            COALESCE((round((sum(((votes.vote_level_id)::numeric * COALESCE(users.voting_weight, 1.00))) / sum(COALESCE(users.voting_weight, 1.00)))))::integer, 0) AS weighted_average_level_id
           FROM ((public.reasons
             LEFT JOIN public.votes ON (((votes.reason_id = reasons.id) AND (votes.vote_type = 'relevance'::public.vote_type_enum) AND (votes.is_active = true))))
             LEFT JOIN public.users ON ((votes.created_by = users.id)))
          WHERE (reasons.is_active = true)
          GROUP BY reasons.id) vote_counts ON ((vote_counts.reason_id = r.id)))
     LEFT JOIN public.levels ON ((levels.id = vote_counts.weighted_average_level_id)))
  WHERE (r.is_active = true);


--
-- Name: claim_reason_certainty; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.claim_reason_certainty AS
 SELECT c.id AS claim_id,
    rrs.certainty_percentage AS certainty_percentage_support,
    rrr.certainty_percentage AS certainty_percentage_refute,
    (rrs.certainty_percentage * (1.0 - rrr.certainty_percentage)) AS reasoned_certainty,
    ccp.competing_claims_falsehood,
    (1.0 - ((1.0 - (rrs.certainty_percentage * (1.0 - rrr.certainty_percentage))) * ((1)::numeric - ((1)::numeric - ((1)::numeric - ccp.competing_claims_falsehood))))) AS combined_mutually_exclusive_certainty
   FROM (((public.claims c
     LEFT JOIN ( SELECT rr.target_claim_id AS claim_id,
            ((1)::numeric - COALESCE(public.product(((1)::numeric - (rr.relevance_level_multiplier * cc.certainty_level_multiplier))), (1)::numeric)) AS certainty_percentage
           FROM (public.reason_relevance rr
             LEFT JOIN public.claim_certainty cc ON ((cc.claim_id = rr.reason_claim_id)))
          WHERE (rr.reason_side = 'support'::public.reason_side_enum)
          GROUP BY rr.target_claim_id
         HAVING (sum(rr.relevance_level_multiplier) > (0)::numeric)) rrs ON ((rrs.claim_id = c.id)))
     LEFT JOIN ( SELECT rr.target_claim_id AS claim_id,
            ((1)::numeric - COALESCE(public.product(((1)::numeric - (rr.relevance_level_multiplier * cc.certainty_level_multiplier))), (1)::numeric)) AS certainty_percentage
           FROM (public.reason_relevance rr
             LEFT JOIN public.claim_certainty cc ON ((cc.claim_id = rr.reason_claim_id)))
          WHERE (rr.reason_side = 'refute'::public.reason_side_enum)
          GROUP BY rr.target_claim_id
         HAVING (sum(rr.relevance_level_multiplier) > (0)::numeric)) rrr ON ((rrr.claim_id = c.id)))
     LEFT JOIN ( SELECT c1.id AS claim_id,
            public.product((1.0 - (r2s.certainty_percentage * (1.0 - r2r.certainty_percentage)))) AS competing_claims_falsehood
           FROM (((public.claims c1
             JOIN public.claims c2 ON (((c2.set_id = c1.set_id) AND (c2.id <> c1.id) AND (c2.is_active = true))))
             LEFT JOIN ( SELECT rr.target_claim_id AS claim_id,
                    ((1)::numeric - COALESCE(public.product(((1)::numeric - (rr.relevance_level_multiplier * cc.certainty_level_multiplier))), (1)::numeric)) AS certainty_percentage
                   FROM (public.reason_relevance rr
                     LEFT JOIN public.claim_certainty cc ON ((cc.claim_id = rr.reason_claim_id)))
                  WHERE (rr.reason_side = 'support'::public.reason_side_enum)
                  GROUP BY rr.target_claim_id
                 HAVING (sum(rr.relevance_level_multiplier) > (0)::numeric)) r2s ON ((r2s.claim_id = c2.id)))
             LEFT JOIN ( SELECT rr.target_claim_id AS claim_id,
                    ((1)::numeric - COALESCE(public.product(((1)::numeric - (rr.relevance_level_multiplier * cc.certainty_level_multiplier))), (1)::numeric)) AS certainty_percentage
                   FROM (public.reason_relevance rr
                     LEFT JOIN public.claim_certainty cc ON ((cc.claim_id = rr.reason_claim_id)))
                  WHERE (rr.reason_side = 'refute'::public.reason_side_enum)
                  GROUP BY rr.target_claim_id
                 HAVING (sum(rr.relevance_level_multiplier) > (0)::numeric)) r2r ON ((r2r.claim_id = c2.id)))
          GROUP BY c1.id) ccp ON ((ccp.claim_id = c.id)));


--
-- Name: claim_sets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.claim_sets (
    id integer NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    is_mutually_exclusive boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    topic_id integer NOT NULL,
    domain_id integer NOT NULL,
    is_active boolean DEFAULT true NOT NULL,
    search_vector tsvector,
    moderated_at timestamp with time zone,
    moderated_by uuid,
    moderation_reason text
);


--
-- Name: COLUMN claim_sets.moderated_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.claim_sets.moderated_at IS 'Timestamp of last moderation action';


--
-- Name: COLUMN claim_sets.moderated_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.claim_sets.moderated_by IS 'User ID of moderator who last modified this record';


--
-- Name: COLUMN claim_sets.moderation_reason; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.claim_sets.moderation_reason IS 'Reason provided by moderator for the change';


--
-- Name: claim_sets_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.claim_sets ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.claim_sets_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: claims_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.claims ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.claims_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: domains; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.domains (
    id integer NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    is_default boolean DEFAULT false NOT NULL,
    is_active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL
);


--
-- Name: domains_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.domains ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.domains_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: history; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.history (
    id bigint NOT NULL,
    changed_by uuid,
    changed_at timestamp with time zone DEFAULT now() NOT NULL,
    table_name text NOT NULL,
    field_name text NOT NULL,
    record_id text NOT NULL,
    old_value text,
    new_value text,
    operation text NOT NULL,
    CONSTRAINT history_operation_check CHECK ((operation = ANY (ARRAY['INSERT'::text, 'UPDATE'::text, 'DELETE'::text])))
);


--
-- Name: TABLE history; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.history IS 'Comprehensive audit trail for all database changes';


--
-- Name: COLUMN history.changed_by; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.changed_by IS 'User who made the change (NULL for system changes)';


--
-- Name: COLUMN history.changed_at; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.changed_at IS 'Timestamp of the change';


--
-- Name: COLUMN history.table_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.table_name IS 'Name of the table that was modified';


--
-- Name: COLUMN history.field_name; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.field_name IS 'Name of the field that was changed';


--
-- Name: COLUMN history.record_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.record_id IS 'ID of the record (converted to text)';


--
-- Name: COLUMN history.old_value; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.old_value IS 'Previous value (NULL for INSERT operations)';


--
-- Name: COLUMN history.new_value; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.new_value IS 'New value (NULL for DELETE operations)';


--
-- Name: COLUMN history.operation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.history.operation IS 'Type of operation: INSERT, UPDATE, or DELETE';


--
-- Name: history_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.history ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: kv_store_a42d30f9; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.kv_store_a42d30f9 (
    key text NOT NULL,
    value jsonb NOT NULL
);


--
-- Name: posts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posts (
    id integer NOT NULL,
    claim_id integer,
    reason_id integer,
    comment text DEFAULT ''::text NOT NULL,
    is_active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    moderated_at timestamp with time zone,
    moderation_reason text,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    moderated_by uuid,
    post_type text DEFAULT 'armor'::text NOT NULL,
    tag_data jsonb DEFAULT '{"tags": []}'::jsonb,
    CONSTRAINT posts_claim_or_reason_check CHECK ((((claim_id IS NOT NULL) AND (reason_id IS NULL)) OR ((claim_id IS NULL) AND (reason_id IS NOT NULL)))),
    CONSTRAINT posts_post_type_check CHECK ((post_type = ANY (ARRAY['armor'::text, 'ammo'::text]))),
    CONSTRAINT posts_tag_data_tags_array CHECK ((((tag_data -> 'tags'::text) IS NULL) OR (jsonb_typeof((tag_data -> 'tags'::text)) = 'array'::text))),
    CONSTRAINT posts_tag_data_valid CHECK (((jsonb_typeof(tag_data) = 'object'::text) OR (tag_data IS NULL)))
);


--
-- Name: COLUMN posts.post_type; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.posts.post_type IS 'Type of evidence: armor (supporting) or ammo (counter/rebuttal)';


--
-- Name: COLUMN posts.tag_data; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.posts.tag_data IS 'Intel tag metadata storage in format: {tags: [{id, type, created_at, data}]}.
   Tag types: "citation" (books/articles), "url" (web links), "bible" (scripture references), "definition" (word definitions).
   Supports ⟦n⟧ placeholders in comment field for inline tag references.
   Example: {
     "tags": [
       {
         "id": 1,
         "type": "bible",
         "created_at": "2026-01-10T12:00:00Z",
         "data": {"book": "John", "chapter": 3, "verse_start": 16, "normalized": "John 3:16"}
       },
       {
         "id": 2,
         "type": "citation",
         "created_at": "2026-01-10T12:01:00Z",
         "data": {"source_type": "book", "author": "Lewis, C. S.", "title": "Mere Christianity", "year": 2001}
       }
     ]
   }';


--
-- Name: posts_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.posts ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.posts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: reasons_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.reasons ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.reasons_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: topics; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.topics (
    id integer NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    path text DEFAULT ''::text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    parent_topic_id integer,
    level integer DEFAULT 0,
    sort_order integer DEFAULT 0 NOT NULL,
    is_active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL
);


--
-- Name: topics_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.topics ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.topics_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: user_topics; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_topics (
    id integer NOT NULL,
    user_id uuid NOT NULL,
    domain_id integer,
    topic_id integer,
    is_following boolean DEFAULT false NOT NULL,
    is_moderating boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    updated_by uuid DEFAULT COALESCE(auth.uid(), '71853be9-7882-4c0f-9c6c-08a3445b67cf'::uuid) NOT NULL,
    CONSTRAINT user_topics_moderating_implies_following_check CHECK (((NOT is_moderating) OR is_following))
);


--
-- Name: user_topics_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.user_topics ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.user_topics_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: votes_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

ALTER TABLE public.votes ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.votes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: claim_sets claim_sets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claim_sets
    ADD CONSTRAINT claim_sets_pkey PRIMARY KEY (id);


--
-- Name: claims claims_claim_code_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claims
    ADD CONSTRAINT claims_claim_code_key UNIQUE (claim_code);


--
-- Name: claims claims_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claims
    ADD CONSTRAINT claims_pkey PRIMARY KEY (id);


--
-- Name: domains domains_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.domains
    ADD CONSTRAINT domains_pkey PRIMARY KEY (id);


--
-- Name: history history_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.history
    ADD CONSTRAINT history_pkey PRIMARY KEY (id);


--
-- Name: kv_store_a42d30f9 kv_store_a42d30f9_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.kv_store_a42d30f9
    ADD CONSTRAINT kv_store_a42d30f9_pkey PRIMARY KEY (key);


--
-- Name: levels levels_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.levels
    ADD CONSTRAINT levels_pkey PRIMARY KEY (id);


--
-- Name: posts posts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);


--
-- Name: reasons reasons_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reasons
    ADD CONSTRAINT reasons_pkey PRIMARY KEY (id);


--
-- Name: topics topics_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.topics
    ADD CONSTRAINT topics_pkey PRIMARY KEY (id);


--
-- Name: user_topics user_topics_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_topics
    ADD CONSTRAINT user_topics_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: votes votes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.votes
    ADD CONSTRAINT votes_pkey PRIMARY KEY (id);


--
-- Name: claim_sets_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_created_by_idx ON public.claim_sets USING btree (created_by);


--
-- Name: claim_sets_domain_active_created_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_domain_active_created_idx ON public.claim_sets USING btree (domain_id, is_active, created_at DESC) WHERE (is_active = true);


--
-- Name: claim_sets_domain_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_domain_id_idx ON public.claim_sets USING btree (domain_id);


--
-- Name: claim_sets_domain_topic_active_created_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_domain_topic_active_created_idx ON public.claim_sets USING btree (domain_id, topic_id, is_active, created_at DESC) WHERE (is_active = true);


--
-- Name: claim_sets_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_is_active_idx ON public.claim_sets USING btree (is_active);


--
-- Name: claim_sets_moderated_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_moderated_at_idx ON public.claim_sets USING btree (moderated_at);


--
-- Name: claim_sets_moderated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_moderated_by_idx ON public.claim_sets USING btree (moderated_by);


--
-- Name: claim_sets_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_name_idx ON public.claim_sets USING btree (name);


--
-- Name: claim_sets_name_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX claim_sets_name_key ON public.claim_sets USING btree (lower(name));


--
-- Name: claim_sets_search_vector_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_search_vector_idx ON public.claim_sets USING gin (search_vector);


--
-- Name: claim_sets_topic_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_topic_active_idx ON public.claim_sets USING btree (topic_id) WHERE (topic_id IS NOT NULL);


--
-- Name: claim_sets_topic_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_topic_id_idx ON public.claim_sets USING btree (topic_id);


--
-- Name: claim_sets_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claim_sets_updated_by_idx ON public.claim_sets USING btree (updated_by);


--
-- Name: claims_claim_code_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_claim_code_idx ON public.claims USING btree (claim_code);


--
-- Name: claims_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_created_by_idx ON public.claims USING btree (created_by);


--
-- Name: claims_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_is_active_idx ON public.claims USING btree (is_active);


--
-- Name: claims_moderated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_moderated_by_idx ON public.claims USING btree (moderated_by);


--
-- Name: claims_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_name_idx ON public.claims USING btree (name);


--
-- Name: claims_name_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX claims_name_key ON public.claims USING btree (lower(name));


--
-- Name: claims_search_vector_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_search_vector_idx ON public.claims USING gin (search_vector);


--
-- Name: claims_set_active_created_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_set_active_created_idx ON public.claims USING btree (set_id, is_active, created_at DESC) WHERE (is_active = true);


--
-- Name: claims_statement_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_statement_idx ON public.claims USING btree (statement);


--
-- Name: claims_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX claims_updated_by_idx ON public.claims USING btree (updated_by);


--
-- Name: domains_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX domains_created_by_idx ON public.domains USING btree (created_by);


--
-- Name: domains_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX domains_is_active_idx ON public.domains USING btree (is_active);


--
-- Name: domains_name_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX domains_name_idx ON public.domains USING btree (name);


--
-- Name: domains_name_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX domains_name_key ON public.domains USING btree (lower(name));


--
-- Name: domains_single_default_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX domains_single_default_idx ON public.domains USING btree (is_default) WHERE (is_default = true);


--
-- Name: domains_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX domains_updated_by_idx ON public.domains USING btree (updated_by);


--
-- Name: history_changed_at_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX history_changed_at_idx ON public.history USING btree (changed_at);


--
-- Name: history_changed_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX history_changed_by_idx ON public.history USING btree (changed_by);


--
-- Name: history_operation_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX history_operation_idx ON public.history USING btree (operation);


--
-- Name: history_table_record_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX history_table_record_idx ON public.history USING btree (table_name, record_id);


--
-- Name: idx_posts_tag_data; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_posts_tag_data ON public.posts USING gin (tag_data);


--
-- Name: idx_posts_with_tags; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_posts_with_tags ON public.posts USING btree (((tag_data -> 'tags'::text))) WHERE (jsonb_array_length((tag_data -> 'tags'::text)) > 0);


--
-- Name: idx_users_email_verification_token; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_users_email_verification_token ON public.users USING btree (email_verification_token) WHERE (email_verification_token IS NOT NULL);


--
-- Name: idx_users_password_reset_token; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_users_password_reset_token ON public.users USING btree (password_reset_token) WHERE (password_reset_token IS NOT NULL);


--
-- Name: kv_store_a42d30f9_key_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx1; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx1 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx10; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx10 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx11; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx11 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx12; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx12 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx13; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx13 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx14; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx14 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx15; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx15 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx16; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx16 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx17; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx17 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx18; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx18 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx19; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx19 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx2; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx2 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx20; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx20 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx21; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx21 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx22; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx22 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx23; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx23 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx24; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx24 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx25; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx25 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx26; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx26 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx27; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx27 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx28; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx28 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx29; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx29 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx3; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx3 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx30; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx30 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx31; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx31 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx32; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx32 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx33; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx33 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx34; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx34 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx35; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx35 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx36; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx36 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx37; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx37 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx38; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx38 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx39; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx39 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx4; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx4 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx40; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx40 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx41; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx41 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx42; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx42 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx43; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx43 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx44; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx44 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx45; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx45 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx46; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx46 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx47; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx47 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx48; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx48 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx49; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx49 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx5; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx5 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx50; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx50 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx51; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx51 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx52; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx52 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx53; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx53 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx54; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx54 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx55; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx55 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx56; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx56 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx57; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx57 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx58; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx58 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx59; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx59 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx6; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx6 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx60; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx60 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx61; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx61 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx62; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx62 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx63; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx63 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx64; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx64 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx65; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx65 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx66; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx66 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx67; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx67 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx68; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx68 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx69; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx69 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx7; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx7 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx70; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx70 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx71; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx71 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx72; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx72 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx73; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx73 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx74; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx74 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx75; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx75 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx8; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx8 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: kv_store_a42d30f9_key_idx9; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX kv_store_a42d30f9_key_idx9 ON public.kv_store_a42d30f9 USING btree (key text_pattern_ops);


--
-- Name: levels_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX levels_created_by_idx ON public.levels USING btree (created_by);


--
-- Name: levels_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX levels_updated_by_idx ON public.levels USING btree (updated_by);


--
-- Name: posts_claim_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_claim_active_idx ON public.posts USING btree (claim_id, is_active, created_at DESC) WHERE ((is_active = true) AND (claim_id IS NOT NULL));


--
-- Name: posts_claim_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_claim_id_idx ON public.posts USING btree (claim_id);


--
-- Name: posts_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_created_by_idx ON public.posts USING btree (created_by);


--
-- Name: posts_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_is_active_idx ON public.posts USING btree (is_active);


--
-- Name: posts_moderated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_moderated_by_idx ON public.posts USING btree (moderated_by);


--
-- Name: posts_reason_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_reason_active_idx ON public.posts USING btree (reason_id, is_active, created_at DESC) WHERE ((is_active = true) AND (reason_id IS NOT NULL));


--
-- Name: posts_reason_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_reason_id_idx ON public.posts USING btree (reason_id);


--
-- Name: posts_reason_type_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_reason_type_active_idx ON public.posts USING btree (reason_id, post_type, is_active) WHERE ((reason_id IS NOT NULL) AND (is_active = true));


--
-- Name: posts_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX posts_updated_by_idx ON public.posts USING btree (updated_by);


--
-- Name: reasons_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_created_by_idx ON public.reasons USING btree (created_by);


--
-- Name: reasons_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_is_active_idx ON public.reasons USING btree (is_active);


--
-- Name: reasons_moderated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_moderated_by_idx ON public.reasons USING btree (moderated_by);


--
-- Name: reasons_reason_active_created_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_reason_active_created_idx ON public.reasons USING btree (reason_claim_id, is_active, created_at DESC) WHERE (is_active = true);


--
-- Name: reasons_reason_claim_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_reason_claim_id_idx ON public.reasons USING btree (reason_claim_id);


--
-- Name: reasons_target_active_created_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_target_active_created_idx ON public.reasons USING btree (target_claim_id, is_active, created_at DESC) WHERE (is_active = true);


--
-- Name: reasons_target_claim_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_target_claim_id_idx ON public.reasons USING btree (target_claim_id);


--
-- Name: reasons_target_reason_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX reasons_target_reason_key ON public.reasons USING btree (target_claim_id, reason_claim_id);


--
-- Name: reasons_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reasons_updated_by_idx ON public.reasons USING btree (updated_by);


--
-- Name: topics_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX topics_created_by_idx ON public.topics USING btree (created_by);


--
-- Name: topics_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX topics_is_active_idx ON public.topics USING btree (is_active);


--
-- Name: topics_name_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX topics_name_key ON public.topics USING btree (lower(name));


--
-- Name: topics_parent_level_sort_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX topics_parent_level_sort_idx ON public.topics USING btree (parent_topic_id, level, sort_order) WHERE (is_active = true);


--
-- Name: topics_parent_topic_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX topics_parent_topic_id_idx ON public.topics USING btree (parent_topic_id);


--
-- Name: topics_sort_order_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX topics_sort_order_idx ON public.topics USING btree (sort_order);


--
-- Name: topics_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX topics_updated_by_idx ON public.topics USING btree (updated_by);


--
-- Name: user_topics_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_created_by_idx ON public.user_topics USING btree (created_by);


--
-- Name: user_topics_domain_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_domain_id_idx ON public.user_topics USING btree (domain_id);


--
-- Name: user_topics_is_following_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_is_following_idx ON public.user_topics USING btree (is_following);


--
-- Name: user_topics_is_moderating_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_is_moderating_idx ON public.user_topics USING btree (is_moderating);


--
-- Name: user_topics_topic_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_topic_id_idx ON public.user_topics USING btree (topic_id);


--
-- Name: user_topics_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_updated_by_idx ON public.user_topics USING btree (updated_by);


--
-- Name: user_topics_user_domain_topic_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX user_topics_user_domain_topic_key ON public.user_topics USING btree (user_id, domain_id, topic_id);


--
-- Name: user_topics_user_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_user_id_idx ON public.user_topics USING btree (user_id);


--
-- Name: user_topics_user_moderating_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_topics_user_moderating_idx ON public.user_topics USING btree (user_id, is_moderating) WHERE (is_moderating = true);


--
-- Name: users_alias_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_alias_key ON public.users USING btree (lower(alias));


--
-- Name: users_bible_confidence_level_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX users_bible_confidence_level_idx ON public.users USING btree (bible_confidence_level);


--
-- Name: users_default_domain_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX users_default_domain_id_idx ON public.users USING btree (default_domain_id);


--
-- Name: users_email_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_email_key ON public.users USING btree (lower(email));


--
-- Name: users_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX users_is_active_idx ON public.users USING btree (is_active);


--
-- Name: users_is_admin_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX users_is_admin_idx ON public.users USING btree (is_admin);


--
-- Name: users_username_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_username_key ON public.users USING btree (lower(username));


--
-- Name: votes_claim_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_claim_id_idx ON public.votes USING btree (claim_id);


--
-- Name: votes_claim_type_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_claim_type_active_idx ON public.votes USING btree (claim_id, vote_type, is_active) WHERE ((claim_id IS NOT NULL) AND (is_active = true));


--
-- Name: votes_created_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_created_by_idx ON public.votes USING btree (created_by);


--
-- Name: votes_is_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_is_active_idx ON public.votes USING btree (is_active);


--
-- Name: votes_post_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_post_id_idx ON public.votes USING btree (post_id);


--
-- Name: votes_post_type_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_post_type_active_idx ON public.votes USING btree (post_id, vote_type, is_active) WHERE ((post_id IS NOT NULL) AND (is_active = true));


--
-- Name: votes_reason_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_reason_id_idx ON public.votes USING btree (reason_id);


--
-- Name: votes_reason_type_active_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_reason_type_active_idx ON public.votes USING btree (reason_id, vote_type, is_active) WHERE ((reason_id IS NOT NULL) AND (is_active = true));


--
-- Name: votes_updated_by_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_updated_by_idx ON public.votes USING btree (updated_by);


--
-- Name: votes_user_active_created_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX votes_user_active_created_idx ON public.votes USING btree (created_by, is_active, created_at DESC) WHERE (is_active = true);


--
-- Name: votes_user_claim_type_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX votes_user_claim_type_key ON public.votes USING btree (created_by, claim_id, vote_type) WHERE ((claim_id IS NOT NULL) AND (is_active = true));


--
-- Name: votes_user_post_type_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX votes_user_post_type_key ON public.votes USING btree (created_by, post_id, vote_type) WHERE ((post_id IS NOT NULL) AND (is_active = true));


--
-- Name: votes_user_reason_type_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX votes_user_reason_type_key ON public.votes USING btree (created_by, reason_id, vote_type) WHERE ((reason_id IS NOT NULL) AND (is_active = true));


--
-- Name: claim_sets audit_claim_sets_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_claim_sets_trigger AFTER INSERT OR DELETE OR UPDATE ON public.claim_sets FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_claim_sets_trigger ON claim_sets; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_claim_sets_trigger ON public.claim_sets IS 'Audit trail for claim_sets table';


--
-- Name: claims audit_claims_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_claims_trigger AFTER INSERT OR DELETE OR UPDATE ON public.claims FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_claims_trigger ON claims; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_claims_trigger ON public.claims IS 'Audit trail for claims table';


--
-- Name: domains audit_domains_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_domains_trigger AFTER INSERT OR DELETE OR UPDATE ON public.domains FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_domains_trigger ON domains; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_domains_trigger ON public.domains IS 'Audit trail for domains table';


--
-- Name: levels audit_levels_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_levels_trigger AFTER INSERT OR DELETE OR UPDATE ON public.levels FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_levels_trigger ON levels; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_levels_trigger ON public.levels IS 'Audit trail for levels table';


--
-- Name: posts audit_posts_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_posts_trigger AFTER INSERT OR DELETE OR UPDATE ON public.posts FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_posts_trigger ON posts; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_posts_trigger ON public.posts IS 'Audit trail for posts table';


--
-- Name: reasons audit_reasons_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_reasons_trigger AFTER INSERT OR DELETE OR UPDATE ON public.reasons FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_reasons_trigger ON reasons; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_reasons_trigger ON public.reasons IS 'Audit trail for reasons table';


--
-- Name: topics audit_topics_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_topics_trigger AFTER INSERT OR DELETE OR UPDATE ON public.topics FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_topics_trigger ON topics; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_topics_trigger ON public.topics IS 'Audit trail for topics table';


--
-- Name: user_topics audit_user_topics_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_user_topics_trigger AFTER INSERT OR DELETE OR UPDATE ON public.user_topics FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_user_topics_trigger ON user_topics; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_user_topics_trigger ON public.user_topics IS 'Audit trail for user_topics table';


--
-- Name: users audit_users_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_users_trigger AFTER INSERT OR DELETE OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_users_trigger ON users; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_users_trigger ON public.users IS 'Audit trail for users table';


--
-- Name: votes audit_votes_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER audit_votes_trigger AFTER INSERT OR DELETE OR UPDATE ON public.votes FOR EACH ROW EXECUTE FUNCTION public.audit_trigger_func();


--
-- Name: TRIGGER audit_votes_trigger ON votes; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TRIGGER audit_votes_trigger ON public.votes IS 'Audit trail for votes table';


--
-- Name: claim_sets claim_sets_search_vector_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER claim_sets_search_vector_trigger BEFORE INSERT OR UPDATE OF name, description ON public.claim_sets FOR EACH ROW EXECUTE FUNCTION public.claim_sets_search_vector_update();


--
-- Name: claims claims_search_vector_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER claims_search_vector_trigger BEFORE INSERT OR UPDATE OF claim_code, name, statement, statement_tags ON public.claims FOR EACH ROW EXECUTE FUNCTION public.claims_search_vector_update();


--
-- Name: claim_sets set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.claim_sets FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: claims set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.claims FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: domains set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.domains FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: levels set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.levels FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: posts set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.posts FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: reasons set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.reasons FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: topics set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.topics FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: user_topics set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.user_topics FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: votes set_updated_by; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER set_updated_by BEFORE UPDATE ON public.votes FOR EACH ROW EXECUTE FUNCTION public.handle_update_defaults();


--
-- Name: claim_sets claim_sets_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claim_sets
    ADD CONSTRAINT claim_sets_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: claim_sets claim_sets_domain_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claim_sets
    ADD CONSTRAINT claim_sets_domain_id_fkey FOREIGN KEY (domain_id) REFERENCES public.domains(id) ON DELETE RESTRICT;


--
-- Name: claim_sets claim_sets_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claim_sets
    ADD CONSTRAINT claim_sets_moderated_by_fkey FOREIGN KEY (moderated_by) REFERENCES auth.users(id) ON DELETE SET NULL;


--
-- Name: claim_sets claim_sets_topic_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claim_sets
    ADD CONSTRAINT claim_sets_topic_id_fkey FOREIGN KEY (topic_id) REFERENCES public.topics(id) ON DELETE RESTRICT;


--
-- Name: claim_sets claim_sets_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claim_sets
    ADD CONSTRAINT claim_sets_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: claims claims_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claims
    ADD CONSTRAINT claims_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: claims claims_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claims
    ADD CONSTRAINT claims_moderated_by_fkey FOREIGN KEY (moderated_by) REFERENCES auth.users(id) ON DELETE SET NULL;


--
-- Name: claims claims_set_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claims
    ADD CONSTRAINT claims_set_id_fkey FOREIGN KEY (set_id) REFERENCES public.claim_sets(id);


--
-- Name: claims claims_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.claims
    ADD CONSTRAINT claims_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: domains domains_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.domains
    ADD CONSTRAINT domains_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: domains domains_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.domains
    ADD CONSTRAINT domains_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: history history_changed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.history
    ADD CONSTRAINT history_changed_by_fkey FOREIGN KEY (changed_by) REFERENCES auth.users(id) ON DELETE SET NULL;


--
-- Name: levels levels_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.levels
    ADD CONSTRAINT levels_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: levels levels_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.levels
    ADD CONSTRAINT levels_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: posts posts_claim_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_claim_id_fkey FOREIGN KEY (claim_id) REFERENCES public.claims(id);


--
-- Name: posts posts_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: posts posts_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_moderated_by_fkey FOREIGN KEY (moderated_by) REFERENCES auth.users(id) ON DELETE SET NULL;


--
-- Name: posts posts_reason_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_reason_id_fkey FOREIGN KEY (reason_id) REFERENCES public.reasons(id);


--
-- Name: posts posts_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: reasons reasons_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reasons
    ADD CONSTRAINT reasons_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: reasons reasons_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reasons
    ADD CONSTRAINT reasons_moderated_by_fkey FOREIGN KEY (moderated_by) REFERENCES auth.users(id) ON DELETE SET NULL;


--
-- Name: reasons reasons_reason_claim_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reasons
    ADD CONSTRAINT reasons_reason_claim_id_fkey FOREIGN KEY (reason_claim_id) REFERENCES public.claims(id) ON DELETE CASCADE;


--
-- Name: reasons reasons_target_claim_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reasons
    ADD CONSTRAINT reasons_target_claim_id_fkey FOREIGN KEY (target_claim_id) REFERENCES public.claims(id) ON DELETE CASCADE;


--
-- Name: reasons reasons_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reasons
    ADD CONSTRAINT reasons_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: topics topics_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.topics
    ADD CONSTRAINT topics_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: topics topics_parent_topic_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.topics
    ADD CONSTRAINT topics_parent_topic_id_fkey FOREIGN KEY (parent_topic_id) REFERENCES public.topics(id) ON DELETE RESTRICT;


--
-- Name: topics topics_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.topics
    ADD CONSTRAINT topics_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: user_topics user_topics_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_topics
    ADD CONSTRAINT user_topics_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: user_topics user_topics_domain_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_topics
    ADD CONSTRAINT user_topics_domain_id_fkey FOREIGN KEY (domain_id) REFERENCES public.domains(id) ON DELETE CASCADE;


--
-- Name: user_topics user_topics_topic_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_topics
    ADD CONSTRAINT user_topics_topic_id_fkey FOREIGN KEY (topic_id) REFERENCES public.topics(id) ON DELETE CASCADE;


--
-- Name: user_topics user_topics_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_topics
    ADD CONSTRAINT user_topics_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: user_topics user_topics_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_topics
    ADD CONSTRAINT user_topics_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: users users_bible_confidence_level_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_bible_confidence_level_fkey FOREIGN KEY (bible_confidence_level) REFERENCES public.levels(id) ON DELETE RESTRICT;


--
-- Name: users users_default_domain_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_default_domain_id_fkey FOREIGN KEY (default_domain_id) REFERENCES public.domains(id) ON DELETE SET NULL;


--
-- Name: users users_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_id_fkey FOREIGN KEY (id) REFERENCES auth.users(id) ON DELETE CASCADE;


--
-- Name: votes votes_claim_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.votes
    ADD CONSTRAINT votes_claim_id_fkey FOREIGN KEY (claim_id) REFERENCES public.claims(id);


--
-- Name: votes votes_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.votes
    ADD CONSTRAINT votes_created_by_fkey FOREIGN KEY (created_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: votes votes_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.votes
    ADD CONSTRAINT votes_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id);


--
-- Name: votes votes_reason_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.votes
    ADD CONSTRAINT votes_reason_id_fkey FOREIGN KEY (reason_id) REFERENCES public.reasons(id);


--
-- Name: votes votes_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.votes
    ADD CONSTRAINT votes_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES auth.users(id) ON DELETE RESTRICT;


--
-- Name: votes votes_vote_level_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.votes
    ADD CONSTRAINT votes_vote_level_id_fkey FOREIGN KEY (vote_level_id) REFERENCES public.levels(id);


--
-- Name: claims Admins can manage all claims; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can manage all claims" ON public.claims USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: posts Admins can manage all posts; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can manage all posts" ON public.posts USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: reasons Admins can manage all reasons; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can manage all reasons" ON public.reasons USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: claim_sets Admins can manage claim sets; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can manage claim sets" ON public.claim_sets USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: topics Admins can manage topics; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can manage topics" ON public.topics USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: domains Admins can modify domains; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can modify domains" ON public.domains USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: levels Admins can modify levels; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can modify levels" ON public.levels USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: users Admins can update any profile; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can update any profile" ON public.users FOR UPDATE USING ((EXISTS ( SELECT 1
   FROM public.users users_1
  WHERE ((users_1.id = auth.uid()) AND (users_1.is_admin = true)))));


--
-- Name: user_topics Admins can view all subscriptions; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Admins can view all subscriptions" ON public.user_topics FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.users
  WHERE ((users.id = auth.uid()) AND (users.is_admin = true)))));


--
-- Name: domains Anyone can view active domains; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Anyone can view active domains" ON public.domains FOR SELECT USING ((is_active = true));


--
-- Name: claims Authenticated users can create claims; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can create claims" ON public.claims FOR INSERT WITH CHECK ((auth.uid() IS NOT NULL));


--
-- Name: reasons Authenticated users can create reasons; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can create reasons" ON public.reasons FOR INSERT WITH CHECK ((auth.uid() IS NOT NULL));


--
-- Name: claims Authenticated users can view active claims; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can view active claims" ON public.claims FOR SELECT USING (((auth.uid() IS NOT NULL) AND (is_active = true)));


--
-- Name: posts Authenticated users can view active posts; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can view active posts" ON public.posts FOR SELECT USING (((auth.uid() IS NOT NULL) AND (is_active = true)));


--
-- Name: reasons Authenticated users can view active reasons; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can view active reasons" ON public.reasons FOR SELECT USING (((auth.uid() IS NOT NULL) AND (is_active = true)));


--
-- Name: topics Authenticated users can view active topics; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can view active topics" ON public.topics FOR SELECT USING (((auth.uid() IS NOT NULL) AND (is_active = true)));


--
-- Name: votes Authenticated users can view active votes; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can view active votes" ON public.votes FOR SELECT USING (((auth.uid() IS NOT NULL) AND (is_active = true)));


--
-- Name: claim_sets Authenticated users can view claim sets; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can view claim sets" ON public.claim_sets FOR SELECT USING ((auth.uid() IS NOT NULL));


--
-- Name: levels Authenticated users can view levels; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Authenticated users can view levels" ON public.levels FOR SELECT USING ((auth.uid() IS NOT NULL));


--
-- Name: users Public profiles are viewable by registered users; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Public profiles are viewable by registered users" ON public.users FOR SELECT USING ((auth.uid() IS NOT NULL));


--
-- Name: posts Users can create posts; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can create posts" ON public.posts FOR INSERT WITH CHECK ((auth.uid() IS NOT NULL));


--
-- Name: votes Users can create votes; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can create votes" ON public.votes FOR INSERT WITH CHECK ((auth.uid() IS NOT NULL));


--
-- Name: posts Users can delete own unmoderated posts; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can delete own unmoderated posts" ON public.posts FOR DELETE USING (((auth.uid() = created_by) AND (moderated_at IS NULL)));


--
-- Name: votes Users can delete own votes; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can delete own votes" ON public.votes FOR DELETE USING ((auth.uid() = created_by));


--
-- Name: user_topics Users can manage own topic subscriptions; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can manage own topic subscriptions" ON public.user_topics USING ((auth.uid() = user_id));


--
-- Name: claims Users can update own claims; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can update own claims" ON public.claims FOR UPDATE USING (((auth.uid() = created_by) AND (moderated_at IS NULL)));


--
-- Name: users Users can update own profile; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can update own profile" ON public.users FOR UPDATE USING ((auth.uid() = id));


--
-- Name: reasons Users can update own reasons; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can update own reasons" ON public.reasons FOR UPDATE USING (((auth.uid() = created_by) AND (moderated_at IS NULL)));


--
-- Name: posts Users can update own unmoderated posts; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can update own unmoderated posts" ON public.posts FOR UPDATE USING (((auth.uid() = created_by) AND (moderated_at IS NULL)));


--
-- Name: votes Users can update own votes; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can update own votes" ON public.votes FOR UPDATE USING ((auth.uid() = created_by));


--
-- Name: user_topics Users can view own topic subscriptions; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY "Users can view own topic subscriptions" ON public.user_topics FOR SELECT USING ((auth.uid() = user_id));


--
-- Name: claim_sets; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.claim_sets ENABLE ROW LEVEL SECURITY;

--
-- Name: claims; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.claims ENABLE ROW LEVEL SECURITY;

--
-- Name: domains; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.domains ENABLE ROW LEVEL SECURITY;

--
-- Name: kv_store_a42d30f9; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.kv_store_a42d30f9 ENABLE ROW LEVEL SECURITY;

--
-- Name: levels; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.levels ENABLE ROW LEVEL SECURITY;

--
-- Name: posts; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.posts ENABLE ROW LEVEL SECURITY;

--
-- Name: reasons; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.reasons ENABLE ROW LEVEL SECURITY;

--
-- Name: topics; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.topics ENABLE ROW LEVEL SECURITY;

--
-- Name: user_topics; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.user_topics ENABLE ROW LEVEL SECURITY;

--
-- Name: users; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;

--
-- Name: votes; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.votes ENABLE ROW LEVEL SECURITY;

--
-- PostgreSQL database dump complete
--

\unrestrict UnvDtAvLF7C8akbkxsldf988qfzmAX4mR8wticpkiC1SY578tLKVSfBZmK8SnPa

