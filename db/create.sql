-- psql script to create a YouDo database.

\set ON_ERROR_STOP

CREATE TABLE transaction (
    id SERIAL PRIMARY KEY,
    tstamp TIMESTAMP WITH TIME ZONE NOT NULL,  -- (set by transaction_auto)

    -- The query that caused this transaction.
    query TEXT NOT NULL,                       -- (set by transaction_auto)

    -- Information about the user responsible for this transaction,
    -- as supplied by ydserver.  Not the same as the PostgreSQL user
    -- account under which the transaction was made, since that is
    -- the ydserver's account.  Here in the database we cannot verify
    -- this information.
    yd_userid INTEGER NOT NULL,
        -- Theoretically yd_userid should REFERENCE user.id, but
        -- that would mean we couldn't delete user accounts if they
        -- had ever made a transaction.  So, no REFERENCES constraint
        -- here, but see transaction_check_yd_userid_trigger below.
    yd_ipaddr INET NOT NULL,
    yd_useragent VARCHAR NOT NULL,

    -- Information about the Postgres user responsible for this transaction.
    -- This should almost always be the ydserver's account.
    pg_sessuser NAME NOT NULL,      -- Postgres session user (set by transaction_auto)
    pg_curruser NAME NOT NULL,      -- Postgres current user (set by transaction_auto)
    pg_ipaddr INET                  -- Postgres client's IP (set by transaction_auto)
                                    -- (Can be NULL; psql connects via Unix domain sockets.)
);

CREATE FUNCTION transaction_auto() RETURNS TRIGGER AS $$
    BEGIN
        NEW.tstamp = CURRENT_TIMESTAMP;
        NEW.query = CURRENT_QUERY();
        NEW.pg_sessuser = SESSION_USER;
        NEW.pg_curruser = CURRENT_USER;
        NEW.pg_ipaddr = INET_CLIENT_ADDR();
        RETURN NEW;
    END;
$$ LANGUAGE PLPGSQL;
CREATE TRIGGER transaction_auto_trigger
    BEFORE INSERT ON transaction
    FOR EACH ROW EXECUTE PROCEDURE transaction_auto();

CREATE TYPE db_t AS (version VARCHAR);
CREATE TABLE db_v
( txnid INTEGER REFERENCES transaction
, id INTEGER
, PRIMARY KEY (txnid,id)
, obj db_t
);
CREATE TABLE db
( id INTEGER PRIMARY KEY
, txnid INTEGER
, FOREIGN KEY (txnid,id) REFERENCES db_v
, obj db_t NOT NULL
);

CREATE SEQUENCE db_id_seq;
CREATE FUNCTION db_new_id() RETURNS TRIGGER AS $$
    BEGIN
        IF NEW.id IS NULL THEN
            NEW.id = nextval('db_id_seq');
        END IF;
        RETURN NEW;
    END;
$$ LANGUAGE PLPGSQL;
CREATE TRIGGER db_new_id_trigger
    BEFORE INSERT ON db_v
    FOR EACH ROW EXECUTE PROCEDURE db_new_id();
CREATE FUNCTION db_new_version() RETURNS TRIGGER AS $$
    BEGIN
        IF NEW.obj IS NULL THEN
            DELETE FROM db WHERE id = NEW.id;
        ELSIF EXISTS(SELECT id FROM db WHERE id = NEW.id) THEN
            UPDATE db
            SET txnid = NEW.txnid, obj = NEW.obj
            WHERE id = NEW.id;
        ELSE
            INSERT INTO db (id, txnid, obj)
            VALUES (NEW.id, NEW.txnid, NEW.obj);
        END IF;
        RETURN NULL;
    END;
$$ LANGUAGE PLPGSQL;
CREATE TRIGGER db_new_version_trigger
    AFTER INSERT ON db_v
    FOR EACH ROW EXECUTE PROCEDURE db_new_version();

CREATE TYPE yd_user_t AS (name VARCHAR);

CREATE TABLE yd_user_v
( txnid INTEGER REFERENCES transaction
, id INTEGER
, PRIMARY KEY (txnid,id)
, obj yd_user_t
);
CREATE TABLE yd_user
( id INTEGER PRIMARY KEY
, txnid INTEGER
, FOREIGN KEY (txnid,id) REFERENCES yd_user_v
, obj yd_user_t NOT NULL
);

CREATE SEQUENCE yd_user_id_seq;
CREATE FUNCTION yd_user_new_id() RETURNS TRIGGER AS $$
    BEGIN
        IF NEW.id IS NULL THEN
            NEW.id = nextval('yd_user_id_seq');
        END IF;
        RETURN NEW;
    END;
$$ LANGUAGE PLPGSQL;
CREATE TRIGGER yd_user_new_id_trigger
    BEFORE INSERT ON yd_user_v
    FOR EACH ROW EXECUTE PROCEDURE yd_user_new_id();

CREATE FUNCTION yd_user_new_version() RETURNS TRIGGER AS $$
    BEGIN
        IF NEW.obj IS NULL THEN
            DELETE FROM yd_user WHERE id = NEW.id;
        ELSIF EXISTS(SELECT id FROM yd_user WHERE id = NEW.id) THEN
            UPDATE yd_user
            SET txnid = NEW.txnid, obj = NEW.obj
            WHERE id = NEW.id;
        ELSE
            INSERT INTO yd_user (id, txnid, obj)
            VALUES (NEW.id, NEW.txnid, NEW.obj);
        END IF;
        RETURN NULL;
    END;
$$ LANGUAGE PLPGSQL;
CREATE TRIGGER yd_user_new_version_trigger
    AFTER INSERT ON yd_user_v
    FOR EACH ROW EXECUTE PROCEDURE yd_user_new_version();

CREATE FUNCTION yddb_init() RETURNS VOID AS $$
DECLARE
    txnid INTEGER;
BEGIN
    WITH txn AS (
        INSERT INTO transaction (yd_userid, yd_ipaddr, yd_useragent)
        VALUES (0, '127.0.0.1', 'youdo/db/create.sql')
        RETURNING id
    ) SELECT txn.id FROM txn INTO txnid;
    INSERT INTO yd_user_v (txnid, id, obj)
    VALUES (txnid, 0, ROW('yddb')::yd_user_t);
    INSERT INTO db_v (txnid, id, obj)
    VALUES (txnid, null, ROW('0.1')::db_t);
END;
$$ LANGUAGE PLPGSQL;
SELECT yddb_init() OFFSET 1;  -- "offset 1": yddb_init is called, but no output
DROP FUNCTION yddb_init();

CREATE FUNCTION transaction_check_yd_userid() RETURNS TRIGGER AS $$
    BEGIN
        IF NOT EXISTS(SELECT id FROM yd_user WHERE id = NEW.yd_userid) THEN
            RAISE EXCEPTION 'Invalid userid %', NEW.yd_userid;
        END IF;
        RETURN NEW;
    END;
$$ LANGUAGE PLPGSQL;
CREATE TRIGGER transaction_check_yd_userid_trigger
    BEFORE INSERT ON transaction
    FOR EACH ROW EXECUTE PROCEDURE transaction_check_yd_userid();
