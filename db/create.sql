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

CREATE FUNCTION transaction_auto() RETURNS TRIGGER
AS $$ BEGIN
    NEW.tstamp = CURRENT_TIMESTAMP;
    NEW.query = CURRENT_QUERY();
    NEW.pg_sessuser = SESSION_USER;
    NEW.pg_curruser = CURRENT_USER;
    NEW.pg_ipaddr = INET_CLIENT_ADDR();
    RETURN NEW;
END; $$ LANGUAGE PLPGSQL;
CREATE TRIGGER transaction_auto_trigger
BEFORE INSERT ON transaction
FOR EACH ROW EXECUTE PROCEDURE transaction_auto();

CREATE FUNCTION versioned_table(typ REGTYPE) RETURNS VOID
AS $$
DECLARE
    basename VARCHAR;
BEGIN
    IF right(typ::TEXT, 2) <> '_t' THEN
        RAISE EXCEPTION 'Type name % does not end with _t', typ;
    END IF;
    basename := left(typ::TEXT, -2);
    EXECUTE format('
        CREATE TABLE %I
        ( txnid INTEGER REFERENCES transaction
        , id INTEGER
        , PRIMARY KEY (txnid,id)
        , obj %I
        );
        ', basename||'_v', typ);
    EXECUTE format('
        CREATE TABLE %I
        ( txnid INTEGER
        , id INTEGER PRIMARY KEY
        , FOREIGN KEY (txnid,id) REFERENCES %I
        , obj %I NOT NULL
        );
        ', basename, basename||'_v', typ);
    EXECUTE format('CREATE SEQUENCE %I;', basename||'_id_seq');
    EXECUTE format('
        CREATE FUNCTION %I() RETURNS TRIGGER
        AS $TRIG$ BEGIN
            IF NEW.id IS NULL THEN
                NEW.id = nextval(%L);
            END IF;
            RETURN NEW;
        END; $TRIG$ LANGUAGE PLPGSQL;
        ', basename||'_new_id', basename||'_id_seq');
    EXECUTE format('
        CREATE TRIGGER %I
        BEFORE INSERT ON %I
        FOR EACH ROW EXECUTE PROCEDURE %I();
        ', basename||'_new_id_trigger',
           basename||'_v',
           basename||'_new_id');
    EXECUTE format('
        CREATE FUNCTION %I() RETURNS TRIGGER AS $NEWVER$
            BEGIN
                IF NEW.obj IS NULL THEN
                    DELETE FROM %I WHERE id = NEW.id;
                ELSIF EXISTS(SELECT id FROM %I WHERE id = NEW.id) THEN
                    UPDATE %I
                    SET txnid = NEW.txnid, obj = NEW.obj
                    WHERE id = NEW.id;
                ELSE
                    INSERT INTO %I (id, txnid, obj)
                    VALUES (NEW.id, NEW.txnid, NEW.obj);
                END IF;
                RETURN NULL;
            END
        $NEWVER$ LANGUAGE PLPGSQL;
        ', basename||'_new_version', basename, basename, basename, basename);
    EXECUTE format('
        CREATE TRIGGER %I AFTER INSERT ON %I
        FOR EACH ROW EXECUTE PROCEDURE %I();
        ', basename||'_new_version_trigger', basename||'_v', basename||'_new_version');
END; $$ LANGUAGE PLPGSQL;

CREATE TYPE db_t AS (version VARCHAR);
SELECT versioned_table('db_t') OFFSET 1;  -- offset 1 to suppress output

CREATE TYPE yd_user_t AS (name VARCHAR);
SELECT versioned_table('yd_user_t') OFFSET 1;

-- Initialize the database with metadata and a single user (yddb, user 0).
CREATE FUNCTION yddb_init() RETURNS VOID
AS $$
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
END; $$ LANGUAGE PLPGSQL;
SELECT yddb_init() OFFSET 1;  -- offset 1 to suppress output
DROP FUNCTION yddb_init();

-- Note that this trigger must be installed after yddb_init(), since
-- otherwise the first transaction and user are mutually dependent.
CREATE FUNCTION transaction_check_yd_userid() RETURNS TRIGGER
AS $$ BEGIN
    IF NOT EXISTS(SELECT id FROM yd_user WHERE id = NEW.yd_userid) THEN
        RAISE EXCEPTION 'Invalid userid %', NEW.yd_userid;
    END IF;
    RETURN NEW;
END; $$ LANGUAGE PLPGSQL;
CREATE TRIGGER transaction_check_yd_userid_trigger
BEFORE INSERT ON transaction
FOR EACH ROW EXECUTE PROCEDURE transaction_check_yd_userid();
