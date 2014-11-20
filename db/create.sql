-- psql script to create a YouDo database.
\set ON_ERROR_STOP

-- Data is immutable; "altered" data consists of new versions of that
-- data, and the old data is retained for reference.  Transactions are
-- reifed in the 'transaction' table, and versions are distinguished
-- by the id of the transaction which introduced them.

-- To create a versioned table:
--      CREATE TYPE foo_t AS (...);
--      SELECT versioned_table('foo_t');
-- The type's name must end with '_t'.  The function versioned_table()
-- will create two tables, foo_v (holding all versions) and foo
-- (holding the current version), which have the same fields:
--      txnid integer    (the transaction that created this version)
--      id integer       (the id of the object)
--      obj foo_t        (the object itself)
-- The differences are in constraints; see versioned_table() below for
-- details.  Note that, since the actual data is in a column with composite
-- type, we must frequently use the (obj).field and ROW(...) syntax.
-- See http://www.postgresql.org/docs/9.3/interactive/rowtypes.html

-- To query the current data, select from foo as usual.  To query
-- historical data as of a particular transaction N, use something like
--      SELECT foo_v.id, foo_v.(obj).*
--      FROM foo_v
--      NATURAL JOIN
--      (SELECT max(txnid) as txnid,  -- latest txn w/ a version of object
--              id
--       FROM foo_v
--       GROUP BY id
--       WHERE txnid <= N  -- version in effect may be from any previous txn
--      )
--      WHERE foo_v.obj IS NOT NULL;  -- exclude deleted objects

-- To update an existing record in foo, insert a record into foo_v
-- with the current transaction number, the id of the object being
-- updated, and the new version of the object.  For example, in a
-- psql script,
--      BEGIN;
--      INSERT INTO transaction (yd_userid, yd_ipaddr, yd_useragent)
--      VALUES (...) RETURNING id
--          \gset txn
--      INSERT INTO foo_v (txnid, id, obj)
--      VALUES (txn.id, id_being_modified, ROW(...));
--      END;
-- Triggers will update foo accordingly.

-- To add a new record to foo, insert a record into foo_v with the
-- current transaction number, the new object, and no specified id.
-- For example,
--      BEGIN;
--      INSERT INTO transaction (yd_userid, yd_ipaddr, yd_useragent)
--      VALUES (...) RETURNING id
--          \gset txn
--      INSERT INTO foo_v (txnid, obj)
--      VALUES (txn.id, ROW(...));
--      END;
-- A trigger will update foo accordingly.

-- To delete a record from foo, insert a record into foo_v with the
-- current transaction number, the id of the object being deleted,
-- and a NULL object.  For example,
--      BEGIN;
--      INSERT INTO transaction (yd_userid, yd_ipaddr, yd_useragent)
--      VALUES (...) RETURNING id
--          \gset txn
--      INSERT INTO foo_v (txnid, id, obj)
--      VALUES (txn.id, id_being_deleted, NULL);
--      END;
-- A trigger will delete the record from foo accordingly.

-- To insert, update, and delete records in foo, users need the
-- corresponding privilege to foo; they also need insert privilege
-- to foo_v and insert privilege to transaction.

-- Constraints for objects in foo_v/foo should be imposed on the foo
-- table, NOT on the foo_v table.  Rationale: for example, foreign
-- key constraints cannot be satisfied by historical data because the
-- referred-to record may itself have been deleted.  (And it's awkward
-- to refer to the appropriate historical version of the referred-to
-- record because the transactions which introduced the referring and
-- the referred-to records may not be the same.)  Moreover, when the
-- database design changes, historical data will not comply with any
-- new constraints and should not be made to; data is only required
-- to comply with the constraints in effect when it is produced.

-- It is possible to reconstruct foo from foo_v, but foo's role is
-- not merely to cache the current version of data for performance,
-- since it is also the locus of data integrity constraints.

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

-- Create tables to hold versioned data of the given type.
CREATE FUNCTION versioned_table(typ REGTYPE) RETURNS VOID
AS $$
DECLARE
    basename VARCHAR;
BEGIN
    IF right(typ::TEXT, 2) <> '_t' THEN
        RAISE EXCEPTION 'Type name % does not end with _t', typ;
    END IF;
    basename := left(typ::TEXT, -2);

    -- foo_v holds versions of objects of type foo_t, distinguished
    -- by the transaction that produced them.
    EXECUTE format('
        CREATE TABLE %I
        ( txnid INTEGER REFERENCES transaction
        , id SERIAL
        , PRIMARY KEY (txnid,id)
        , obj %I
        );
        ', basename||'_v', typ);

    -- foo holds the current version from foo_v (of nondeleted objects).
    EXECUTE format('
        CREATE TABLE %I
        ( txnid INTEGER
        , id INTEGER PRIMARY KEY
        , FOREIGN KEY (txnid,id) REFERENCES %I
        , obj %I NOT NULL
        );
        ', basename, basename||'_v', typ);

    -- New versions inserted into foo_v cause foo to be updated.
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
    INSERT INTO db_v (txnid, obj)
    VALUES (txnid, ROW('0.1')::db_t);
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
