-- psql script to create a YouDo database.
\set ON_ERROR_STOP

-- Data is immutable; "altered" data consists of new versions of that
-- data, and the old data is retained for reference.  Transactions are
-- reifed in the 'transaction' table, and versions are distinguished
-- by the id of the transaction which introduced them.

-- To create a versioned table:
--      CREATE TABLE foo AS (...);
--      SELECT versionize('foo');
-- The function versionize() will create a table foo_v to hold
-- all versions, and augment table foo with version information.
-- Both tables have the following columns:
--      txnid integer    (the transaction that created this version)
--      id integer       (the id of the object)
--      ... + the fields that foo had initially ...
-- The fields txnid and id in foo are created by versionize(); do
-- not create them yourself.  The table foo_v also has a field
--      deleted boolean
-- indicating whether the transaction deleted the record.  The tables
-- foo and foo_v have different constraints; see versionize()
-- for details.

-- To query the current data, select from foo as usual.  To query
-- historical data as of a particular transaction N, use something like
--      SELECT foo_v.*
--      FROM foo_v
--      NATURAL JOIN
--      (SELECT max(txnid) as txnid,  -- latest txn w/ a version of object
--              id
--       FROM foo_v
--       GROUP BY id
--       WHERE txnid <= N  -- version in effect may be from any previous txn
--      )
--      WHERE NOT foo_v.deleted;  -- exclude deleted objects

-- To make any change to the data, first create a new transaction,
-- then insert, update, and delete as usual, ignoring the txnid field.
-- For example:
--      BEGIN
--      INSERT INTO transaction (yd_userid, yd_ipaddr, yd_useragent)
--      VALUES (...);
--      INSERT INTO foo (... fields of foo other than txnid, id...)
--      VALUES (...);
--      END;
-- On insert, id will be set as usual; it is a SERIAL field.
-- On insert and update, txnid will be set to the id of the most
-- recently created transaction.  On insert, update, and delete,
-- an appropriate record will be added to foo_v.

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

-- Create a secondary table 'foo_v' to hold (current and historical)
-- versions of the data in the given table 'foo', and augment 'foo'
-- with version-tracking fields.
CREATE FUNCTION versionize(tab REGCLASS) RETURNS VOID AS $$
BEGIN
    -- Augment the table with unique IDs and with transaction IDs.
    EXECUTE format('
        ALTER TABLE %I ADD txnid INTEGER NOT NULL;
        ALTER TABLE %I ADD id SERIAL PRIMARY KEY;
        ', tab, tab);

    -- foo_v holds versions of objects from table foo, distinguished
    -- by the transaction that produced them.
    EXECUTE format('
        CREATE TABLE %I (LIKE %I);
        ALTER TABLE %I ADD deleted BOOLEAN NOT NULL DEFAULT FALSE;
        ', tab||'_v', tab, tab||'_v');

    -- Every object has at most one version from each transaction.
    EXECUTE format('
        ALTER TABLE %I ADD PRIMARY KEY (txnid, id);
        ', tab||'_v');

    -- The current version of an object is one of its versions.
    EXECUTE format('
        ALTER TABLE %I ADD FOREIGN KEY (txnid, id) REFERENCES %I;
        ', tab, tab||'_v');

    -- Log new and updated records.
    EXECUTE format('
        CREATE FUNCTION %I() RETURNS TRIGGER AS $FUNC$ BEGIN
            SELECT max(id) FROM transaction INTO NEW.txnid;
            WITH x AS (SELECT NEW::%I AS y)
            INSERT INTO %I SELECT (y).* FROM x;
            RETURN NEW;
        END; $FUNC$ LANGUAGE PLPGSQL;
        ', tab||'_change', tab, tab||'_v');
    EXECUTE FORMAT('
        CREATE TRIGGER %I BEFORE INSERT OR UPDATE ON %I
        FOR EACH ROW EXECUTE PROCEDURE %I();
        ', tab||'_change_trigger', tab, tab||'_change');

    -- Log deleted records.
    EXECUTE format('
        CREATE FUNCTION %I() RETURNS TRIGGER AS $FUNC$ BEGIN
            SELECT max(id) FROM transaction INTO OLD.txnid;
            WITH x AS (SELECT OLD::%I AS y)
            INSERT INTO %I SELECT (y).*, TRUE AS deleted FROM x;
            RETURN OLD;
        END; $FUNC$ LANGUAGE PLPGSQL;
        ', tab||'_delete', tab, tab||'_v');
    EXECUTE FORMAT('
        CREATE TRIGGER %I BEFORE DELETE ON %I
        FOR EACH ROW EXECUTE PROCEDURE %I();
        ', tab||'_delete_trigger', tab, tab||'_delete');
END; $$ LANGUAGE PLPGSQL;

CREATE TABLE db (version VARCHAR NOT NULL);
SELECT versionize('db') OFFSET 1;  -- offset 1 to suppress output

CREATE TABLE yd_user (name VARCHAR NOT NULL);
SELECT versionize('yd_user') OFFSET 1;

-- Initialize the database with metadata and a single user (yddb, user 0).
CREATE FUNCTION yddb_init() RETURNS VOID
AS $$
DECLARE
    txnid INTEGER;
BEGIN
    INSERT INTO transaction (yd_userid, yd_ipaddr, yd_useragent)
    VALUES (0, '127.0.0.1', 'youdo/db/create.sql');
    INSERT INTO yd_user (id, name) VALUES (0, 'yddb');
    INSERT INTO db (version) VALUES ('0.1');
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
