-- Create a ydserver user account with appropriate permissions.
-- You must separately execute
--      GRANT USAGE TO SCHEMA foo TO ydserver
-- where foo is the schema containing the Youdo tables.
CREATE ROLE ydserver LOGIN;
GRANT SELECT ON transaction, db, db_v, yd_user, yd_user_v, youdo, youdo_v
    TO ydserver;
GRANT INSERT ON transaction, db, db_v, yd_user, yd_user_v, youdo, youdo_v
    TO ydserver;
GRANT UPDATE ON yd_user, youdo TO ydserver;
GRANT DELETE ON yd_user, youdo TO ydserver;
