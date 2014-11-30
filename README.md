Getting started
===============

One way to set up a YouDo database for testing and development:

1. If you don't already have a user with sufficient privileges to
a database to do step 2, then run `psql` as the superuser `postgres`:

        sudo -u postgres psql

   and then give yourself the necessary powers:

        create database YOURDBNAME;
        grant all on database YOURDBNAME to USERNAME;

   If you don't want to give your day-to-day account privileges to create
   users, then create the `ydserver` account now:

        create role ydserver login password 'PASSWORD';

2. In `psql`, as a user with sufficient privileges to the current
database, create and populate a testing schema:

        create schema ydserver;
        set search_path=ydserver;
        \i db/create.sql
        \unset ON_ERROR_STOP  -- in case role ydserver already exists
        \i db/ydserver.sql
        grant usage to schema ydserver to ydserver;

3. Create a service description for the ydserver to use: from the shell,

        touch ~/etc/pg_service.conf
        chmod 600 ~/etc/pg_service.conf  # it contains a password
        cat >> ~/etc/pg_service.conf <<EOF
        [ydservice]
        host=localhost
        dbname=YOURDBNAME
        user=ydserver
        password=PASSWORD
        EOF

4. Build and run the web server: from the shell,

        cd web
        cabal install --only-dependencies
        cabal configure --enable-tests
        cabal build
        cabal test
        PGSYSCONFDIR=~/etc dist/build/ydserver/ydserver 3087 service=ydservice

5. Visit [http://localhost:3087/](http://localhost:3087/).
