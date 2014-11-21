Getting started
===============

One way to set up a YouDo database for testing and development:

1. In `psql`, as a user with sufficient privileges to the current
database, create and populate a testing schema:

        create schema ydserver;
        set search_path=ydserver;
        \i db/create.sql
        \unset ON_ERROR_STOP  -- in case role ydserver already exists
        \i db/ydserver.sql

2. Create a service description for the ydserver to use: from the shell,

        touch ~/etc/pg_service.conf
        chmod 600 ~/etc/pg_service.conf  -- it contains a password
        cat >> ~/etc/pg_service.conf <<EOF
        [ydservice]
        host=localhost
        dbname=YOURDBNAME
        user=ydserver
        password=PASSWORD
        EOF

3. Build and run the web server: from the shell,

        cd web
        cabal install --only-dependencies
        cabal configure
        cabal build
        PGSYSCONFDIR=~/etc dist/build/ydserver/ydserver 3087 service=ydservice

4. Visit [http://localhost:3087/](http://localhost:3087/).
