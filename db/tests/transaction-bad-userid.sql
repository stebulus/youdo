-- Tests that inserting a transaction with an invalid user ID fails.
-- Normally this would be a silly test because there would be a
-- foreign key constraint on the transaction table, and what's the
-- point of us testing PostgreSQL's foreign key implementation?
-- But this particular constraint is enforced by a trigger whose
-- code we wrote ourselves, so it's testworthy.  (See create.sql for
-- explanation of why it's not a constraint.)
\i create.sql
\unset ON_ERROR_STOP
begin;
select max(id)+1 as id from yd_user_v
    \gset bad
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (:badid, '127.0.0.1', :'script');
end;

begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script')
returning id
    \gset txn
insert into yd_user_v (id, txnid, obj)
values (0, :txnid, null);  -- delete user 0; don't do this for real!
end;

begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script');
end;
