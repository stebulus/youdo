\i create.sql
\unset ON_ERROR_STOP

\echo insert without transaction
begin;
insert into yd_user (name) values ('Charlie');
end;

\echo update without transaction
begin;
update yd_user set name='Bob' where id = 0;
end;

\echo delete without transaction
begin;
delete from yd_user where id = 0;
end;

\echo insert with old transaction
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script')
returning id
    \gset txn
end;
begin;
insert into yd_user (txnid, name) values (:txnid, 'Charlie');
end;

\echo update with old transaction
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script')
returning id
    \gset txn
end;
begin;
update yd_user set txnid=:txnid, name='Charlie' where id=0;
end;
