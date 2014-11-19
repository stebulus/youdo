\i create.sql

\echo create
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script')
returning id
    \gset txn
insert into yd_user_v (txnid, obj)
values (:txnid, row('Alice'))
returning id
    \gset user
end;
select * from yd_user_v;
select * from yd_user;

\echo update
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script')
returning id
    \gset txn
insert into yd_user_v (txnid, id, obj)
values (:txnid, :userid, row('Bob'));
end;
select * from yd_user_v;
select * from yd_user;

\echo delete
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script')
returning id
    \gset txn
insert into yd_user_v (txnid, id, obj)
values (:txnid, :userid, null);
end;
select * from yd_user_v;
select * from yd_user;
