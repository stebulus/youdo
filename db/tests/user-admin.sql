\i create.sql

\echo create
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script');
insert into yd_user (name) values ('Alice')
returning id
    \gset user
end;
select * from yd_user_v;
select * from yd_user;

\echo update
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script');
update yd_user set name = 'Bob' where id = :userid;
end;
select * from yd_user_v;
select * from yd_user;

\echo delete
begin;
insert into transaction (yd_userid, yd_ipaddr, yd_useragent)
values (0, '127.0.0.1', :'script');
delete from yd_user where id = :userid;
end;
select * from yd_user_v;
select * from yd_user;
