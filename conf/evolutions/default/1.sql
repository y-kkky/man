# --- First database schema

# --- !Ups

CREATE SEQUENCE user_id_seq;
create table Users (
  id 			    integer not null default nextval('user_id_seq') primary key,
  email                     varchar(255) not null,
  name                      varchar(255) not null,
  pass                      varchar(255) not null
);


# --- !Downs
drop table if exists Users;
DROP SEQUENCE user_id_seq;
