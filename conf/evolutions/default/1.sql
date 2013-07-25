# --- First database schema

# --- !Ups

CREATE SEQUENCE user_id_seq;
create table Users (
  id 			    integer not null default nextval('user_id_seq') primary key,
  regtime                   varchar not null,
  email                     varchar(255) not null,
  name                      varchar(255) not null,
  city                      varchar(100),
  school                    varchar(100),
  comments text,
  pass                      varchar(255) not null
);


# --- !Downs
drop table if exists Users;
DROP SEQUENCE user_id_seq;
