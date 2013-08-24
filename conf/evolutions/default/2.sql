# --- Second database schema

# --- !Ups

CREATE SEQUENCE lesson_id_seq;
create table Lessons (
  id 			    integer not null default nextval('lesson_id_seq') primary key,
  name        varchar(255)
);

CREATE SEQUENCE bilet_id_seq;
create table Bilets (
  id 			    integer not null default nextval('bilet_id_seq') primary key,
  lesson_id   integer,
  time	      varchar
);
CREATE SEQUENCE question_id_seq;
create table Questions (
  id 			    integer not null default nextval('question_id_seq') primary key,
  bilet_id    integer,
  typ 	      integer,
  text        text,
  image       varchar,
  answer      varchar
);
CREATE SEQUENCE variant_id_seq;
create table Variants (
  id 			    integer not null default nextval('variant_id_seq') primary key,
  question_id int,
  text        varchar(255)
);
create table Stat (
  user_id     integer,
  bilet_id    integer,
  question_id integer,
  right_a     integer,
  answer      varchar
);
create table BiletStat (
  user_id    integer,
  bilet_id   integer,
  ra	     integer,
  max_a	     integer
);
create table DailyStat (
 user_id     integer,
 question_id integer,
 curr_time   varchar,
 right_a     integer,
 answer	     varchar
);
create table microDailyStat (
 user_id     integer,
 curr_time   varchar,
 res_time    integer,
 score	     double,
 ids	     varchar
)
# --- !Downs
drop table if exists Lessons;
DROP SEQUENCE lesson_id_seq;
drop table if exists Bilets;
DROP SEQUENCE bilet_id_seq;
drop table if exists Questions;
DROP SEQUENCE question_id_seq;
drop table if exists Variants;
DROP SEQUENCE variant_id_seq;
drop table if exists Stat;
drop table if exists BiletStat;
drop table if exists DailyStat;
drop table if exists microDailyStat;
