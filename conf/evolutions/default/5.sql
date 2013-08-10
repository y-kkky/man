# --- BiletStat database schema

# --- !Ups
INSERT INTO BiletStat (user_id, bilet_id, perc) VALUES (1, 1, 70), (1, 2, 71), (1, 3, 71)

# --- !Downs
DELETE FROM BiletStat
