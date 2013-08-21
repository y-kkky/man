# --- Fourth database schema

# --- !Ups
INSERT INTO Bilets (lesson_id, time) VALUES (1, '00:10:00')

# --- !Downs
DELETE FROM Bilets
