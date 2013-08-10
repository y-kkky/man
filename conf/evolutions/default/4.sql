# --- Fourth database schema

# --- !Ups
INSERT INTO Bilets (lesson_id, num) VALUES (1, 1), (1, 2), (1, 3)

# --- !Downs
DELETE FROM Bilets
