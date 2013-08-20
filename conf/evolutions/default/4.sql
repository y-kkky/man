# --- Fourth database schema

# --- !Ups
INSERT INTO Bilets (lesson_id) VALUES (1), (1), (1)

# --- !Downs
DELETE FROM Bilets
