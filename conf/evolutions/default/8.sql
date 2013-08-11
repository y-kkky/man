# --- Nineth database schema

# --- !Ups
INSERT INTO Variants (question_id, text) VALUES (1, 'hello'), (1, 'byby'), (1, 'hihihi'), (2, '1 first'), (2, '1 second'), (2, '2 first'), (2, '2 second')


# --- !Downs
DELETE FROM Variants
