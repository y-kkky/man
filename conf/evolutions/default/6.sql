# --- Eight database schema

# --- !Ups
INSERT INTO Users (regtime, email, name, city, school, comments, lessons, pass, rank) Values (111, 'yarik.just@gmail.com', 'Yarik', 'Niko', '2', 'comma', '123', 'C634AE0B9046D163A6E7E259A1AC49D70C5A88B5', 10)


# --- !Downs
DELETE FROM Users
