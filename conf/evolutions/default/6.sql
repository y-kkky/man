# --- Sixth database schema

# --- !Ups
INSERT INTO Questions (bilet_id, typ, text, image, answer) Values (1, 1, 'Pervii','http://www.iaf-messe.com/tl_files/pictures/firmenlogos/e/euk_logo.jpg', 1), (1, 2, 'Vtoroi', '', 2), (1, 3, 'Tretii', '', 2)


# --- !Downs
DELETE FROM Questions
