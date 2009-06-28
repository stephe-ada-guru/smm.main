use test;

insert into Author (First, Middle, Last) values ("Arthur", "C.", "Clarke");
insert into Author (First, Middle, Last) values ("Isaac", "", "Asimov");

insert into Title (Title, Year, Comment, Rating) values ("2001", 1970, "Obelisk", 9);
insert into Title (Title, Year, Comment, Rating) values ("Foundation", 1960, "the first", 9);

insert into AuthorTitle (Author, Title) values (1, 1);
insert into AuthorTitle (Author, Title) values (1, 2);
insert into AuthorTitle (Author, Title) values (2, 2);
