-- SQLite3 syntax

CREATE TABLE Author
(ID     INTEGER PRIMARY KEY,
 First  TEXT,
 Middle TEXT,
 Last   TEXT);
 
CREATE UNIQUE INDEX Author_Last_Name on Author (Last, First, Middle);
CREATE UNIQUE INDEX Author_Name on Author (First, Middle, Last);

CREATE TABLE Collection
(ID     INTEGER PRIMARY KEY,
 Title  TEXT,
 Year   SMALLINT UNSIGNED);

CREATE UNIQUE INDEX Collection_Title_Year on Collection (Title, Year);

CREATE TABLE Series
(ID     INTEGER PRIMARY KEY,
 Title  TEXT);
 
CREATE UNIQUE INDEX Series_Title on Series (Title);

CREATE TABLE Title
(ID       INTEGER PRIMARY KEY,
 Title    TEXT,
 Year     SMALLINT UNSIGNED,
 Comment  TEXT,
 Location TEXT);

CREATE UNIQUE INDEX Title_Title_Year on Title (Title, Year);

CREATE TABLE AuthorCollection
(Author  INTEGER UNSIGNED,
 Collection INTEGER UNSIGNED);
 
CREATE INDEX AuthorCollection_Author on AuthorCollection (Author);
CREATE INDEX AuthorCollection_Collection on AuthorCollection (Collection);

CREATE TABLE AuthorSeries
(Author INTEGER UNSIGNED,
 Series INTEGER UNSIGNED);

CREATE INDEX AuthorSeries_Author on AuthorSeries (Author);
CREATE INDEX AuthorSeries_Series on AuthorSeries (Series);

CREATE TABLE AuthorTitle
(Author INTEGER UNSIGNED,
 Title  INTEGER UNSIGNED);

CREATE INDEX AuthorTitle_Author on AuthorTitle (Author);
CREATE INDEX AuthorTitle_Title on AuthorTitle (Title);

CREATE TABLE SeriesTitle
(Series INTEGER UNSIGNED,
 Title  INTEGER UNSIGNED);

CREATE INDEX SeriesTitle_Series on SeriesTitle (Series);
CREATE INDEX SeriesTitle_Title on SeriesTitle (Title);

CREATE TABLE CollectionTitle
(Collection INTEGER UNSIGNED,
 Title  INTEGER UNSIGNED);
 
CREATE INDEX CollectionTitle_Collection on CollectionTitle (Collection);
CREATE INDEX CollectionTitle_Title on CollectionTitle (Title);

--  .exit here does not cause sqlite3 to leave interactive mode.
--  Providing a useless statement on the sqlite3 command line does.

-- end of file
