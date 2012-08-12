CREATE TABLE IF NOT EXISTS Author
(ID     INTEGER UNSIGNED AUTO_INCREMENT,
 First  TEXT,
 Middle TEXT,
 Last   TEXT,
 PRIMARY KEY ID (ID),
 UNIQUE INDEX `Last Name` (Last, First, Middle),
 UNIQUE INDEX Name (First, Middle, Last));

CREATE TABLE IF NOT EXISTS Collection
(ID     INTEGER UNSIGNED AUTO_INCREMENT,
 Title  TEXT,
 Year   SMALLINT UNSIGNED,
 PRIMARY KEY ID (ID),
 UNIQUE INDEX Title_Year (Title, Year));

CREATE TABLE IF NOT EXISTS Series
(ID     INTEGER UNSIGNED AUTO_INCREMENT,
 Title  TEXT,
 PRIMARY KEY ID (ID),
 UNIQUE INDEX Title (Title));

CREATE TABLE IF NOT EXISTS Title
(ID     INTEGER UNSIGNED AUTO_INCREMENT,
 Title  TEXT,
 Year   SMALLINT UNSIGNED,
 Comment TEXT,
 Rating TINYINT UNSIGNED,
 PRIMARY KEY ID (ID),
 UNIQUE INDEX `Title Year` (Title,Year));

CREATE TABLE IF NOT EXISTS AuthorCollection
(Author  INTEGER UNSIGNED,
 Collection INTEGER UNSIGNED,
 INDEX Author (Author),
 INDEX Collection (Collection));

CREATE TABLE IF NOT EXISTS AuthorSeries
(Author  INTEGER UNSIGNED,
 Series INTEGER UNSIGNED,
 INDEX Author (Author),
 INDEX Series (Series));

CREATE TABLE IF NOT EXISTS AuthorTitle
(Author INTEGER UNSIGNED,
 Title  INTEGER UNSIGNED,
 INDEX Author (Author),
 INDEX Title (Title));

CREATE TABLE IF NOT EXISTS SeriesTitle
(Series INTEGER UNSIGNED,
 Title  INTEGER UNSIGNED,
 INDEX Series (Series),
 INDEX Title (Title));

CREATE TABLE IF NOT EXISTS CollectionTitle
(Collection INTEGER UNSIGNED,
 Title  INTEGER UNSIGNED,
 INDEX Collection (Collection),
 INDEX Title (Title));

-- end of file
