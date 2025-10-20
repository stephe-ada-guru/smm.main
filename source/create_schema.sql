-- SQLite3 syntax

-- SQLite3 and/or GNATCOLL GPL 2016 has a bug when comparing DATETIME
-- fields to bound parameters of type Ada.Calendar.Time; it seems to
-- give random results. So we use a CHAR[19] type for *_Downloaded
-- fields, with format "YYYY-MM-DD HH:MM:SS". That also makes it human
-- readable with command line sql tools.
--
-- All times stored in the database are in UTC (Greenwich time zone).

CREATE TABLE Song
(ID              INTEGER NOT NULL,
 File_Name       TEXT,
 Category        TEXT,
 Artist          TEXT,
 Album_Artist    TEXT,
 Composer        TEXT,
 Album           TEXT,
 Year            INTEGER,
 Title           TEXT,
 Track           INTEGER,
 
 -- We'd prefer CHAR[19] for these two, since they are actually fixed
 -- length, but Kotlin Room ksp can't handle that in the Android app.
 -- SQLite maps CHAR[19] to TEXT anyway, so this doesn't lose anything
 -- in the actual implementation.
 Last_Downloaded TEXT,
 Prev_Downloaded TEXT,
 
 Play_Before     INTEGER,
 Play_After      INTEGER,
 PRIMARY KEY (ID));
               
CREATE INDEX File_Name on Song (File_Name);
CREATE INDEX Artist on Song (Artist);
CREATE INDEX Album  on Song (Album);
CREATE INDEX Title  on Song (Title);
CREATE INDEX Last_Downloaded on Song (Last_Downloaded);
CREATE UNIQUE INDEX Song_Name on Song (Album_Artist, Album, Title)
-- end of file
