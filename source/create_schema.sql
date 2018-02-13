-- SQLite3 syntax

-- SQLite3 and/or GNATCOLL GPL 2016 has a bug when comparing DATETIME
-- fields to bound parameters of type Ada.Calendar.Time; it seems to
-- give random results. So we use a CHAR[19] type for *_Downloaded
-- fields, with format "YYYY-MM-DD HH:MM:SS". That also makes it human
-- readable with command line sql tools.
--
-- All times stored in the database are in UTC (Greenwich time zone).

CREATE TABLE Song
(ID              INTEGER PRIMARY KEY,
 File_Name       TEXT,
 Category        TEXT,
 Artist          TEXT,  
 Album           TEXT,   
 Title           TEXT,   
 Last_Downloaded CHAR[19],
 Prev_Downloaded CHAR[19],
 Play_Before     INTEGER,
 Play_After      INTEGER);
               
CREATE INDEX File_Name on Song (File_Name);
CREATE INDEX Artist on Song (Artist);
CREATE INDEX Album  on Song (Album);
CREATE INDEX Title  on Song (Title);

-- end of file
