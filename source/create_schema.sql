-- SQLite3 syntax

-- SQLite3 and/or GNATCOLL GPL 2016 has a bug when comparing DATETIME
-- fields to bound parameters of type Ada.Calendar.Time; it seems to
-- give random results. So we use a TEXT type for Modified, Deleted,
-- *_Downloaded fields, with format "YYYY-MM-DD HH:MM:SS". That also
-- makes it human readable with command line sql tools.
--
-- We'd prefer CHAR[19] for time fields, since they are actually fixed
-- length, but Kotlin Room ksp can't handle that in the Android app.
-- SQLite maps CHAR[19] to TEXT anyway, so this doesn't lose anything
-- in the actual implementation.
--
-- All times stored in the database are in local time zone.

-- We use a schema version number to guard against operator error in
-- managing database transitions. PRAGMA user_version would be
-- simplest, but GNATCOLL.SQL does not provide a way to return the
-- value from 'pragma user_version'. So we use a table that stores one
-- value.

CREATE TABLE Schema_Version
(ID INTEGER PRIMARY KEY,
 Version INTEGER NOT NULL --  Must match smm-database.ads Schema_Version
);

INSERT INTO Schema_Version (ID, Version) VALUES (1, 2);

CREATE TABLE Song
(ID              INTEGER NOT NULL,
 Modified        TEXT NOT NULL,
 Deleted         TEXT,
 File_Name       TEXT NOT NULL,
 Category        TEXT NOT NULL,
 Artist          TEXT,
 Album_Artist    TEXT NOT NULL,
 Composer        TEXT,
 Album           TEXT,
 Year            INTEGER,
 Title           TEXT NOT NULL,
 Track           INTEGER,
 
 Last_Downloaded TEXT,
 Prev_Downloaded TEXT,
 
 Play_Before     INTEGER,
 Play_After      INTEGER,
 PRIMARY KEY (ID));
               
CREATE UNIQUE INDEX File_Name on Song (File_Name);
CREATE INDEX Artist on Song (Artist);
CREATE INDEX Album  on Song (Album);
CREATE INDEX Title  on Song (Title);
CREATE INDEX Last_Downloaded on Song (Last_Downloaded);
CREATE UNIQUE INDEX Song_Name on Song (Album_Artist, Album, Title)
-- end of file
