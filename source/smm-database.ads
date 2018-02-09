--  Abstract :
--
--  Interface to SQLite3 database
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Calendar;
with Ada.Finalization;
with GNATCOLL.SQL.Exec;
package SMM.Database is

   No_Data     : exception;
   Null_Field  : exception;
   Entry_Error : exception; --  User violated some limit or index constraint

   subtype Time_String is String (1 .. 19);
   --  UTC time in 'YYYY-MM-DD HH:MM:SS' format

   Jan_1_1958 : aliased constant Time_String := "1958-01-01 00:00:00";
   --  A time before any valid database Modified time, used for a
   --  default time in various places.

   type Database is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (DB : in out Database);
   --  Disconnect from database.

   procedure Open (DB : in out Database; File_Name : in String);

   procedure Insert
     (DB              : in out Database;
      ID              : in     Integer;
      Category        : in     String;
      Artist          : in     String;
      Album           : in     String;
      Title           : in     String;
      Last_Downloaded : in     Time_String;
      Prev_Downloaded : in     Time_String);

   function UTC_Image (Item : in Ada.Calendar.Time) return Time_String;

private

   type Database is new Ada.Finalization.Limited_Controlled with
   record
      Connection : GNATCOLL.SQL.Exec.Database_Connection;
   end record;

end SMM.Database;
