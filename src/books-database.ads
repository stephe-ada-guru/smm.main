--  Abstract :
--
--  Top level interface to the database for Books.
--
--  Copyright (C) 2002 - 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Finalization;
with GNATCOLL.SQL.Exec;
with SAL.Config_Files;
package Books.Database is

   No_Data     : exception;
   Null_Field  : exception;
   Entry_Error : exception; --  User violated some limit

   type Database (Config : SAL.Config_Files.Configuration_Access_Type) is
     new Ada.Finalization.Limited_Controlled with private;
   type Database_Access is access all Database'class;
   procedure Free (Pointer : in out Database_Access);
   --  We need an access type because Gtk Windows are not limited, and
   --  thus cannot have a limited component.

   overriding procedure Initialize (DB : in out Database);
   --  Connect to database. Gets database name, user name from
   --  DB.Config, which must be Open.
   --
   --  Raises SAL.Config_File_Error for any errors.

   overriding procedure Finalize (DB : in out Database);

   subtype ID_Type is Integer;

   function Image (ID : in ID_Type) return String;
   --  Image has leading zeros to width 5, to allow sorting.

   ----------
   --  Tables

   type Table (DB : access Database'Class) is abstract new Ada.Finalization.Limited_Controlled with private;
   type Table_Access is access all Table'Class;

   procedure Free (Pointer : in out Table_Access);

   procedure Next (T : in out Table'Class);
   --  Move cursor to next record, according to current find
   --  statement.
   --
   --  Marks data invalid if there is no next.

   function Field
     (T           : in Table;
      Field_Index : in GNATCOLL.SQL.Exec.Field_Index)
     return String;
   --  Raises No_Data if cursor has no row.
   --  Raises Null_Field if field has no data.

   function Valid (T : in Table) return Boolean;
   --  True if current data is valid (Next, Find or Fetch returned a row)
   --
   --  Dispatching for object.method notation

   function Valid_Field
     (T     : in Table;
      Field : in GNATCOLL.SQL.Exec.Field_Index)
     return Boolean;
   --  True if Valid and Field is non-null
   --
   --  Dispatching for object.method notation

private

   type Database (Config : SAL.Config_Files.Configuration_Access_Type)
      is new Ada.Finalization.Limited_Controlled with
   record
      Connection : GNATCOLL.SQL.Exec.Database_Connection; -- Sqlite or whatever; allocated in Initialize
   end record;

   type Table (DB : access Database'Class) is abstract new Ada.Finalization.Limited_Controlled with record

      Cursor : GNATCOLL.SQL.Exec.Forward_Cursor;
      --  Holds result of last Find
   end record;

   procedure Checked_Execute
     (T         : in out Table'Class;
      Statement : in     String;
      Params    : in     GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters);
   --  Execute Statement, catch GNU.DB.SQLCLI.Database_Error, check
   --  error message, convert to Entry_Error if recognized.

   procedure Find
     (T         : in out Table'Class;
      Statement : in     String;
      Params    : in     GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters);
   --  Execute statement, call Next.

end Books.Database;
