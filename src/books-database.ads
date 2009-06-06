--  Abstract :
--
--  Top level interface to the database for Books.
--
--  Copyright (C) 2002 - 2004, 2009 Stephen Leake.  All Rights Reserved.
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
--

with Ada.Finalization;
with GNU.DB.SQLCLI;
with Interfaces;
with SAL.Config_Files;
package Books.Database is

   No_Data     : exception;
   Entry_Error : exception; --  User violated some limit

   type Database (Config : SAL.Config_Files.Configuration_Access_Type) is
     new Ada.Finalization.Limited_Controlled with private;
   type Database_Access is access all Database'class;
   procedure Free (Pointer : in out Database_Access);
   --  We need an access type because Gtk Windows are not limited, and
   --  thus cannot have a limited component.

   procedure Initialize (DB : in out Database);
   --  Connect to database. Gets database name, user name from
   --  DB.Config, which must be Open.
   --
   --  Raises SAL.Config_File_Error for any errors.

   procedure Finalize (DB : in out Database);

   type ID_Type is new Interfaces.Unsigned_32;

   function Image (ID : in ID_Type) return String;
   --  Image has leading zeros to width 5, to allow sorting.

   function Value (ID : in String) return ID_Type;

   ----------
   --  Tables

   type Table (DB : access Database'Class) is abstract new Ada.Finalization.Limited_Controlled with private;
   type Table_Access is access all Table'Class;

   procedure Free (Pointer : in out Table_Access);

   ----------
   --  Classwide Table operations

   procedure Find_All_By_ID (T : in out Table'Class);
   --  Set current find statement to return all records ordered by ID.
   --  Fetch first.

   procedure Next (T : in Table'Class);
   --  Move cursor to next record, according to current find
   --  statement. Fetch data.
   --
   --  Raises Books.Database.No_Data if there is no next.

   ----------
   --  Dispatching Table operations

   procedure Initialize (T : in out Table) is abstract;
   --  Create database access statements, fetch first record.

   procedure Finalize (T : in out Table);
   --  Free all statements. Root version frees common statements.

   procedure Clear_Data (T : in out Table) is abstract;
   --  Erase local data.

private

   type Database (Config : SAL.Config_Files.Configuration_Access_Type)
      is new Ada.Finalization.Limited_Controlled with
   record
      Environment : GNU.DB.SQLCLI.SQLHANDLE;
      Connection  : GNU.DB.SQLCLI.SQLHANDLE;
   end record;

   type String_Access is access String;
   --  We must use an access type for String fields, so that the type
   --  of the field is "access String", which is the type of the
   --  BindCol TargetValue parameter. Note that "access
   --  String_Subtype" is _not_ the same.

   package ID_Binding is new GNU.DB.SQLCLI.UnsignedBinding (ID_Type);
   package Unsigned_16_Binding is new GNU.DB.SQLCLI.UnsignedBinding (Interfaces.Unsigned_16);
   package Unsigned_8_Binding is new GNU.DB.SQLCLI.UnsignedBinding (Interfaces.Unsigned_8);

   type Table (DB : access Database'Class) is abstract new Ada.Finalization.Limited_Controlled with record

      Update_Statement    : GNU.DB.SQLCLI.SQLHANDLE := GNU.DB.SQLCLI.SQL_NULL_HANDLE;
      Insert_Statement    : GNU.DB.SQLCLI.SQLHANDLE := GNU.DB.SQLCLI.SQL_NULL_HANDLE;
      Delete_Statement    : GNU.DB.SQLCLI.SQLHANDLE := GNU.DB.SQLCLI.SQL_NULL_HANDLE;
      All_By_ID_Statement : GNU.DB.SQLCLI.SQLHANDLE := GNU.DB.SQLCLI.SQL_NULL_HANDLE;
      Find_Statement      : GNU.DB.SQLCLI.SQLHANDLE; --  Copy of actual handle; not freed.
      Find_Pattern        : String_Access;
      Find_Pattern_Length : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;

   end record;

   procedure Checked_Execute (Statement : in GNU.DB.SQLCLI.SQLHANDLE);
   --  Execute Statement, catch GNU.DB.SQLCLI.Database_Error, check
   --  error message, convert to Entry_Error if recognized.

end Books.Database;
