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
with Ada.Strings.Unbounded;
with GNATCOLL.SQL.Exec;
package SMM.Database is

   No_Data     : exception;
   Null_Field  : exception;
   Entry_Error : exception; --  User violated some limit or index constraint

   subtype Time_String is String (1 .. 19);
   --  UTC time in 'YYYY-MM-DD HH:MM:SS' format

   Jan_1_1958 : constant Time_String := "1958-01-01 00:00:00";
   --  A time before any valid database Modified time, used for a
   --  default time in various places.

   Default_Time_String : Time_String renames Jan_1_1958;

   Null_ID : constant Integer := -1;

   type Database is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (DB : in out Database);
   --  Disconnect from database.

   procedure Open (DB : in out Database; File_Name : in String);

   procedure Insert
     (DB              : in Database;
      ID              : in Integer;
      File_Name       : in String;
      Category        : in String;
      Artist          : in String;
      Album           : in String;
      Title           : in String;
      Last_Downloaded : in Time_String := Default_Time_String;
      Prev_Downloaded : in Time_String := Default_Time_String;
      Play_Before     : in Integer     := Null_ID;
      Play_After      : in Integer     := Null_ID);

   function UTC_Image (Item : in Ada.Calendar.Time) return Time_String;

   ----------
   --  Iterate over db contents, in ID order

   type Song_Type is record
      ID              : Integer;
      File_Name       : Ada.Strings.Unbounded.Unbounded_String;
      Category        : Ada.Strings.Unbounded.Unbounded_String;
      Artist          : Ada.Strings.Unbounded.Unbounded_String;
      Album           : Ada.Strings.Unbounded.Unbounded_String;
      Title           : Ada.Strings.Unbounded.Unbounded_String;
      Last_Downloaded : Time_String;
      Prev_Downloaded : Time_String;
      Play_Before     : Integer;
      Play_After      : Integer;
   end record;

   type Cursor is tagged private;

   function Has_Element (Position : in Cursor) return Boolean;

   function First (DB : in Database'Class) return Cursor;
   --  Increasing ID order.

   function Last (DB : in Database'Class) return Cursor;
   --  Decreasing ID order.

   function Find_File_Name (DB : in Database'Class; File_Name : in String) return Cursor;
   function Find_ID (DB : in Database'Class; ID : in Integer) return Cursor;

   procedure Next (Position : in out Cursor);

   function Element (Position : in Cursor) return Song_Type;

   function ID (Position : in Cursor) return Integer;
   function File_Name (Position : in Cursor) return String;
   function Category (Position : in Cursor) return String;
   function Last_Downloaded (Position : in Cursor) return Time_String;
   function Prev_Downloaded (Position : in Cursor) return Time_String;
   function Play_After (Position : in Cursor) return Integer;
   function Play_Before (Position : in Cursor) return Integer;

   function Play_After_Is_Present (Position : in Cursor) return Boolean;
   function Play_Before_Is_Present (Position : in Cursor) return Boolean;

   procedure Write_Last_Downloaded
     (Position : in Cursor;
      DB       : in Database'Class;
      Time     : in Time_String);

   procedure Write_Play_Before_After
     (DB        : in Database'Class;
      Before_ID : in Integer;
      After_ID  : in Integer);

private

   type Database is new Ada.Finalization.Limited_Controlled with
   record
      Connection : GNATCOLL.SQL.Exec.Database_Connection;
   end record;

   type Cursor is tagged record
      --  FIXME: need finalization to release cursor?
      Cursor : GNATCOLL.SQL.Exec.Forward_Cursor;
   end record;

end SMM.Database;
