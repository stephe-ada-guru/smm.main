--  Abstract :
--
--  See spec.
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

with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with GNATCOLL.SQL.Sqlite;
package body SMM.Database is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   procedure Checked_Execute
     (DB        : in Database'Class;
      Statement : in String;
      Params    : in GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
   is begin
      GNATCOLL.SQL.Exec.Execute (DB.Connection, Statement, Params);

      if DB.Connection.Success then
         GNATCOLL.SQL.Exec.Commit (DB.Connection);
      else
         declare
            Msg : constant String := DB.Connection.Error;
         begin
            GNATCOLL.SQL.Exec.Rollback (DB.Connection);

            raise Entry_Error with Msg;
         end;
      end if;
   end Checked_Execute;

   function Checked_Fetch
     (DB        : in Database'Class;
      Statement : in String;
      Params    : in GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
     return Cursor
   is begin
      return Result : Cursor do
         GNATCOLL.SQL.Exec.Fetch (Result.Cursor, DB.Connection, Statement, Params);

         if not DB.Connection.Success then
            raise Entry_Error with DB.Connection.Error;
         end if;
      end return;
   end Checked_Fetch;

   ----------
   --  Public subprograms

   overriding procedure Finalize (DB : in out Database)
   is
      use type GNATCOLL.SQL.Exec.Database_Connection;
   begin
      if DB.Connection = null then
         null;
      else
         GNATCOLL.SQL.Exec.Free (DB.Connection);
      end if;
   exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Database disconnect: exception " & Ada.Exceptions.Exception_Message (E));
   end Finalize;

   procedure Open (DB : in out Database; File_Name : in String)
   is
      use GNATCOLL.SQL.Exec;
   begin
      if not Ada.Directories.Exists (File_Name) then
         raise SAL.Config_File_Error with File_Name & " does not exist";
      end if;

      DB.Connection := GNATCOLL.SQL.Exec.Build_Connection (GNATCOLL.SQL.Sqlite.Setup (File_Name));

      if not DB.Connection.Success then
         raise SAL.Config_File_Error with File_Name & DB.Connection.Error;
      end if;
   end Open;

   procedure Insert
     (DB              : in Database;
      ID              : in Integer;
      File_Name       : in String;
      Category        : in String;
      Artist          : in String;
      Album           : in String;
      Title           : in String;
      Last_Downloaded : in Time_String;
      Prev_Downloaded : in Time_String := Default_Time_String;
      Play_Before     : in Integer     := Null_ID;
      Play_After      : in Integer     := Null_ID)
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.SQL.Exec;

      Statement : Unbounded_String :=
        +"INSERT INTO Song (ID, File_Name, Category, Artist, Album, Title, Last_Downloaded";

      Values : Unbounded_String := +"VALUES (?,?,?,?,?,?,?";

      Params : SQL_Parameters (1 .. 10) :=
        (+ID, +File_Name, +Category, +Artist, +Album, +Title, +Last_Downloaded, others => Null_Parameter);

      Last : Integer := 7;
   begin
      if Prev_Downloaded /= Default_Time_String then
         Last          := Last + 1;
         Statement     := Statement & ", Prev_Downloaded";
         Values        := Values & ",?";
         Params (Last) := +Prev_Downloaded;
      end if;

      if Play_Before /= Null_ID then
         Last          := Last + 1;
         Statement     := Statement & ", Play_Before";
         Values        := Values & ",?";
         Params (Last) := +Play_Before;
      end if;

      if Play_After /= Null_ID then
         Last          := Last + 1;
         Statement     := Statement & ", Play_After";
         Values        := Values & ",?";
         Params (Last) := +Play_After;
      end if;

      Statement := Statement & ") " & Values & ")";

      Checked_Execute (DB, -Statement, Params (1 .. Last));
   end Insert;

   function UTC_Image (Item : in Ada.Calendar.Time) return Time_String
   is
      --  GNAT GPL 2016 Clock returns UTC
   begin
      return Ada.Calendar.Formatting.Image (Item);
   end UTC_Image;

   ----------
   --  Iterators

   All_Fields : constant String :=
     "ID, File_Name, Category, Artist, Album, Title, Last_Downloaded, Prev_Downloaded, Play_Before, Play_After";

   --  Field indices
   use all type GNATCOLL.SQL.Exec.Field_Index;
   ID_Field              : constant GNATCOLL.SQL.Exec.Field_Index := GNATCOLL.SQL.Exec.Field_Index'First;
   File_Name_Field       : constant GNATCOLL.SQL.Exec.Field_Index := ID_Field + 1;
   Category_Field        : constant GNATCOLL.SQL.Exec.Field_Index := File_Name_Field + 1;
   Artist_Field          : constant GNATCOLL.SQL.Exec.Field_Index := Category_Field + 1;
   Album_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Artist_Field + 1;
   Title_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Album_Field + 1;
   Last_Downloaded_Field : constant GNATCOLL.SQL.Exec.Field_Index := Title_Field + 1;
   Prev_Downloaded_Field : constant GNATCOLL.SQL.Exec.Field_Index := Last_Downloaded_Field + 1;
   Play_Before_Field     : constant GNATCOLL.SQL.Exec.Field_Index := Prev_Downloaded_Field + 1;
   Play_After_Field      : constant GNATCOLL.SQL.Exec.Field_Index := Play_Before_Field + 1;

   function Has_Element (Position : Cursor) return Boolean
   is begin
      return Position.Cursor.Has_Row;
   end Has_Element;

   function First (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT " & All_Fields & " FROM Song ORDER BY ID";
   begin
      return Checked_Fetch (DB, Statement);
   end First;

   function Find_File_Name (DB : in Database'Class; File_Name : in String) return Cursor
   is
      use GNATCOLL.SQL.Exec;
      Statement : constant String := "SELECT " & All_Fields & " FROM Song WHERE File_Name = ?";
   begin
      return Checked_Fetch (DB, Statement, Params => (1 => +File_Name));
   end Find_File_Name;

   function Find_ID (DB : in Database'Class; ID : in Integer) return Cursor
   is
      use GNATCOLL.SQL.Exec;
      Statement : constant String := "SELECT " & All_Fields & " FROM Song WHERE ID = ?";
   begin
      return Checked_Fetch (DB, Statement, Params => (1 => +ID));
   end Find_ID;

   procedure Next (Position : in out Cursor)
   is begin
      Position.Cursor.Next;
   end Next;

   function Element (Position : in Cursor) return Song_Type
   is begin
      if not Position.Cursor.Has_Row then
         raise No_Data;
      end if;

      return
        (ID              => Integer'Value (Position.Cursor.Value (ID_Field)),
         File_Name       => +Position.Cursor.Value (File_Name_Field),
         Category        => +Position.Cursor.Value (Category_Field),
         Artist          => +Position.Cursor.Value (Artist_Field),
         Album           => +Position.Cursor.Value (Album_Field),
         Title           => +Position.Cursor.Value (Title_Field),
         Last_Downloaded =>
           (if Position.Cursor.Is_Null (Last_Downloaded_Field)
            then Default_Time_String
            else Position.Cursor.Value (Last_Downloaded_Field)),
         Prev_Downloaded =>
           (if Position.Cursor.Is_Null (Prev_Downloaded_Field)
            then Default_Time_String
            else Position.Cursor.Value (Prev_Downloaded_Field)),
         Play_Before =>
           (if Position.Cursor.Is_Null (Play_Before_Field)
            then Null_ID
            else Integer'Value (Position.Cursor.Value (Play_Before_Field))),
         Play_After =>
           (if Position.Cursor.Is_Null (Play_After_Field)
            then Null_ID
            else Integer'Value (Position.Cursor.Value (Play_After_Field))));
   end Element;

   function ID (Position : in Cursor) return Integer
   is begin
      return Integer'Value (Position.Cursor.Value (ID_Field));
   end ID;

   function File_Name (Position : in Cursor) return String
   is begin
      return Position.Cursor.Value (File_Name_Field);
   end File_Name;

   function Category (Position : in Cursor) return String
   is begin
      return Position.Cursor.Value (Category_Field);
   end Category;

   function Last_Downloaded (Position : in Cursor) return Time_String
   is begin
      return
        (if Position.Cursor.Is_Null (Last_Downloaded_Field)
         then Default_Time_String
         else Position.Cursor.Value (Last_Downloaded_Field));
   end Last_Downloaded;

   function Prev_Downloaded (Position : in Cursor) return Time_String
   is begin
      return
        (if Position.Cursor.Is_Null (Prev_Downloaded_Field)
         then Default_Time_String
         else Position.Cursor.Value (Prev_Downloaded_Field));
   end Prev_Downloaded;

   function Play_Before (Position : in Cursor) return Integer
   is begin
      return
        (if Position.Cursor.Is_Null (Play_Before_Field)
         then Null_ID
         else Integer'Value (Position.Cursor.Value (Play_Before_Field)));
   end Play_Before;

   function Play_After_Is_Present (Position : in Cursor) return Boolean
   is begin
      return not Position.Cursor.Is_Null (Play_After_Field);
   end Play_After_Is_Present;

   function Play_Before_Is_Present (Position : in Cursor) return Boolean
   is begin
      return not Position.Cursor.Is_Null (Play_Before_Field);
   end Play_Before_Is_Present;

   procedure Write_Last_Downloaded
     (Position : in Cursor;
      DB       : in Database'Class;
      Time     : in Time_String)
   is
      use GNATCOLL.SQL.Exec;
   begin
      Checked_Execute
        (DB,
         Statement => "UPDATE Song SET Last_Downloaded = ?, Prev_Downloaded = ? WHERE ID =?",
         Params    => (+Time, +Position.Last_Downloaded, +Position.ID));
   end Write_Last_Downloaded;

end SMM.Database;
