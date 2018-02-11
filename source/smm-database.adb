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
     (DB        : in out Database;
      Statement : in     String;
      Params    : in     GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
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
     (DB              : in out Database;
      ID              : in     Integer;
      File_Name       : in     String;
      Category        : in     String;
      Artist          : in     String;
      Album           : in     String;
      Title           : in     String;
      Last_Downloaded : in     Time_String;
      Prev_Downloaded : in     Time_String := Jan_1_1958;
      Play_Before     : in     Integer     := Null_ID;
      Play_After      : in     Integer     := Null_ID)
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
      if Prev_Downloaded /= Jan_1_1958 then
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

   function Has_Element (Position : Cursor) return Boolean
   is begin
      return Position.Cursor.Has_Row;
   end Has_Element;

   function First (DB : in Database'Class) return Cursor
   is
      Statement : constant String :=
        "SELECT ID, File_Name, Category, Artist, Album, Title, Last_Downloaded, Prev_Downloaded," &
        " Play_Before, Play_After FROM Song ORDER BY ID";
   begin
      return Result : Cursor do
         GNATCOLL.SQL.Exec.Fetch (Result.Cursor, DB.Connection, Statement);

         if not DB.Connection.Success then
            raise Entry_Error with DB.Connection.Error;
         end if;
      end return;
   end First;

   function Current (Position : in Cursor) return Song_Type
   is
      --  FIXME: debugging
      Play_Before : Integer;
   begin
      if not Position.Cursor.Has_Row then
         raise No_Data;
      end if;
      --  FIXME: debugging
      if Position.Cursor.Is_Null (8) then
         Play_Before := Null_ID;
      else
         Play_Before := Integer'Value (Position.Cursor.Value (8));
      end if;

      return
        (ID              => Integer'Value (Position.Cursor.Value (0)),
         File_Name       => +Position.Cursor.Value (1),
         Category        => +Position.Cursor.Value (2),
         Artist          => +Position.Cursor.Value (3),
         Album           => +Position.Cursor.Value (4),
         Title           => +Position.Cursor.Value (5),
         Last_Downloaded => Position.Cursor.Value (6),
         Prev_Downloaded =>
           (if Position.Cursor.Is_Null (7)
            then Jan_1_1958
            else Position.Cursor.Value (7)),
         Play_Before => Play_Before,
           --  (if Position.Cursor.Is_Null (8)
           --   then Null_ID
           --   else Integer'Value (Position.Cursor.Value (8))),
         Play_After =>
           (if Position.Cursor.Is_Null (9)
            then Null_ID
            else Integer'Value (Position.Cursor.Value (9))));
   end Current;

   procedure Next (Position : in out Cursor)
   is begin
      Position.Cursor.Next;
   end Next;

end SMM.Database;
