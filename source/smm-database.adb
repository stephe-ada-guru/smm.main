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
      Category        : in     String;
      Artist          : in     String;
      Album           : in     String;
      Title           : in     String;
      Last_Downloaded : in     Time_String;
      Prev_Downloaded : in     Time_String)
   is
      use GNATCOLL.SQL.Exec;

      Statement : constant String :=
        "INSERT INTO Author (ID, Category, Artist, Album, Title, Last_Downloaded, Prev_Downloaded)" &
        " VALUES (?,?,?,?,?,?,?)";

      Params : constant SQL_Parameters (1 .. 7) :=
        (+ID, +Category, +Artist, +Album, +Title, +Last_Downloaded, +Prev_Downloaded);
   begin
      Checked_Execute (DB, Statement, Params);
   end Insert;

   function UTC_Image (Item : in Ada.Calendar.Time) return Time_String
   is
      --  GNAT GPL 2016 Clock returns UTC
   begin
      return Ada.Calendar.Formatting.Image (Item);
   end UTC_Image;

end SMM.Database;
