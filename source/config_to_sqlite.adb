--  Abstract :
--
--  Convert a music SAL config file to an sqlite3 database.
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Config_Files;
with SAL.Time_Conversions;
with SMM.Database;
with SMM.Metadata;
procedure Config_To_Sqlite
is
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   procedure Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("config_to_sqlite <config db file name> <sql db file name>");
   end Usage;

   Config_Db : SAL.Config_Files.Configuration_Type;
   SQL_Db    : SMM.Database.Database;

   function SAL_To_UTC (SAL_Time : in SAL.Time_Conversions.Time_Type) return SMM.Database.Time_String
   is begin
      return SMM.Database.UTC_Image (SAL.Time_Conversions.To_Calendar_Time (SAL_Time));
   end SAL_To_UTC;

begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count /= 2 then
         Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      SAL.Config_Files.Open
        (Config_Db, Argument (1),
         Duplicate_Key         => SAL.Config_Files.Raise_Exception,
         Read_Only             => True,
         Case_Insensitive_Keys => True);

      SMM.Database.Open (SQL_Db, Argument (2));
   end;

   declare
      use Ada.Strings.Unbounded;
      use SMM;
      use SAL.Config_Files;

      Music_Root : constant String := Read (Config_Db, Root_Key);

      I : Iterator_Type := First (Config_Db, Songs_Key);
   begin
      loop
         exit when Is_Null (I);
         declare
            File_Name : constant String := Music_Root & Read (Config_Db, I, File_Key);
            Artist    : Unbounded_String;
            Album     : Unbounded_String;
            Title     : Unbounded_String;
         begin
            SMM.Metadata.Read_Meta (File_Name, Artist, Album, Title);

            SQL_Db.Insert
              (ID              => Integer'Value (Current (I)),
               Category        => Read (Config_Db, I, Category_Key),
               Artist          => -Artist,
               Album           => -Album,
               Title           => -Title,
               Last_Downloaded => SAL_To_UTC (Read_Last_Downloaded (Config_Db, I)),
               Prev_Downloaded => SAL_To_UTC (Read_Prev_Downloaded (Config_Db, I)));

            Next (I);
         end;
         Next (I);
      end loop;
   end;
exception
when E : others =>
   declare
      use Ada.Text_IO;
      use Ada.Exceptions;
      use GNAT.Traceback.Symbolic;
   begin
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Symbolic_Traceback (E));
   end;
end Config_To_Sqlite;
