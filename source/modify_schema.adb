--  Abstract :
--
--  Modify the schema of the database.
--
--  Copyright (C) 2018 - 2019, 2025 Stephen Leake All Rights Reserved.
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
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SMM.Database;
procedure Modify_Schema
is
   procedure Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("modify_schema <old db file name> <new db file name>");
   end Usage;

   Old_DB : SMM.Database.Database;
   New_DB : SMM.Database.Database;

begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count /= 2 then
         Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Old_DB.Open (Argument (1));
      New_DB.Open (Argument (2));
   end;

   declare
      use SMM;
      use SMM.Database;

      I          : Cursor  := Old_DB.First_By_ID;
      Warm_Fuzzy : Integer := 0;

   begin
      loop
         exit when not I.Has_Element;
         begin
            New_DB.Insert
              (ID              => I.ID,
               File_Name       => I.File_Name,
               Category        => I.Category,
               Artist          => I.Artist,
               Album           => I.Album,
               Album_Artist    => I.Album_Artist,
               Composer        => I.Composer,
               Title           => I.Title,
               Year            => I.Year,
               Track           => I.Track,
               Last_Downloaded => I.Last_Downloaded,
               Prev_Downloaded => I.Prev_Downloaded,
               Play_Before     => I.Play_Before,
               Play_After      => I.Play_After);
         exception
         when E : SMM.Database.Entry_Error =>
            declare
               use Ada.Strings.Fixed;
            begin
               if 0 /= Index
                 (Source => Ada.Exceptions.Exception_Message (E),
                  Pattern => "UNIQUE constraint failed")
               then
                  --  resuming a previously failed convert; ignore
                  null;
               else
                  raise;
               end if;
            end;

         when others =>
            Ada.Text_IO.Put_Line (I.File_Name & ": exception");
            raise;
         end;
         Next (I);

         if 0 = Warm_Fuzzy mod 10_000 then
            Warm_Fuzzy := 0;
            Ada.Text_IO.New_Line;
         elsif 0 = Warm_Fuzzy mod 100 then
            Ada.Text_IO.Put (".");
         end if;
         Warm_Fuzzy := Warm_Fuzzy + 1;
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
end Modify_Schema;
