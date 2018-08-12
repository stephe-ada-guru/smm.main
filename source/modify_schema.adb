--  Abstract :
--
--  Modify the schema of the database.
--
--  To add a new field:
--
--  1. Edit create_schema.sql
--  2. Edit smm-database.ad? Insert, Update
--     do _not_ add new field to Fields, Cursor functions yet
--  3. Edit this file to copy old fields, add new
--  4. create smm_new.db
--  5. run modify_schema.exe c:/home/stephe/smm/smm_server.config smm_new.db
--  6. Edit smm-database.ad? Fields, Cursor function
--  7. edit rest of smm to handle new field
--     smm-update.adb, smm-import.adb
--  8. mv smm_new.db c:/home/stephe/smm/smm.db
--  9. make install
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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Config_Files;
with SMM.Database;
with SMM.ID3;
procedure Modify_Schema
is
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   procedure Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("modify_schema <old db server config file> <new db file name>");
   end Usage;

   Server_Config : SAL.Config_Files.Configuration_Type;

   Old_DB : SMM.Database.Database;
   New_DB : SMM.Database.Database;

begin
   declare
      use Ada.Command_Line;
      use SAL.Config_Files;
   begin
      if Argument_Count /= 2 then
         Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      SAL.Config_Files.Open
        (Server_Config, Argument (1),
         Duplicate_Key         => SAL.Config_Files.Raise_Exception,
         Read_Only             => True,
         Case_Insensitive_Keys => True);

      Old_DB.Open (Read (Server_Config, "DB_Filename", Missing_Key => Raise_Exception));
      New_DB.Open (Argument (2));
   end;

   declare
      use Ada.Strings.Unbounded;
      use SMM;
      use SMM.Database;
      use SAL.Config_Files;

      Root_Dir : constant String := As_Directory (Read (Server_Config, SMM.Root_Key));

      I          : Cursor  := Old_DB.First;
      Warm_Fuzzy : Integer := 0;
   begin
      loop
         exit when not I.Has_Element;
         declare
            use SMM.ID3;
            File_Name  : constant String := I.File_Name;
            File       : SMM.ID3.File;
            ID3_Frames : Frame_Lists.List;
         begin
            File.Open (Root_Dir & File_Name);

            ID3_Frames := File.All_Frames;

            New_DB.Insert
              (ID              => I.ID,
               File_Name       => I.File_Name,
               Category        => I.Category,
               Artist          => I.Artist,
               Album           => I.Album,
               Album_Artist    => -Find (SMM.ID3.Alt_Artist, ID3_Frames),
               Title           => I.Title,
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
            Ada.Text_IO.Put_Line (File_Name & ": exception");
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
