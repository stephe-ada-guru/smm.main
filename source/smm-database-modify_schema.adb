--  Abstract :
--
--  Modify the schema of the database.
--
--  Copyright (C) 2018 - 2019, 2025, 2026 Stephen Leake All Rights Reserved.
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
with GNATCOLL.SQL.Exec;
procedure SMM.Database.Modify_Schema
is
   procedure Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("modify_schema <old db file name> <new db file name>");
   end Usage;

   Old_DB : SMM.Database.Database;
   New_DB : SMM.Database.Database;

   use all type GNATCOLL.SQL.Exec.Field_Index;
   Old_ID              : constant GNATCOLL.SQL.Exec.Field_Index := GNATCOLL.SQL.Exec.Field_Index'First;
   --  Old_Modified        : constant GNATCOLL.SQL.Exec.Field_Index := Old_ID + 1;
   --  Old_Deleted         : constant GNATCOLL.SQL.Exec.Field_Index := Old_Modified + 1;
   Old_File_Name       : constant GNATCOLL.SQL.Exec.Field_Index := Old_ID + 1;
   Old_Category        : constant GNATCOLL.SQL.Exec.Field_Index := Old_File_Name + 1;
   Old_Artist          : constant GNATCOLL.SQL.Exec.Field_Index := Old_Category + 1;
   Old_Album_Artist    : constant GNATCOLL.SQL.Exec.Field_Index := Old_Artist + 1;
   Old_Composer        : constant GNATCOLL.SQL.Exec.Field_Index := Old_Album_Artist + 1;
   Old_Album           : constant GNATCOLL.SQL.Exec.Field_Index := Old_Composer + 1;
   Old_Year            : constant GNATCOLL.SQL.Exec.Field_Index := Old_Album + 1;
   Old_Title           : constant GNATCOLL.SQL.Exec.Field_Index := Old_Year + 1;
   Old_Track           : constant GNATCOLL.SQL.Exec.Field_Index := Old_Title + 1;
   Old_Last_Downloaded : constant GNATCOLL.SQL.Exec.Field_Index := Old_Track + 1;
   Old_Prev_Downloaded : constant GNATCOLL.SQL.Exec.Field_Index := Old_Last_Downloaded + 1;
   Old_Play_Before     : constant GNATCOLL.SQL.Exec.Field_Index := Old_Prev_Downloaded + 1;
   Old_Play_After      : constant GNATCOLL.SQL.Exec.Field_Index := Old_Play_Before + 1;

   function Old_Field
     (Position : in SMM.Database.Cursor;
      Field    : in GNATCOLL.SQL.Exec.Field_Index)
     return String
   --  Exception if Field is null
   is begin
      return
        (if Position.Cursor.Is_Null (Field)
         then raise SAL.Programmer_Error with "invalid null field" & GNATCOLL.SQL.Exec.Field_Index'Image (Field)
         else Position.Cursor.Value (Field));
   end Old_Field;

   function Old_Field
     (Position : in SMM.Database.Cursor;
      Field    : in GNATCOLL.SQL.Exec.Field_Index;
      Default  : in String)
     return String
   is begin
      return
        (if Position.Cursor.Is_Null (Field)
         then Default
         else Position.Cursor.Value (Field));
   end Old_Field;

   function Old_Field
     (Position : in SMM.Database.Cursor;
      Field    : in GNATCOLL.SQL.Exec.Field_Index;
      Default  : in Integer)
     return Integer
   is begin
      return
        (if Position.Cursor.Is_Null (Field)
         then Default
         else Integer'Value (Position.Cursor.Value (Field)));
   end Old_Field;

begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count /= 2 then
         Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Old_DB.Open (Argument (1), Expected_Schema => 0);
      New_DB.Open (Argument (2));
   end;

   declare
      I          : Cursor  := Old_DB.First_By_ID;
      Warm_Fuzzy : Integer := 0;

   begin
      loop
         exit when not I.Has_Element;
         begin
            New_DB.Insert
              (ID              => I.ID, -- Always first
               File_Name       => Old_Field (I, Old_File_Name),
               Category        => Old_Field (I, Old_Category),
               Artist          => Old_Field (I, Old_Artist, ""),
               Album           => Old_Field (I, Old_Album, ""),
               Album_Artist    => Old_Field (I, Old_Album_Artist),
               Composer        => Old_Field (I, Old_Composer, ""),
               Title           => Old_Field (I, Old_Title),
               Year            => Old_Field (I, Old_Year, No_Year),
               Track           => Old_Field (I, Old_Track, No_Track),
               Last_Downloaded => Old_Field (I, Old_Last_Downloaded, Default_Time_String),
               Prev_Downloaded => Old_Field (I, Old_Prev_Downloaded, Default_Time_String),
               Play_Before     => Old_Field (I, Old_Play_Before, Null_ID),
               Play_After      => Old_Field (I, Old_Play_After, Null_ID));
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
end SMM.Database.Modify_Schema;
