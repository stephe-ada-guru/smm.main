--  Abstract :
--
--  See spec
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks;
with Ada.Directories;
with SMM.Database;
with Test_Utils; use Test_Utils;
package body SMM.Database.Test is

   DB_File_Name : constant String := "tmp/smm.db";

   DB : SMM.Database.Database;

   type ID_Array is array (Natural range <>) of Integer;

   procedure Test_Find_Like (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      procedure Check_One
        (Label : in String;
         Param : in Field_Values;
         Expected : in ID_Array)
      is
         use AUnit.Checks;

         I : constant Cursor  := Find_Like (DB, Param);
         Found_Count : Integer := 0;
         J : Integer := Expected'First;
      begin
         loop
            exit when not I.Has_Element or J > Expected'Last;

            Found_Count := Found_Count + 1;

            Check (Label & ".id", I.ID, Expected (J));
            J := J + 1;
         end loop;

         Check (Label & "found_count", Found_Count, Expected'Last);
      end Check_One;

   begin
      Check_One
        ("1",
         (Artist => +"2", Album => +"", Title => +"3"),
         (1 => 6));
   end Test_Find_Like;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("smm-database-test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Like'Access, "Test_Find_Like");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
      use Ada.Directories;
   begin
      Cleanup;

      Create_Directory ("tmp");

      Create_Empty_DB (DB_File_Name);

      DB.Open (DB_File_Name);

      DB.Insert (1, "artist_1/album_1/1 - song_1.mp3", "vocal", "artist 1", "album 1", "1 - song_1");
      DB.Write_Play_Before_After (1, 2);
      DB.Insert (2, "artist_1/album_1/2 - song_2.mp3", "vocal", "artist 1", "album 1", "2 - song_2");
      DB.Insert
        (3, "artist_1/album_1/03 The Dance #1.mp3", "instrumental", "artist 1", "album 1", "03 The Dance #1");
      DB.Insert (4, "artist_2/album_1/1 - song_1.mp3", "vocal", "artist 2", "album 1", "1 - song_1");
      DB.Insert (5, "artist_2/album_1/2 - song_2.mp3", "vocal", "artist 2", "album 1", "2 - song_2");
      DB.Insert (6, "artist_2/album_1/3 - song_3.mp3", "vocal", "artist 2", "album 1", "3 - song_3");
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      DB.Finalize;
   end Tear_Down_Case;

end SMM.Database.Test;
