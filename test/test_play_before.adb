--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2015, 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Directories;
with AUnit.Checks;
with SMM.Database;
with SMM.Song_Lists;
with Test_Utils; use Test_Utils;
package body Test_Play_Before is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SMM.Song_Lists.Song_Lists;

      DB_File_Name : constant String := "tmp/smm.db";
      DB : SMM.Database.Database;

      Songs  : List;
      Song_I : SMM.Song_Lists.Song_Lists.Cursor;

      procedure Check
        (I        : in out SMM.Song_Lists.Song_Lists.Cursor;
         Expected : in     String)
      is
         use AUnit.Checks;
      begin
         Check ("", DB.Find_ID (Element (I)).File_Name, Expected);
         Next (I);
      end Check;

   begin
      --  Create the test environment; a db with some
      --  Play_Before and Play_After items.

      Cleanup;

      Ada.Directories.Create_Directory ("tmp");

      Test_Utils.Create_Empty_DB (DB_File_Name);

      DB.Open (DB_File_Name);

      Insert (DB, 1, "intro_1.mp3", 2.0);

      DB.Write_Play_Before_After (1, 2);
      --  Sets 1.Play_Before and 2.Play_After

      Insert (DB, 2, "song_1.mp3", 1.0);
      Insert (DB, 3, "song_3.mp3", 3.0);
      Insert (DB, 4, "intro_5.mp3", 4.0);
      DB.Write_Play_Before_After (4, 5);
      Insert (DB, 5, "song_5.mp3", 5.0);

      Songs.Append (1);
      Songs.Append (3);
      Songs.Append (2);
      Songs.Append (4);

      SMM.Song_Lists.Play_Before (DB, Songs);

      Song_I := First (Songs);

      Check (Song_I, "intro_1.mp3");
      Check (Song_I, "song_1.mp3");
      Check (Song_I, "song_3.mp3");
      Check (Song_I, "intro_5.mp3");
      Check (Song_I, "song_5.mp3");
   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Play_Before");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Play_Before;
