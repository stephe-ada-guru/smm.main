--  Abstract :
--
--  See spec
--
--  Copyright (C) 2009, 2011, 2012, 2013, 2015, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Calendar;
with Ada.Directories;
with SMM.Database;
with SMM.Song_Lists;
with Test_Utils;
package body Test_Least_Recent is

   procedure Create_Test_DB (DB : in out SMM.Database.Database)
   is
      use Ada.Directories;
      use SMM.Database;
      use Test_Utils;

      DB_File_Name : constant String := "tmp/smm.db";

   begin
      Cleanup;

      Create_Directory ("tmp");

      Test_Utils.Create_Empty_DB (DB_File_Name);

      Open (DB, DB_File_Name);

      Insert (DB, 1, "I1.mp3", 1.0, "instrumental");
      Insert (DB, 2, "I2.mp3", 1.0, "instrumental");
      Insert (DB, 3, "I3.mp3", 1.0, "instrumental");
      Insert (DB, 4, "I4.mp3", 0.0, "instrumental");
      Insert (DB, 5, "I5.mp3", 0.0, "instrumental");
      Insert (DB, 6, "I6.mp3", 0.0, "instrumental");
      Insert (DB, 7, "I7.mp3", 0.0, "instrumental");
      Insert (DB, 8, "V2.mp3", 1.0);
      Insert (DB, 9, "V3.mp3", 1.0);
      Insert (DB, 10, "V4.mp3", 0.0);
      Insert (DB, 11, "V5.mp3", 0.0);
      Insert (DB, 12, "V6.mp3", 0.0);
   end Create_Test_DB;

   procedure Check
     (Label    : in String;
      Computed : in SMM.Song_Lists.Song_Lists.Cursor;
      Expected : in String;
      DB       : in SMM.Database.Database)
   is
      use SMM.Database;
      use SMM.Song_Lists.Song_Lists;
      use AUnit.Checks;
   begin
      Check (Label & ".list has_element", Has_Element (Computed), True);
      Check (Label & ".not null_id", Element (Computed) /= Null_ID, True);
      Check (Label & ".file_name", Find_ID (DB, Element (Computed)).File_Name, Expected);
   end Check;

   procedure Mark_Downloaded
     (DB    : in     SMM.Database.Database;
      Songs : in out SMM.Song_Lists.Song_Lists.List;
      Time  : in     Duration)
   is
      use SMM.Database;
      use SMM.Song_Lists.Song_Lists;
      I : SMM.Song_Lists.Song_Lists.Cursor;
   begin
      loop
         I := First (Songs);
         exit when I = No_Element;
         Find_ID (DB, Element (I)).Write_Last_Downloaded (DB, UTC_Image (Ada.Calendar.Time_Of (1958, 1, 1, Time)));
         Songs.Delete_First;
      end loop;
   end Mark_Downloaded;

   ----------
   --  Test procedures

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use SMM.Song_Lists.Song_Lists;

      DB    : SMM.Database.Database;
      Songs : List;
      I     : Cursor;
   begin
      Create_Test_DB (DB);

      SMM.Song_Lists.Least_Recent_Songs
        (DB, "instrumental", Songs,
         Song_Count        => 2,
         New_Song_Count    => 2,
         Over_Select_Ratio => 2.0,
         Seed              => 1);

      Check ("song count", Integer (Songs.Length), 2);

      --  Results depend on random number generator, which can change
      --  with compiler version. These are correct for GNAT GPL 2014.
      --  Possible results are any song I1 .. I7.
      I := First (Songs);
      Check ("1 1", I, "I2.mp3", DB);
      Next (I);

      Check ("1 2", I, "I3.mp3", DB);

      Mark_Downloaded (DB, Songs, 2.0);

      SMM.Song_Lists.Least_Recent_Songs
        (DB, "instrumental", Songs,
         Song_Count => 2, New_Song_Count => 2, Over_Select_Ratio => 2.0, Seed => 2);

      Check ("song count", Integer (Songs.Length), 2);

      --  Possible results are two from I1 .. I7, excluding the two
      --  from first call.
      I := First (Songs);
      Check ("2 1", I, "I2.mp3", DB);
      Next (I);

      Check ("2 2", I, "I4.mp3", DB);
   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_least_recent.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Least_Recent;
