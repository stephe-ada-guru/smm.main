--  Abstract :
--
--  See spec
--
--  Copyright (C) 2009, 2011, 2012, 2013 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Directories;
with Ada.Text_IO;
with SAL.AUnit;
with SAL.Config_Files;
with SAL.Time_Conversions;
with SMM;
package body Test_Least_Recent is

   procedure Cleanup
   is begin
      Ada.Directories.Delete_Tree ("tmp");
   exception
   when Ada.Text_IO.Name_Error =>
      --  already deleted
      null;
   end Cleanup;

   procedure Create_Test_Db (Db : out SAL.Config_Files.Configuration_Type)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      Db_File : File_Type;
      Db_File_Name : constant String := "tmp/smm.db";
   begin
      Cleanup;

      Create_Directory ("tmp");

      Create (Db_File, Out_File, Db_File_Name);

      Put_Line (Db_File, "Root = c:/Projects/smm/");
      Put_Line (Db_File, "Songs. 1.File = I1.mp3");
      Put_Line (Db_File, "Songs. 1.Last_Downloaded = 1.0");
      Put_Line (Db_File, "Songs. 1.Category = instrumental");
      Put_Line (Db_File, "Songs. 2.File = I2.mp3");
      Put_Line (Db_File, "Songs. 2.Last_Downloaded = 1.0");
      Put_Line (Db_File, "Songs. 2.Category = instrumental");
      Put_Line (Db_File, "Songs. 3.File = I3.mp3");
      Put_Line (Db_File, "Songs. 3.Last_Downloaded = 1.0");
      Put_Line (Db_File, "Songs. 3.Category = instrumental");
      Put_Line (Db_File, "Songs. 4.File = I4.mp3");
      Put_Line (Db_File, "Songs. 4.Last_Downloaded = 0.0");
      Put_Line (Db_File, "Songs. 4.Category = instrumental");
      Put_Line (Db_File, "Songs. 5.File = I5.mp3");
      Put_Line (Db_File, "Songs. 5.Last_Downloaded = 0.0");
      Put_Line (Db_File, "Songs. 5.Category = instrumental");
      Put_Line (Db_File, "Songs. 6.File = I6.mp3");
      Put_Line (Db_File, "Songs. 6.Last_Downloaded = 0.0");
      Put_Line (Db_File, "Songs. 6.Category = instrumental");
      Put_Line (Db_File, "Songs. 7.File = I7.mp3");
      Put_Line (Db_File, "Songs. 7.Last_Downloaded = 0.0");
      Put_Line (Db_File, "Songs. 7.Category = instrumental");
      Put_Line (Db_File, "Songs. 8.File = V2.mp3");
      Put_Line (Db_File, "Songs. 8.Last_Downloaded = 1.0");
      Put_Line (Db_File, "Songs. 9.File = V3.mp3");
      Put_Line (Db_File, "Songs. 9.Last_Downloaded = 1.0");
      Put_Line (Db_File, "Songs. 10.File = V4.mp3");
      Put_Line (Db_File, "Songs. 10.Last_Downloaded = 0.0");
      Put_Line (Db_File, "Songs. 11.File = V5.mp3");
      Put_Line (Db_File, "Songs. 11.Last_Downloaded = 0.0");
      Put_Line (Db_File, "Songs. 12.File = V6.mp3");
      Put_Line (Db_File, "Songs. 12.Last_Downloaded = 0.0");
      Close (Db_File);

      SAL.Config_Files.Open
        (Db,
         Db_File_Name,
         Missing_File  => SAL.Config_Files.Raise_Exception,
         Duplicate_Key => SAL.Config_Files.Raise_Exception,
         Read_Only     => False);
   end Create_Test_Db;

   procedure Check
     (Label    : in String;
      Db       : in SAL.Config_Files.Configuration_Type;
      Computed : in SMM.Song_Lists.Iterator_Type;
      Expected : in String)
   is begin
      AUnit.Assertions.Assert (not SMM.Song_Lists.Is_Null (Computed), Label & " null iterator");
      SAL.AUnit.Check (Label, SAL.Config_Files.Read (Db, SMM.Song_Lists.Current (Computed), "file"), Expected);
   end Check;

   procedure Mark_Downloaded
     (Db    : in out SAL.Config_Files.Configuration_Type;
      Songs : in     SMM.Song_Lists.List_Type;
      Time  : in     SAL.Time_Conversions.Time_Type)
   is
      use SMM.Song_Lists;
      I : Iterator_Type := First (Songs);

   begin
      loop
         exit when Is_Null (I);
         SMM.Write_Last_Downloaded (Db, Current (I), Time);
         Next (I);
      end loop;
      SAL.Config_Files.Flush (Db); --  for debugging
   end Mark_Downloaded;

   ----------
   --  Test procedures

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use SAL.AUnit;
      use SMM.Song_Lists;
      Db    : SAL.Config_Files.Configuration_Type;
      Songs : List_Type;
      I     : Iterator_Type;
   begin

      Create_Test_Db (Db);

      --  Songs from not-yet-downloaded block mixed in with others
      SMM.Least_Recent_Songs (Db, "instrumental", Songs, Song_Count => 2, New_Song_Count => 2, Seed => 1);

      Check ("song count", SMM.Count (Songs), 2);

      I := First (Songs);
      Check ("1 1", Db, I, "I4.mp3");
      Next (I);

      Check ("1 2", Db, I, "I1.mp3");

      Mark_Downloaded (Db, Songs, 2.0);

      Finalize (Songs);

      --  Can't get any songs from previous download, but not all from one block
      SMM.Least_Recent_Songs (Db, "instrumental", Songs, Song_Count => 2, New_Song_Count => 2, Seed => 2);

      Check ("song count", SMM.Count (Songs), 2);

      I := First (Songs);

      Check ("2 1", Db, I, "I5.mp3");
      Next (I);

      Check ("2 2", Db, I, "I2.mp3");

   end Nominal;
   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Least_Recent");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Least_Recent;
