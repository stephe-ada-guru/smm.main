--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2012 - 2013, 2015 - 2016 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO;
with AUnit.Checks.Text_IO;
with SAL.Config_Files;
with SMM.Download;
with Test_Utils; use Test_Utils;
package body Test_Download is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;

      pragma Unreferenced (T);

      Start_Dir : constant String := Current_Directory;

      Db_File_Name : constant String := "tmp/smm.db";

      Db_File  : File_Type;
      Playlist : File_Type;

      Db : SAL.Config_Files.Configuration_Type;
   begin
      --  Create the test environment; a db with some new, some
      --  already played songs; an existing playlist and corresponding
      --  files.

      Cleanup;

      Create_Directory ("tmp");

      Create (Db_File, Out_File, Db_File_Name);

      --  We will download 'vocal'; put one instrumental in db with
      --  oldest time, to show it is excluded.
      --
      --  We will download 6 files; 3 new, 3 least recently
      --  played. We have more than 4 new, more than 2 played in db.
      --  Some already played files are in target dir from previous
      --  download.
      --
      --  Least_Recent_Songs randomizes 2 * Song_Count songs, 12 here.
      --  So we need that many new and less recently played in the db
      --  than songs currently in target dir.
      --
      --  Category defaults to vocal
      Put_Line (Db_File, "Root = " & SMM.As_Directory (Current_Directory & "/tmp/source"));
      Put_Line (Db_File, "Songs. 1.File = artist_1/excluded_1.mp3");
      Put_Line (Db_File, "Songs. 1.Last_Downloaded = 1.0");
      Put_Line (Db_File, "Songs. 1.Category = instrumental");
      Put_Line (Db_File, "Songs. 2.File = artist_1/played_1.mp3");
      Put_Line (Db_File, "Songs. 2.Last_Downloaded = 2.0");
      Put_Line (Db_File, "Songs. 3.File = artist_1/played_2.mp3");
      Put_Line (Db_File, "Songs. 3.Last_Downloaded = 2.0");
      Put_Line (Db_File, "Songs. 4.File = artist_1/new_1.mp3");
      Put_Line (Db_File, "Songs. 4.Last_Downloaded = 0.0"); -- new
      Put_Line (Db_File, "Songs. 5.File = artist_1/new_2.mp3");
      Put_Line (Db_File, "Songs. 5.Last_Downloaded = 0.0"); -- new
      Put_Line (Db_File, "Songs. 6.File = artist_1/in_target_1.mp3");
      Put_Line (Db_File, "Songs. 6.Last_Downloaded = 3.0");
      Put_Line (Db_File, "Songs. 7.File = artist_1/in_target_2.mp3");
      Put_Line (Db_File, "Songs. 7.Last_Downloaded = 3.0");
      Put_Line (Db_File, "Songs. 8.File = artist_2/in_target_3.mp3");
      Put_Line (Db_File, "Songs. 8.Last_Downloaded = 3.0");
      Put_Line (Db_File, "Songs. 9.File = artist_2/new_3.mp3");
      Put_Line (Db_File, "Songs. 9.Last_Downloaded = 0.0"); -- new
      Put_Line (Db_File, "Songs. 10.File = artist_2/new_4.mp3");
      Put_Line (Db_File, "Songs. 10.Last_Downloaded = 0.0"); -- new
      Put_Line (Db_File, "Songs. 11.File = artist_2/played_3.mp3");
      Put_Line (Db_File, "Songs. 11.Last_Downloaded = 2.0");
      Put_Line (Db_File, "Songs. 12.File = artist_2/played_4.mp3");
      Put_Line (Db_File, "Songs. 12.Last_Downloaded = 2.0");
      Put_Line (Db_File, "Songs. 13.File = artist_2/played_5.mp3");
      Put_Line (Db_File, "Songs. 13.Last_Downloaded = 2.5");
      Put_Line (Db_File, "Songs. 14.File = artist_2/played_6.mp3");
      Put_Line (Db_File, "Songs. 14.Last_Downloaded = 2.5");
      Put_Line (Db_File, "Songs. 15.File = artist_2/played_7.mp3");
      Put_Line (Db_File, "Songs. 15.Last_Downloaded = 2.5");
      Put_Line (Db_File, "Songs. 16.File = artist_2/played_8.mp3");
      Put_Line (Db_File, "Songs. 16.Last_Downloaded = 2.5");
      Put_Line (Db_File, "Songs. 17.File = artist_3/new_5.mp3");
      Put_Line (Db_File, "Songs. 17.Last_Downloaded = 0.0"); -- new
      Put_Line (Db_File, "Songs. 18.File = artist_3/new_6.mp3");
      Put_Line (Db_File, "Songs. 18.Last_Downloaded = 0.0"); -- new
      Close (Db_File);


      Create_Directory ("tmp/source");
      Create_Directory ("tmp/source/artist_1");
      Create_Test_File ("tmp/source/artist_1/artist_1.jpg");
      Create_Test_File ("tmp/source/artist_1/excluded_1.mp3");
      Create_Test_File ("tmp/source/artist_1/played_1.mp3");
      Create_Test_File ("tmp/source/artist_1/played_2.mp3");
      Create_Test_File ("tmp/source/artist_1/new_1.mp3");
      Create_Test_File ("tmp/source/artist_1/new_2.mp3");
      Create_Test_File ("tmp/source/artist_1/in_target_1.mp3");
      Create_Test_File ("tmp/source/artist_1/in_target_2.mp3");
      Create_Directory ("tmp/source/artist_2");
      Create_Test_File ("tmp/source/artist_2/artist_2.jpg");
      Create_Test_File ("tmp/source/artist_2/in_target_3.mp3");
      Create_Test_File ("tmp/source/artist_2/new_3.mp3");
      Create_Test_File ("tmp/source/artist_2/new_4.mp3");
      Create_Test_File ("tmp/source/artist_2/played_3.mp3");
      Create_Test_File ("tmp/source/artist_2/played_4.mp3");
      Create_Test_File ("tmp/source/artist_2/played_5.mp3");
      Create_Test_File ("tmp/source/artist_2/played_6.mp3");
      Create_Test_File ("tmp/source/artist_2/played_7.mp3");
      Create_Test_File ("tmp/source/artist_2/played_8.mp3");

      --  Album art is downloaded when a new target directory is created.
      Create_Directory ("tmp/source/artist_3");
      Create_Test_File ("tmp/source/artist_3/artist_3.jpg");
      Create_Test_File ("tmp/source/artist_3/new_5.mp3");
      Create_Test_File ("tmp/source/artist_3/new_6.mp3");

      Create_Directory ("tmp/target");
      Create_Directory ("tmp/target/vocal");
      Create_Directory ("tmp/target/vocal/artist_1");
      Create_Test_File ("tmp/target/vocal/artist_1/in_target_1.mp3");
      Create_Test_File ("tmp/target/vocal/artist_1/in_target_2.mp3");
      Create_Directory ("tmp/target/vocal/artist_2");
      Create_Test_File ("tmp/target/vocal/artist_2/in_target_3.mp3");

      Create (Playlist, Out_File, "tmp/target/vocal.m3u");
      Put_Line (Playlist, "vocal/artist_1/in_target_1.mp3");
      Put_Line (Playlist, "vocal/artist_1/in_target_2.mp3");
      Put_Line (Playlist, "vocal/artist_2/in_target_3.mp3");
      Close (Playlist);

      SAL.Config_Files.Open
        (Db,
         Db_File_Name,
         Duplicate_Key         => SAL.Config_Files.Raise_Exception,
         Read_Only             => False,
         Case_Insensitive_Keys => True);

      SMM.Download
        (Db,
         Category       => "vocal",
         Destination    => SMM.As_Directory (Current_Directory & "/tmp/target"),
         Song_Count     => 6,
         New_Song_Count => 8,
         Seed           => 1);

      Set_Directory (Start_Dir);

      SAL.Config_Files.Close (Db);

      Open (Playlist, In_File, "tmp/target/vocal.m3u");
      --  Previous songs
      Check (Playlist, "vocal/artist_1/in_target_1.mp3");
      Check (Playlist, "vocal/artist_1/in_target_2.mp3");
      Check (Playlist, "vocal/artist_2/in_target_3.mp3");
      --  New songs and less recent songs in random order - choice and order can
      --  change with compiler version. This is correct for GNAT GPL
      --  2014. Possible songs here are all except 6, 7, 8
      Check (Playlist, "vocal/artist_2/played_4.mp3");
      Check (Playlist, "vocal/artist_2/played_8.mp3");
      Check (Playlist, "vocal/artist_1/new_2.mp3");
      Check (Playlist, "vocal/artist_2/played_7.mp3");
      Check (Playlist, "vocal/artist_1/new_1.mp3");
      Check (Playlist, "vocal/artist_3/new_5.mp3");

      Check_End (Playlist);
      Close (Playlist);

      Check_File_Count ("tmp/target/vocal/artist_1/", 4);
      Check_File_Exists ("tmp/target/vocal/artist_1/in_target_1.mp3");
      Check_File_Exists ("tmp/target/vocal/artist_1/in_target_2.mp3");
      Check_File_Exists ("tmp/target/vocal/artist_1/new_1.mp3");
      Check_File_Exists ("tmp/target/vocal/artist_1/new_2.mp3");

      Check_File_Count ("tmp/target/vocal/artist_2/", 4);
      Check_File_Exists ("tmp/target/vocal/artist_2/in_target_3.mp3");
      Check_File_Exists ("tmp/target/vocal/artist_2/played_4.mp3");
      Check_File_Exists ("tmp/target/vocal/artist_2/played_7.mp3");
      Check_File_Exists ("tmp/target/vocal/artist_2/played_8.mp3");

      Check_File_Count ("tmp/target/vocal/artist_3/", 2);
      Check_File_Exists ("tmp/target/vocal/artist_3/artist_3.jpg");
      Check_File_Exists ("tmp/target/vocal/artist_3/new_5.mp3");
   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../test/test_download.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      SMM.Verbosity := T.Verbosity;
   end Set_Up_Case;

end Test_Download;
