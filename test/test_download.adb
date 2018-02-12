--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2012 - 2013, 2015 - 2016, 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks.Text_IO;
with Ada.Directories;
with Ada.Text_IO;
with SMM.Database;
with SMM.Download;
with SMM.ID3;
with Test_Utils; use Test_Utils;
package body Test_Download is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;
      use SMM.ID3;

      pragma Unreferenced (T);

      Start_Dir : constant String := Current_Directory;

      DB_File_Name : constant String := "tmp/smm.db";

      Playlist : File_Type;

      DB : SMM.Database.Database;
   begin
      --  Create the test environment; a db with some new, some
      --  already played songs; an existing playlist and corresponding
      --  files.

      Cleanup;

      Create_Directory ("tmp");

      Create_Empty_DB (DB_File_Name);

      DB.Open (DB_File_Name);

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

      Insert (DB, 1, "artist_1/excluded_1.mp3", 1.0, "instrumental");
      Insert (DB, 2, "artist_1/played_1.mp3", 2.0);
      Insert (DB, 3, "artist_1/played_2.mp3", 2.0);
      Insert (DB, 4, "artist_1/new_1.mp3", 0.0);
      Insert (DB, 5, "artist_1/new_2.mp3", 0.0);
      Insert (DB, 6, "artist_1/in_target_1.mp3", 3.0);
      Insert (DB, 7, "artist_1/in_target_2.mp3", 3.0);
      Insert (DB, 8, "artist_2/in_target_3.mp3", 3.0);
      Insert (DB, 9, "artist_2/new_3.mp3", 0.0);
      Insert (DB, 10, "artist_2/new_4.mp3", 0.0);
      Insert (DB, 11, "artist_2/played_3.mp3", 2.0);
      Insert (DB, 12, "artist_2/played_4.mp3", 2.0);
      Insert (DB, 13, "artist_2/played_5.mp3", 2.5);
      Insert (DB, 14, "artist_2/played_6.mp3", 2.5);
      Insert (DB, 15, "artist_2/played_7.mp3", 2.5);
      Insert (DB, 16, "artist_2/played_8.mp3", 2.5);
      Insert (DB, 17, "artist_3/new_5.mp3", 0.0);
      Insert (DB, 18, "artist_3/new_6.mp3", 0.0);

      Create_Directory ("tmp/source");
      Create_Directory ("tmp/source/artist_1");
      Create_Test_File ("tmp/source/artist_1/AlbumArt_artist_1.jpg");

      Create ("tmp/source/artist_1/excluded_1.mp3", (Artist, +"artist_1") & (Album, +"") & (Title, +"excluded_1"));
      Create ("tmp/source/artist_1/played_1.mp3", (Artist, +"artist_1") & (Album, +"") & (Title, +"played_1"));
      Create ("tmp/source/artist_1/played_2.mp3", (Artist, +"artist_1") & (Album, +"") & (Title, +"played_2"));
      Create ("tmp/source/artist_1/new_1.mp3", (Artist, +"artist_1") & (Album, +"") & (Title, +"new_1"));
      Create ("tmp/source/artist_1/new_2.mp3", (Artist, +"artist_1") & (Album, +"") & (Title, +"new_2"));
      Create ("tmp/source/artist_1/in_target_1.mp3", (Artist, +"artist_1") & (Album, +"") & (Title, +"in_target_1"));
      Create ("tmp/source/artist_1/in_target_2.mp3", (Artist, +"artist_1") & (Album, +"") & (Title, +"in_target_2"));
      Create_Directory ("tmp/source/artist_2");
      Create_Test_File ("tmp/source/artist_2/AlbumArt_artist_2.jpg");
      Create ("tmp/source/artist_2/in_target_3.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"in_target_3"));
      Create ("tmp/source/artist_2/new_3.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"new_3"));
      Create ("tmp/source/artist_2/new_4.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"new_4"));
      Create ("tmp/source/artist_2/played_3.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"played_3"));
      Create ("tmp/source/artist_2/played_4.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"played_4"));
      Create ("tmp/source/artist_2/played_5.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"played_5"));
      Create ("tmp/source/artist_2/played_6.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"played_6"));
      Create ("tmp/source/artist_2/played_7.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"played_7"));
      Create ("tmp/source/artist_2/played_8.mp3", (Artist, +"artist_2") & (Album, +"") & (Title, +"played_8"));

      --  Album art is downloaded when a new target directory is created.
      Create_Directory ("tmp/source/artist_3");
      Create_Test_File ("tmp/source/artist_3/AlbumArt_artist_3.jpg");
      Create_Test_File ("tmp/source/artist_3/liner_notes_artist_3.jpg");
      Create ("tmp/source/artist_3/new_5.mp3", (Artist, +"artist_3") & (Album, +"") & (Title, +"new_5"));
      Create ("tmp/source/artist_3/new_6.mp3", (Artist, +"artist_3") & (Album, +"") & (Title, +"new_6"));

      Create_Directory ("tmp/target");
      Create_Directory ("tmp/target/vocal");
      Create_Directory ("tmp/target/vocal/artist_1");
      Create ("tmp/target/vocal/artist_1/in_target_1.mp3", (Artist, +"vocal") & (Album, +"") & (Title, +"artist_1"));
      Create ("tmp/target/vocal/artist_1/in_target_2.mp3", (Artist, +"vocal") & (Album, +"") & (Title, +"artist_1"));
      Create_Directory ("tmp/target/vocal/artist_2");
      Create ("tmp/target/vocal/artist_2/in_target_3.mp3", (Artist, +"vocal") & (Album, +"") & (Title, +"artist_2"));

      Create (Playlist, Out_File, "tmp/target/vocal.m3u");
      Put_Line (Playlist, "vocal/artist_1/in_target_1.mp3");
      Put_Line (Playlist, "vocal/artist_1/in_target_2.mp3");
      Put_Line (Playlist, "vocal/artist_2/in_target_3.mp3");
      Close (Playlist);

      SMM.Download
        (DB,
         Source_Root       => "tmp/source/",
         Category          => "vocal",
         Destination       => SMM.As_Directory (Current_Directory & "/tmp/target"),
         Song_Count        => 6,
         New_Song_Count    => 8,
         Over_Select_Ratio => 2.0,
         Download_Time     => "1960-01-01 00:00:00",
         Seed              => 1);

      Set_Directory (Start_Dir);

      DB.Finalize;

      Open (Playlist, In_File, "tmp/target/vocal.m3u");
      --  Previous songs
      Check (Playlist, "vocal/artist_1/in_target_1.mp3");
      Check (Playlist, "vocal/artist_1/in_target_2.mp3");
      Check (Playlist, "vocal/artist_2/in_target_3.mp3");
      --  New songs and less recent songs in random order - choice and order can
      --  change with compiler version. This is correct for GNAT GPL
      --  2017. Possible songs here are all except 6, 7, 8
      Check (Playlist, "vocal/artist_2/played_4.mp3");
      Check (Playlist, "vocal/artist_2/played_8.mp3");
      Check (Playlist, "vocal/artist_1/new_2.mp3");
      Check (Playlist, "vocal/artist_2/played_7.mp3");
      Check (Playlist, "vocal/artist_1/new_1.mp3");
      Check (Playlist, "vocal/artist_3/new_6.mp3");

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
      Check_File_Exists ("tmp/target/vocal/artist_3/AlbumArt_artist_3.jpg");
      Check_File_Exists ("tmp/target/vocal/artist_3/new_6.mp3");
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
