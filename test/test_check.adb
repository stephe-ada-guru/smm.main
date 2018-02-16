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

with AUnit.Checks.Text_IO;
with Ada.Directories;
with GNAT.OS_Lib;
with SMM.Database;
with SMM.ID3;
with Test_Utils;
package body Test_Check is

   Dir          : constant String := Ada.Directories.Current_Directory;
   DB_File_Name : constant String := Dir & "/tmp/smm.db";

   ----------
   --  Test procedures

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;
      use AUnit.Checks.Text_IO;
      use Ada.Directories;
      use SMM;
      use SMM.ID3;
      use Test_Utils;

      Source_Dir     : constant String := "/tmp/source/";
      Out_File_Name  : constant String := "test_check.out";
      Good_File_Name : constant String := "../test/test_check.good_out";
      DB             : SMM.Database.Database;

      Success : Boolean;
      Status  : Integer;
   begin
      if Dir (Dir'Last - 5 .. Dir'Last) /= "\build" then
         raise Program_Error with "current_directory = " & Current_Directory;
      end if;

      Cleanup;
      Test_Utils.Delete_File (Out_File_Name);

      Create_Directory ("tmp");

      Create_Empty_DB (DB_File_Name);

      Create_Directory ("tmp/source");
      Create_Directory ("tmp/source/artist_1");
      Create_Directory ("tmp/source/artist_1/album_1");
      Create_Test_File ("tmp/source/artist_1/album_1/liner_notes.pdf");
      Create_Test_File ("tmp/source/artist_1/album_1/AlbumArt_huge.jpg");

      Create
        ("tmp/source/artist_1/album_1/1 - song_1.mp3",
         (Artist, +"artist_1") &
           (Album, +"album_1") &
           (Title, +"1 - song_1"));

      Create
        ("tmp/source/artist_1/album_1/2 - song_2.mp3", -- not in db
         (Artist, +"artist_1") &
           (Album, +"album_1") &
           (Title, +"2 - song_2"));

      Create_Directory ("tmp/source/artist_2");
      Create_Directory ("tmp/source/artist_2/album_1"); -- missing liner_notes, album art

      Create
        ("tmp/source/artist_2/album_1/1 - song_1.mp3",
         (Artist, +"artist_2") &
           (Album, +"album_1") &
           (Title, +"1 - song_1"));

      Create
        ("tmp/source/artist_2/album_1/2 - song_2.mp3",
         (Artist, +"artist_2") &
           (Album, +"album_1") &
           (Title, +"1 - song_1"));


      DB.Open (DB_File_Name);

      DB.Insert
        (ID          => 1,
         File_Name   => "artist_1/album_1/1 - song_1.mp3",
         Category    => "test",
         Artist      => "artist_1",
         Album       => "album_1",
         Title       => "1 - song_1",
         Play_Before => 10); -- missing Play_After

      DB.Insert
        (ID          => 2,
         File_Name   => "artist_2/album_1/1 - song_1.mp3",
         Category    => "test",
         Artist      => "artist_2",
         Album       => "album_1",
         Title       => "1 - song_1",
         Play_Before => 3); -- mismatched

      DB.Insert
        (ID         => 3,
         File_Name  => "artist_2/album_1/2 - song_2.mp3",
         Category   => "test",
         Artist     => "artist_2",
         Album      => "album_1",
         Title      => "2 - song_2",
         Play_After => 4); -- missing Play_Before

      DB.Insert
        (ID          => 4,
         File_Name   => "artist_2/album_1/3 - song_3.mp3", -- not on disk
         Category    => "test",
         Artist      => "artist_2",
         Album       => "album_1",
         Title       => "3 - song_3",
         Play_After  => 6); -- not in db

      DB.Insert
        (ID          => 10, -- non-sequential ID ok
         File_Name   => "artist_2/album_1/4 - song_4.mp3", -- not on disk
         Category    => "test",
         Artist      => "artist_2",
         Album       => "album_1",
         Title       => "3 - song_3",
         Play_Before => 11); -- not in db

      Set_Directory (Dir & Source_Dir);

      GNAT.OS_Lib.Spawn
        (Program_Name => Dir & "/smm.exe",
         Args         =>
           (new String'("--db=" & DB_File_Name),
            new String'("check")),
         Output_File  => Out_File_Name,
         Success      => Success,
         Return_Code  => Status,
         Err_To_Out   => True);

      if not Success then
         raise Program_Error with "smm check failed to execute";
      end if;

      Check ("status", Status, 1);

      Set_Directory (Dir);
      Check_Files ("1", Dir & Source_Dir & Out_File_Name, Good_File_Name);
   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_check.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Ada.Directories.Set_Directory (Dir);
   end Tear_Down_Case;

end Test_Check;
