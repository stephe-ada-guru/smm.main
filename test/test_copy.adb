--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2012, 2013, 2015, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Directories;
with Ada.Text_IO;
with AUnit.Checks.Text_IO;
with SMM.Copy;
with Test_Utils; use Test_Utils;
package body Test_Copy is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;

      pragma Unreferenced (T);

      Build_Directory : constant String := Current_Directory;

      Playlist : File_Type;

      procedure Test (File : in String)
      is
         use AUnit.Assertions;
         Target : constant String := Build_Directory & "/tmp/target/" & File;
      begin
         Check (Playlist, File);
         Assert (Exists (Target), Target & " not found");
      end Test;

   begin
      --  Create the test environment; an existing playlist and
      --  corresponding files.

      Cleanup;

      Create_Directory ("tmp");

      Create_Directory ("tmp/source");
      Create_Directory ("tmp/source/artist_1");
      Create_Test_File ("tmp/source/artist_1/song_1.mp3");
      Create_Test_File ("tmp/source/artist_1/song_2.mp3");
      Create_Directory ("tmp/source/artist_2");
      Create_Test_File ("tmp/source/artist_2/song_3.mp3");
      Create_Test_File ("tmp/source/artist_2/song_4.mp3");

      Create_Directory ("tmp/source/playlists");
      Create (Playlist, Out_File, "tmp/source/playlists/yoga.m3u");
      Put_Line (Playlist, "../artist_1/song_1.mp3");
      Put_Line (Playlist, "../artist_1/song_2.mp3");
      Put_Line (Playlist, "../artist_2/song_3.mp3");
      Put_Line (Playlist, "../artist_2/song_4.mp3");
      Close (Playlist);

      Create_Directory ("tmp/target");

      Set_Directory ("tmp/source"); -- smm db directory root

      SMM.Copy
        (Playlist    => Build_Directory & "/tmp/source/playlists/yoga.m3u",
         Destination => SMM.As_Directory (Build_Directory & "/tmp/target"));

      Set_Directory (Build_Directory & "/tmp/target");

      Open (Playlist, In_File, "yoga.m3u");
      Test ("yoga/artist_1/song_1.mp3");
      Test ("yoga/artist_1/song_2.mp3");
      Test ("yoga/artist_2/song_3.mp3");
      Test ("yoga/artist_2/song_4.mp3");
      Check_End (Playlist);
      Close (Playlist);

      Set_Directory (Build_Directory);

   exception
   when others =>
      if Is_Open (Playlist) then
         Close (Playlist);
      end if;
      Set_Directory (Build_Directory);
      raise;
   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_copy.adb");
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

end Test_Copy;
