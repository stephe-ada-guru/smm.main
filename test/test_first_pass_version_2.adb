--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2011 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with Ada.Directories;
with Ada.Text_IO;
with SMM.First_Pass;
with SAL.AUnit.Text_IO;
with Test_Utils; use Test_Utils;
package body Test_First_Pass_Version_2 is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Ada.Directories;
      use Ada.Text_IO;
      use SAL.AUnit;
      use SAL.AUnit.Text_IO;

      File : File_Type;

      Start_Dir : constant String := Current_Directory;

      File_Count : Integer;

   begin
      --  Create the test environment; a playlist, a .last,
      --  corresponding directory with files in playlist.

      Cleanup;

      Create_Directory ("tmp");
      Create_Directory ("tmp/Vocal");
      Create_Test_File ("tmp/Vocal/file_4.mp3");
      Create_Test_File ("tmp/Vocal/file_5.mp3");
      Create_Test_File ("tmp/Vocal/file_6.mp3");

      Create (File, Out_File, "tmp/Vocal.m3u");
      Put_Line (File, "Vocal/file_4.mp3");
      Put_Line (File, "Vocal/file_5.mp3");
      Put_Line (File, "Vocal/file_6.mp3");
      Close (File);

      Create (File, Out_File, "tmp/Vocal.last");
      Put_Line (File, "Vocal/file_5.mp3");
      Close (File);

      SMM.First_Pass (Category => "Vocal", Root_Dir => "tmp/", File_Count => File_Count);

      Check ("File_Count", File_Count, 1);

      --  Check that the played files are deleted, but the others are not.
      Set_Directory (Start_Dir);
      Check_Exists ("tmp/Vocal/file_4.mp3", False);
      Check_Exists ("tmp/Vocal/file_5.mp3", False);
      Check_Exists ("tmp/Vocal/file_6.mp3", True);

      --  Check that the playlist is updated
      Open (File, In_File, "tmp/Vocal.m3u");
      Check (File, "Vocal/file_6.mp3");
      Check_End (File);
      Close (File);

      --  Check that .last file is deleted
      Check_Exists ("tmp/Vocal.last", False);

   end Nominal;

   procedure New_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Ada.Directories;
      use Ada.Text_IO;
      use SAL.AUnit;
      use SAL.AUnit.Text_IO;

      File : File_Type;

      Start_Dir : constant String := Current_Directory;

      File_Count : Integer;
   begin
      --  test a completely new playlist; .last does not exist, or is empty.

      Cleanup;

      Create_Directory ("tmp");
      Create_Directory ("tmp/Vocal");
      Create_Test_File ("tmp/Vocal/file_4.mp3");
      Create_Test_File ("tmp/Vocal/file_5.mp3");
      Create_Test_File ("tmp/Vocal/file_6.mp3");

      Create (File, Out_File, "tmp/Vocal.m3u");
      Put_Line (File, "Vocal/file_4.mp3");
      Put_Line (File, "Vocal/file_5.mp3");
      Put_Line (File, "Vocal/file_6.mp3");
      Close (File);

      SMM.First_Pass (Category => "Vocal", Root_Dir => "tmp", File_Count => File_Count);

      Check ("File_Count", File_Count, 3);

      Set_Directory (Start_Dir);
      Check_Exists ("tmp/Vocal/file_4.mp3", True);
      Check_Exists ("tmp/Vocal/file_5.mp3", True);
      Check_Exists ("tmp/Vocal/file_6.mp3", True);

      Open (File, In_File, "tmp/Vocal.m3u");
      Check (File, "Vocal/file_4.mp3");
      Check (File, "Vocal/file_5.mp3");
      Check (File, "Vocal/file_6.mp3");
      Check_End (File);
      Close (File);

      Check_Exists ("tmp/Vocal.last", False);

      --  now test present but empty .last
      Create (File, Out_File, "tmp/Vocal.last");
      New_Line (File);
      Close (File);

      SMM.First_Pass (Category => "Vocal", Root_Dir => "tmp", File_Count => File_Count);

      Check ("File_Count", File_Count, 3);

      Check_Exists ("tmp/Vocal.last", False);

   end New_Playlist;

   procedure Finished_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Ada.Directories;
      use Ada.Text_IO;
      use SAL.AUnit;
      use SAL.AUnit.Text_IO;

      File : File_Type;

      Start_Dir : constant String := Current_Directory;

      File_Count : Integer;
   begin
      --  test a completely played playlist; .last has last file in
      --  playlist

      Cleanup;

      Create_Directory ("tmp");
      Create_Directory ("tmp/Vocal");
      Create_Test_File ("tmp/Vocal/file_4.mp3");
      Create_Test_File ("tmp/Vocal/file_5.mp3");
      Create_Test_File ("tmp/Vocal/file_6.mp3");

      Create (File, Out_File, "tmp/Vocal.m3u");
      Put_Line (File, "Vocal/file_4.mp3");
      Put_Line (File, "Vocal/file_5.mp3");
      Put_Line (File, "Vocal/file_6.mp3");
      Close (File);

      Create (File, Out_File, "tmp/Vocal.last");
      Put_Line (File, "Vocal/file_6.mp3");
      Close (File);

      SMM.First_Pass (Category => "Vocal", Root_Dir => "tmp", File_Count => File_Count);

      Check ("File_Count", File_Count, 0);

      Set_Directory (Start_Dir);
      Check_Exists ("tmp/Vocal/file_4.mp3", False);
      Check_Exists ("tmp/Vocal/file_5.mp3", False);
      Check_Exists ("tmp/Vocal/file_6.mp3", False);

      Open (File, In_File, "tmp/Vocal.m3u");
      Check_End (File);
      Close (File);

      Check_Exists ("tmp/Vocal.last", False);

   end Finished_Playlist;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_First_Pass_Version_2");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Nominal'Access, "Nominal");

      else
         Register_Routine (T, Nominal'Access, "Nominal");
         Register_Routine (T, New_Playlist'Access, "New_Playlist");
         Register_Routine (T, Finished_Playlist'Access, "Finished_Playlist");
      end if;

   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      SMM.Verbosity := T.Verbosity;
   end Set_Up_Case;

end Test_First_Pass_Version_2;
