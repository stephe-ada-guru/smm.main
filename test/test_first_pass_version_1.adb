--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2012 Stephen Leake.  All Rights Reserved.
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
with SAL.AUnit;
with Test_Utils; use Test_Utils;
package body Test_First_Pass_Version_1 is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use SAL.AUnit;

      pragma Unreferenced (T);

      Playlist : File_Type;

      Start_Dir : constant String := Current_Directory;

      File_Count : Integer;

   begin
      --  Create the test environment; a playlist, corresponding
      --  directory with files not in playlist.

      Cleanup;

      Create_Directory ("tmp");
      Create_Directory ("tmp/Vocal");
      Create_Directory ("tmp/Vocal/artist_1");
      Create_Test_File ("tmp/Vocal/artist_1/file_4.mp3");
      Create_Test_File ("tmp/Vocal/artist_1/file_5.mp3");
      Create_Directory ("tmp/Vocal/artist_2");
      Create_Test_File ("tmp/Vocal/artist_2/file_6.mp3");

      Create (Playlist, Out_File, "tmp/Vocal.m3u");
      Put_Line (Playlist, "Vocal/artist_2/file_6.mp3");
      Close (Playlist);

      SMM.First_Pass
        (Category   => "Vocal",
         Root_Dir   => SMM.As_Directory (Current_Directory & "/tmp"),
         File_Count => File_Count);

      Check ("File_Count", File_Count, 1);

      --  Check that the extra files are deleted, but the others are not.
      Set_Directory (Start_Dir);
      Check_Exists ("tmp/Vocal/artist_1/file_4.mp3", False);
      Check_Exists ("tmp/Vocal/artist_1/file_5.mp3", False);
      Check_Exists ("tmp/Vocal/artist_2/file_6.mp3", True);
   end Nominal;

   procedure Empty_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use SAL.AUnit;

      Test : Test_Case renames Test_Case (T);

      Playlist : File_Type;

      Start_Dir : constant String := Current_Directory;

      File_Count : Integer;
   begin
      --  test an empty playlist

      Cleanup;
      if Test.Verbosity > 0 then
         Ada.Text_IO.Put_Line ("Starting Empty_Playlist");
      end if;

      Create_Directory ("tmp");
      Create_Directory ("tmp/Vocal");
      Create_Test_File ("tmp/Vocal/file_4.mp3");
      Create_Test_File ("tmp/Vocal/file_5.mp3");
      Create_Test_File ("tmp/Vocal/file_6.mp3");

      Create (Playlist, Out_File, "tmp/Vocal.m3u");
      Close (Playlist);

      SMM.First_Pass
        (Category   => "Vocal",
         Root_Dir   => SMM.As_Directory (Current_Directory & "/tmp"),
         File_Count => File_Count);

      Check ("File_Count", File_Count, 0);

      --  Check that the extra files are deleted
      Set_Directory (Start_Dir);
      Check_Exists ("tmp/Vocal/file_4.mp3", False);
      Check_Exists ("tmp/Vocal/file_5.mp3", False);
      Check_Exists ("tmp/Vocal/file_6.mp3", False);

   end Empty_Playlist;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_First_Pass_Version_1");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      case T.Debug is
      when 0 =>
         Register_Routine (T, Nominal'Access, "Nominal");
         Register_Routine (T, Empty_Playlist'Access, "Empty_Playlist");

      when 1 =>
         Register_Routine (T, Nominal'Access, "Nominal");

      when 2 =>
         Register_Routine (T, Empty_Playlist'Access, "Empty_Playlist");

      when others =>
         raise SAL.Programmer_Error;
      end case;

   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      SMM.Verbosity := T.Verbosity;
   end Set_Up_Case;

end Test_First_Pass_Version_1;
