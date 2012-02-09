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
with SAL.AUnit.Text_IO;
with SMM.Second_Pass;
with Test_Utils; use Test_Utils;
package body Test_Second_Pass is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use SAL.AUnit.Text_IO;

      pragma Unreferenced (T);

      Playlist : File_Type;

      Start_Dir : constant String := Current_Directory;
   begin
      --  Create the test environment; a playlist, corresponding
      --  directory with files not in playlist.

      Cleanup;

      Create_Directory ("tmp");
      Create_Directory ("tmp/vocal");
      Create_Directory ("tmp/vocal/artist_1");
      Create_Test_File ("tmp/vocal/artist_1/file_6.mp3");
      Create_Test_File ("tmp/vocal/artist_1/file_7.mp3");
      Create_Directory ("tmp/vocal/artist_2");
      Create_Test_File ("tmp/vocal/artist_2/file_8.mp3");

      Create (Playlist, Out_File, "tmp/vocal.m3u");
      Put_Line (Playlist, "vocal/artist_1/file_6.mp3");
      Close (Playlist);

      SMM.Second_Pass
        (Category => "vocal",
         Root_Dir => SMM.As_Directory (Current_Directory & "/tmp"));

      Set_Directory (Start_Dir);
      Open (Playlist, In_File, "tmp/vocal.m3u");
      Check (Playlist, "vocal/artist_1/file_6.mp3");
      Check (Playlist, "vocal/artist_1/file_7.mp3");
      Check (Playlist, "vocal/artist_2/file_8.mp3");
      Check_End (Playlist);
      Close (Playlist);

   end Nominal;

   procedure Empty_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use SAL.AUnit.Text_IO;

      pragma Unreferenced (T);

      Playlist : File_Type;

      Start_Dir : constant String := Current_Directory;
   begin
      --  An empty playlist

      Cleanup;

      Create_Directory ("tmp");
      Create_Directory ("tmp/vocal");
      Create_Test_File ("tmp/vocal/file_6.mp3");
      Create_Test_File ("tmp/vocal/file_7.mp3");
      Create_Test_File ("tmp/vocal/file_8.mp3");

      Create (Playlist, Out_File, "tmp/vocal.m3u");
      Close (Playlist);

      SMM.Second_Pass
        (Category => "vocal",
         Root_Dir => SMM.As_Directory (Current_Directory & "/tmp"));

      Set_Directory (Start_Dir);

      --  WORKAROUND; Ada.Directories.Search returns files in
      --  different order on Windows than on GNU/Linux.
      Open (Playlist, In_File, "tmp/vocal.m3u");
      Check (Playlist, ""); --  Create inserts one empty line, append doesn't delete it.
      Check (Playlist, "vocal/file_6.mp3");
      Check (Playlist, "vocal/file_7.mp3");
      Check (Playlist, "vocal/file_8.mp3");
      Check_End (Playlist);
      Close (Playlist);

   end Empty_Playlist;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Second_Pass");
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

end Test_Second_Pass;
