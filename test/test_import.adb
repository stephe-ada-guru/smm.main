--  Abstract :
--
--  See spec
--
--  Copyright (C) 2018, 2022 Stephen Leake.  All Rights Reserved.
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
with Ada.Directories;
with GNAT.OS_Lib;
with SMM.Database;
with SMM.ID3;
with Test_Utils;
package body Test_Import is

   Dir          : constant String := Ada.Directories.Current_Directory;
   DB_File_Name : constant String := Dir & "/tmp/smm.db";

   ----------
   --  Test procedures

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use Ada.Directories;
      use SMM;
      use SMM.ID3;
      use Test_Utils;

      DB  : SMM.Database.Database;

      Success : Boolean;
      Args    : constant GNAT.OS_Lib.Argument_List :=
        (new String'("--db=" & DB_File_Name),
         new String'("import"),
         new String'("test"),
         new String'("."));
   begin
      if Dir (Dir'Last - 5 .. Dir'Last) /= "\build" then
         raise Program_Error with "current_directory = " & Current_Directory;
      end if;

      Cleanup;

      Create_Directory ("tmp");

      Create_Empty_DB (DB_File_Name);

      Create_Directory ("tmp/source");
      Create_Directory ("tmp/source/artist_1");
      Create_Directory ("tmp/source/artist_1/album_1");
      Create_Test_File ("tmp/source/artist_1/album_1/liner_notes.pdf");
      Create_Test_File ("tmp/source/artist_1/album_1/AlbumArt_1.jpg");

      Create
        ("tmp/source/artist_1/album_1/1 - song_1.mp3",
         +(Artist, +"artist_1") &
           (Album, +"album_1") &
           (Title, +"1 - song_1"));

      Create
        ("tmp/source/artist_1/album_1/2 - song_2.mp3",
         +(Artist, +"artist_1") &
           (Album, +"album_1") &
           (Title, +"2 - song_2"));

      Create
        ("tmp/source/artist_1/album_1/03 The Dance #1.mp3",
         +(Artist, +"artist_1") &
           (Album, +"album_1") &
           (Title, +"03 The Dance #1"));

      Create_Directory ("tmp/source/artist_2");
      Create_Directory ("tmp/source/artist_2/album_1");
      Create_Test_File ("tmp/source/artist_2/album_1/liner_notes.pdf");
      Create_Test_File ("tmp/source/artist_2/album_1/AlbumArt_1.jpg");

      Create
        ("tmp/source/artist_2/album_1/1 - song_1.mp3",
         +(Artist, +"artist_2") &
           (Album, +"album_1") &
           (Title, +"1 - song_1"));

      Create
        ("tmp/source/artist_2/album_1/2 - song_2.mp3",
         +(Artist, +"artist_2") &
           (Album, +"album_1") &
           (Title, +"2 - song_2"));

      Create
        ("tmp/source/artist_2/album_1/3 - song_3.mp3",
         +(Artist, +"artist_2") &
           (Album, +"album_1") &
           (Title, +"3 - song_3"));

      Create_Directory ("tmp/source/Jason Castro [Deluxe] [+Video] [+Digital Booklet]");
      Create_Test_File ("tmp/source/Jason Castro [Deluxe] [+Video] [+Digital Booklet]/liner_notes.pdf");

      Set_Directory (Dir & "/tmp/source");

      GNAT.OS_Lib.Spawn (Dir & "/smm.exe", Args, Success);
      if not Success then
         raise Program_Error with "smm failed";
      end if;

      DB.Open (DB_File_Name);

      declare
         use AUnit.Checks;
         use SMM.Database;

         I : Cursor := DB.First;
      begin
         --  smm import traverses directories/files in alphabetical order

         Check ("1.ID", I.ID, 1);
         Check ("1.file_name", I.File_Name, "artist_1/album_1/03 The Dance #1.mp3");
         I.Next;

         Check ("2.ID", I.ID, 2);
         Check ("2.file_name", I.File_Name, "artist_1/album_1/1 - song_1.mp3");
         I.Next;

         Check ("3.ID", I.ID, 3);
         Check ("3.file_name", I.File_Name, "artist_1/album_1/2 - song_2.mp3");
         I.Next;

         Check ("4.ID", I.ID, 4);
         Check ("4.file_name", I.File_Name, "artist_2/album_1/1 - song_1.mp3");
         I.Next;

         Check ("5.ID", I.ID, 5);
         Check ("5.file_name", I.File_Name, "artist_2/album_1/2 - song_2.mp3");
         I.Next;

         Check ("6.ID", I.ID, 6);
         Check ("6.file_name", I.File_Name, "artist_2/album_1/3 - song_3.mp3");
         I.Next;

         Check ("all files", I.Has_Element, False);
      end;
   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_import.adb");
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

end Test_Import;
