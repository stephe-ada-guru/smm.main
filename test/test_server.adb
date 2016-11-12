--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2016 Stephen Leake.  All Rights Reserved.
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
with AWS.Client;
with AWS.Response.AUnit;
with AWS.URL;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL;
with SMM;
with Test_Utils;
package body Test_Server is

   Server : GNAT.OS_Lib.Process_Id;

   Verbose : Boolean := False;

   ----------
   --  Test procedures

   procedure Test_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;

      Test : Test_Case renames Test_Case (T);

      URL : constant String := "http://" & Test.Server_IP.all & ":8080/download?category=vocal&count=5&seed=0";
      Response : constant Data   :=  AWS.Client.Get (URL);
      Msg      : constant String := Message_Body (Response);

      --  Song order depends on random engine
      Expected : constant String :=
        "artist_2/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
          "artist_1/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
          "artist_2/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
          "artist_1/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
          "artist_2/album_1/3 - song_3.mp3" & ASCII.CR & ASCII.LF;

      --  FIXME: add these:
      --    "artist_1/album_1/AlbumArg_1.jpg" & ASCII.CR &
      --    "artist_1/album_1/liner_notes.pdf" & ASCII.CR &
      --    "artist_2/album_1/AlbumArg_1.jpg" & ASCII.CR;
   begin
      if Verbose then
         Ada.Text_IO.Put (Msg);
      end if;
      Check ("playlist", Msg, Expected);
   end Test_Playlist;

   procedure Test_Get_File (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;
      use AWS.Response.AUnit;

      Test : Test_Case renames Test_Case (T);

      procedure Check_File
        (Directory : in String;
         Filename  : in String;
         Mime      : in String)
      is
         URL : constant String := "http://" & Test.Server_IP.all & ":8080/" & Directory &
           AWS.URL.Encode (Filename);

         Response : constant Data   :=  AWS.Client.Get (URL);
         Msg      : constant String := Message_Body (Response);
      begin
         Check ("mode", Mode (Response), Message); --  Not clear why this is "message", not "file"
         Check ("mime", Content_Type (Response), Mime);
         Check ("file content", Msg, "body: tmp/source/" & Directory & Filename & ASCII.CR & ASCII.LF);
      end Check_File;

   begin
      Check_File ("artist_1/album_1/", "AlbumArt_1.jpg", "image/jpeg");
      Check_File ("artist_1/album_1/", "1 - song_1.mp3", "audio/mpeg");
      Check_File ("artist_1/album_1/", "liner_notes.pdf", "application/pdf");
   end Test_Get_File;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_server.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Playlist'Access, "Test_Playlist");
      Register_Routine (T, Test_Get_File'Access, "Test_Get_File");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use Test_Utils;

      Db_File_Name : constant String := "tmp/smm.db";
      Db_File      : File_Type;
   begin
      Verbose := T.Debug > 0;

      if T.Debug /= 1 then
         Cleanup;

         Create_Directory ("tmp");

         Create (Db_File, Out_File, Db_File_Name);

         Put_Line (Db_File, "Root = " & SMM.As_Directory (Current_Directory & "/tmp/source"));
         Put_Line (Db_File, "Songs. 1.File = artist_1/album_1/1 - song_1.mp3");
         Put_Line (Db_File, "Songs. 1.Category = vocal");
         Put_Line (Db_File, "Songs. 2.File = artist_1/album_1/2 - song_2.mp3");
         Put_Line (Db_File, "Songs. 2.Category = vocal");
         Put_Line (Db_File, "Songs. 3.File = artist_1/album_1/3 - song_3.mp3");
         Put_Line (Db_File, "Songs. 3.Category = instrumental");
         Put_Line (Db_File, "Songs. 4.File = artist_2/album_1/1 - song_1.mp3");
         Put_Line (Db_File, "Songs. 4.Category = vocal");
         Put_Line (Db_File, "Songs. 5.File = artist_2/album_1/2 - song_2.mp3");
         Put_Line (Db_File, "Songs. 5.Category = vocal");
         Put_Line (Db_File, "Songs. 6.File = artist_2/album_1/3 - song_3.mp3");
         Put_Line (Db_File, "Songs. 6.Category = vocal");

         Close (Db_File);

         Create_Directory ("tmp/source");
         Create_Directory ("tmp/source/artist_1");
         Create_Directory ("tmp/source/artist_1/album_1");
         Create_Test_File ("tmp/source/artist_1/album_1/liner_notes.pdf");
         Create_Test_File ("tmp/source/artist_1/album_1/AlbumArt_1.jpg");
         Create_Test_File ("tmp/source/artist_1/album_1/1 - song_1.mp3");
         Create_Test_File ("tmp/source/artist_1/album_1/2 - song_2.mp3");

         Create_Directory ("tmp/source/artist_2");
         Create_Directory ("tmp/source/artist_2/album_1");
         Create_Test_File ("tmp/source/artist_2/album_1/liner_notes.pdf");
         Create_Test_File ("tmp/source/artist_2/album_1/AlbumArt_1.jpg");
         Create_Test_File ("tmp/source/artist_2/album_1/1 - song_1.mp3");
         Create_Test_File ("tmp/source/artist_2/album_1/2 - song_2.mp3");
         Create_Test_File ("tmp/source/artist_2/album_1/3 - song_3.mp3");

      end if;

      if T.Debug /= 1 then
         declare
            use GNAT.OS_Lib;
            Args : String_List (1 .. 2) :=
              (1 => new String'("smm_server_test_1.config"),
               2 => null);

            Args_Last : Integer := 1;
         begin
            if T.Debug = 2 then
               Args (2) := new String'("1"); -- enable log
               Args_Last := 2;
            end if;

            Server := GNAT.OS_Lib.Non_Blocking_Spawn ("smm-server_driver.exe", Args (1 .. Args_Last));
            if Server = Invalid_Pid then
               raise SAL.Initialization_Error with "could not spawn smm-server.exe";
            end if;
            delay 1.0; -- give server time to create listening socket
         end;
      end if;

   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      GNAT.OS_Lib.Kill (Server, Hard_Kill => True);
   end Tear_Down_Case;

end Test_Server;
