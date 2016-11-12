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
   pragma Unreferenced (Verbose);

   ----------
   --  Test procedures

   --  procedure Test_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   --  is
   --     use AUnit.Checks;
   --     use Books.Database.Diff; -- To_Insert etc
   --     use GNATCOLL.JSON.AUnit;

   --     Test : Test_Case renames Test_Case (T);

   --     Remote_DB : aliased Books.Database_Remote.IP.Database (Open_Stream (Test.Server_IP.all), 0);

   --  begin
   --  end Test_Playlist;

   procedure Test_Get_File (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;
      use AWS.Response.AUnit;

      Test : Test_Case renames Test_Case (T);

      procedure Check_File
        (Directory : in String;
         Filename : in String)
      is
         URL : constant String := "http://" & Test.Server_IP.all & ":8080/" & Directory &
           AWS.URL.Encode (Filename);

         Response : constant Data   :=  AWS.Client.Get (URL);
         Msg      : constant String := Message_Body (Response);
      begin
         Check ("mode", Mode (Response), Message); --  Not clear why this is "message", not "file"
         Check ("file content", Msg, "body: tmp/source/" & Directory & Filename & ASCII.CR & ASCII.LF);
      end Check_File;

   begin
      Check_File ("artist_1/album_1/", "AlbumArt_1.jpg");
      Check_File ("artist_1/album_1/", "1 - song_1.mp3");
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
--      Register_Routine (T, test_playlist'Access, "test_playlist");
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

         Close (Db_File);

         Create_Directory ("tmp/source");
         Create_Directory ("tmp/source/artist_1");
         Create_Directory ("tmp/source/artist_1/album_1");
         Create_Test_File ("tmp/source/artist_1/album_1/AlbumArt_1.jpg");
         Create_Test_File ("tmp/source/artist_1/album_1/1 - song_1.mp3");

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
