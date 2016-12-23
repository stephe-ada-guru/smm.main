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

with AUnit.Assertions;
with AUnit.Checks.Text_IO;
with AWS.Client;
with AWS.Messages.AUnit;
with AWS.Response.AUnit;
with AWS.URL;
with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL.Config_Files;
with SAL.Time_Conversions.AUnit;
with SMM;
with Test_Utils;
package body Test_Server is

   Db_File_Name : constant String := "tmp/smm.db";

   Server : GNAT.OS_Lib.Process_Id;

   Verbose : Boolean := False;

   ----------
   --  Test procedures

   procedure Test_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;

      Test : Test_Case renames Test_Case (T);

      URL      : constant String := "http://" & Test.Server_IP.all & ":8080/download?category=vocal&count=5&seed=0";
      Response : constant Data   := AWS.Client.Get (URL);
      Msg      : constant String := Message_Body (Response);

      --  Song order depends on random engine
      Expected : constant String :=
        "artist_2/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
          "artist_1/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
          "artist_2/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
          "artist_1/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
          "artist_1/album_1/03 The Dance #1.mp3" & ASCII.CR & ASCII.LF;
   begin
      if Verbose then
         Ada.Text_IO.Put (Msg);
      end if;
      Check ("playlist", Msg, Expected);
   end Test_Playlist;

   procedure Test_Meta (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;

      Test : Test_Case renames Test_Case (T);

      URL      : constant String := "http://" & Test.Server_IP.all & ":8080/artist_1/album_1/meta";
      Response : constant Data   := AWS.Client.Get (URL);
      Msg      : constant String := Message_Body (Response);

      --  Song order depends on random engine
      Expected : constant String :=
        "artist_1/album_1/AlbumArt_1.jpg" & ASCII.CR & ASCII.LF &
        "artist_1/album_1/liner_notes.pdf" & ASCII.CR & ASCII.LF;
   begin
      if Verbose then
         Ada.Text_IO.Put (Msg);
      end if;
      Check ("meta", Msg, Expected);
   end Test_Meta;

   procedure Test_Get_File (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;
      use AWS.Response.AUnit;

      Test : Test_Case renames Test_Case (T);

      procedure Check_File
        (Directory  : in String;
         Filename   : in String;
         Mime       : in String;
         Song_Index : in Integer := 0)
      is
         use SAL.Config_Files;
         use SAL.Time_Conversions;
         use SAL.Time_Conversions.AUnit;

         URL : constant String := "http://" & Test.Server_IP.all & ":8080/" & Directory &
           AWS.URL.Encode (Filename);

         Response : constant Data   :=  AWS.Client.Get (URL);
         Msg      : constant String := Message_Body (Response);
         Db_File  : Configuration_Type;
         I        : Iterator_Type;
      begin
         Check ("mode", Mode (Response), Message); --  Not clear why this is "message", not "file"
         Check ("mime", Content_Type (Response), Mime);
         Check ("file content", Msg, "body: tmp/source/" & Directory & Filename & ASCII.CR & ASCII.LF);

         if Song_Index > 0 then
            Open (Db_File, Db_File_Name, Missing_File => Raise_Exception, Read_Only => True);
            I := Find (Db_File, SMM.Songs_Key, Integer'Image (Song_Index));
            Check
              ("last_downloaded",
               SMM.Read_Last_Downloaded (Db_File, I),
               To_TAI_Time (Ada.Calendar.Clock),
               Tolerance => 60.0);
         end if;
      exception
      when AUnit.Assertions.Assertion_Error =>
         Ada.Text_IO.Put_Line (Msg);
         raise;
      end Check_File;

   begin
      Check_File ("artist_1/album_1/", "AlbumArt_1.jpg", "image/jpeg");
      Check_File ("artist_1/album_1/", "1 - song_1.mp3", "audio/mpeg", Song_Index => 1);
      Check_File ("artist_1/album_1/", "liner_notes.pdf", "application/pdf");
      Check_File ("artist_1/album_1/", "03 The Dance #1.mp3", "audio/mpeg", Song_Index => 7);
   end Test_Get_File;

   procedure Test_Send_Notes (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use AUnit.Checks;
      use AUnit.Checks.Text_IO;
      use AWS.Response;
      use AWS.Messages.AUnit;

      Test : Test_Case renames Test_Case (T);

      Note_File_Name : constant String := "tmp/source/remote_cache/vocal.note";
      URL            : constant String := "http://" & Test.Server_IP.all & ":8080/remote_cache/vocal.note";

      Note_File : File_Type;
      Response  : Data;

      Notes_1 : constant String :=
        """vocal/artist_1/album_1/file_1.mp3"" Category" & ASCII.CR & ASCII.LF &
        """vocal/artist_1/album_1/file_2.mp3"" Don't Play" & ASCII.CR & ASCII.LF;

      Notes_2 : constant String :=
        """vocal/artist_2/album_1/file_1.mp3"" Category" & ASCII.CR & ASCII.LF &
        """vocal/artist_2/album_1/file_2.mp3"" Don't Play" & ASCII.CR & ASCII.LF;
   begin
      --  Test create notes on server
      Response := AWS.Client.Put (URL, Notes_1);

      Check ("1 status", Status_Code (Response), AWS.Messages.S200);

      Check ("1 exists", Exists (Note_File_Name), True);
      Open (Note_File, In_File, Note_File_Name);
      Check (Note_File, """vocal/artist_1/album_1/file_1.mp3"" Category");
      Check (Note_File, """vocal/artist_1/album_1/file_2.mp3"" Don't Play");
      Check_End (Note_File);
      Close (Note_File);

      --  test append to notes on server
      Response := AWS.Client.Put (URL, Notes_2);
      Check ("2 status", Status_Code (Response), AWS.Messages.S200);

      Open (Note_File, In_File, Note_File_Name);
      Check (Note_File, """vocal/artist_1/album_1/file_1.mp3"" Category");
      Check (Note_File, """vocal/artist_1/album_1/file_2.mp3"" Don't Play");
      Check (Note_File, """vocal/artist_2/album_1/file_1.mp3"" Category");
      Check (Note_File, """vocal/artist_2/album_1/file_2.mp3"" Don't Play");
      Check_End (Note_File);
      Close (Note_File);

   end Test_Send_Notes;

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
      if T.Debug = 1 then
         Register_Routine (T, Test_Get_File'Access, "Test_Get_File");
      else
         Register_Routine (T, Test_Playlist'Access, "Test_Playlist");
         Register_Routine (T, Test_Meta'Access, "Test_Meta");
         Register_Routine (T, Test_Get_File'Access, "Test_Get_File");
         Register_Routine (T, Test_Send_Notes'Access, "Test_Send_Notes");
      end if;
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use Test_Utils;

      Db_File      : File_Type;
      Dir          : constant String := Current_Directory;
   begin
      if Dir (Dir'Last - 5 .. Dir'Last) /= "\build" then
         raise SAL.Programmer_Error with "current_directory = " & Current_Directory;
      end if;

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
         Put_Line (Db_File, "Songs. 7.File = artist_1/album_1/03 The Dance #1.mp3");
         Put_Line (Db_File, "Songs. 7.Category = vocal");

         Close (Db_File);

         Create_Directory ("tmp/source");
         Create_Directory ("tmp/source/artist_1");
         Create_Directory ("tmp/source/artist_1/album_1");
         Create_Test_File ("tmp/source/artist_1/album_1/liner_notes.pdf");
         Create_Test_File ("tmp/source/artist_1/album_1/AlbumArt_1.jpg");
         Create_Test_File ("tmp/source/artist_1/album_1/1 - song_1.mp3");
         Create_Test_File ("tmp/source/artist_1/album_1/2 - song_2.mp3");
         Create_Test_File ("tmp/source/artist_1/album_1/03 The Dance #1.mp3");

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
