--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2016, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL.Config_Files;
with SAL.Time_Conversions.AUnit;
with SMM.ID3;
with Test_Utils;
package body Test_Server is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Db_File_Name : constant String := "tmp/smm.db";

   Server      : GNAT.OS_Lib.Process_Id;
   Server_Port : constant String := "8081"; --  must match ../build/smm_server_test_1.config Server_Port

   Verbose : Boolean := False;

   function Encode (Item : in String) return String
   is begin
      --  Mimic Android OkHttp encoding, which does not encode [ ].
      return AWS.URL.Encode (Item, Ada.Strings.Maps.To_Set (" #+;/:$,""{}|\^`'"));
   end Encode;

   procedure Check_File
     (Computed           : in String;
      Expected_File_Name : in String)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use AUnit.Checks.Text_IO;

      Expected_File : File_Type;
      Computed_Last : Integer := Computed'First - 1;

      function Get_Line (Item : in String; Last : in out Integer) return String
      is
         --  Our known good files use Unix line endings
         Delim : constant Integer := Index (Source => Item, Pattern => "" & ASCII.LF, From => Last + 1);
      begin
         if Delim < Item'First then
            return Item (Last + 1 .. Item'Last);
         else
            return Result : constant String := Item (Last + 1 .. Delim - 1) do
               Last := Delim;
            end return;
         end if;
      end Get_Line;

   begin
      Open (Expected_File, In_File, Expected_File_Name);
      loop
         exit when End_Of_File (Expected_File);

         Check (Get_Line (Computed, Computed_Last), Expected_File);
      end loop;
   end Check_File;

   ----------
   --  Test procedures

   procedure Test_Playlist (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;

      Test : Test_Case renames Test_Case (T);

      URL      : constant String := "http://" & Test.Server_IP.all &
        ":" & Server_Port & "/download?category=vocal&count=5&new_count=1&seed=0";
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
      use AWS.Response;

      Test : Test_Case renames Test_Case (T);

      procedure Check
        (Label    : in String;
         Resource : in String;
         Expected : in String)
      is
         Encoded_Resource : constant String := Encode (Resource);

         URL      : constant String := "http://" & Test.Server_IP.all & ":" & Server_Port & "/" & Encoded_Resource &
           "/meta";
         Response : constant Data   := AWS.Client.Get (URL);
         Msg      : constant String := Message_Body (Response);
      begin
         if Verbose then
            Ada.Text_IO.Put_Line (URL);
            Ada.Text_IO.Put_Line (Msg);
         end if;
         AUnit.Checks.Check (Label, Msg, Expected);
      end Check;
   begin
      Check
        ("1",
         "artist_1/album_1",
         "artist_1/album_1/AlbumArt_1.jpg" & CRLF &
           "artist_1/album_1/liner_notes.pdf" & CRLF);

      Check
        ("2",
         "Jason Castro [Deluxe] [+Video] [+Digital Booklet]",
         "Jason Castro [Deluxe] [+Video] [+Digital Booklet]/liner_notes.pdf" & CRLF);
   end Test_Meta;

   procedure Test_Get_File (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AWS.Response;
      use AWS.Response.AUnit;

      Test : Test_Case renames Test_Case (T);

      procedure Check_One
        (Directory  : in String;
         Filename   : in String;
         Mime       : in String;
         Song_Index : in Integer := 0)
      is
         use SAL.Config_Files;
         use SAL.Time_Conversions;
         use SAL.Time_Conversions.AUnit;

         URL : constant String := "http://" & Test.Server_IP.all & ":" & Server_Port & "/" & Directory &
           Encode (Filename);

         Response : constant Data   := AWS.Client.Get (URL);
         Msg      : constant String := Message_Body (Response);
         Db_File  : Configuration_Type;
         I        : Iterator_Type;
      begin
         Check (Filename & ".mode", Mode (Response), Message); --  Not clear why this is "message", not "file"
         Check (Filename & ".mime", Content_Type (Response), Mime);
         Check (Filename & ".file content", Msg, "body: tmp/source/" & Directory & Filename & CRLF);

         if Song_Index > 0 then
            Open (Db_File, Db_File_Name, Missing_File => Raise_Exception, Read_Only => True);
            I := Find (Db_File, SMM.Songs_Key, Integer'Image (Song_Index));
            Check
              (Filename & ".last_downloaded",
               SMM.Read_Last_Downloaded (Db_File, I),
               To_TAI_Time (Ada.Calendar.Clock),
               Tolerance => 60.0);
         end if;
      exception
      when AUnit.Assertions.Assertion_Error =>
         Ada.Text_IO.Put_Line (Msg);
         raise;
      end Check_One;

   begin
      Check_One ("artist_1/album_1/", "AlbumArt_1.jpg", "image/jpeg");
      Check_One ("artist_1/album_1/", "1 - song_1.mp3", "audio/mpeg", Song_Index => 1);
      Check_One ("artist_1/album_1/", "liner_notes.pdf", "application/pdf");
      Check_One ("artist_1/album_1/", "03 The Dance #1.mp3", "audio/mpeg", Song_Index => 7);
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
      URL            : constant String := "http://" & Test.Server_IP.all & ":" & Server_Port &
        "/remote_cache/vocal.note";

      Note_File : File_Type;
      Response  : Data;

      Notes_1 : constant String :=
        """vocal/artist_1/album_1/file_1.mp3"" Category" & CRLF &
        """vocal/artist_1/album_1/file_2.mp3"" Don't Play" & CRLF;

      Notes_2 : constant String :=
        """vocal/artist_2/album_1/file_1.mp3"" Category" & CRLF &
        """vocal/artist_2/album_1/file_2.mp3"" Don't Play" & CRLF;
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

   procedure Test_Search_Initial (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AWS.Response;
      use AWS.Messages.AUnit;

      Test : Test_Case renames Test_Case (T);

      URL : constant String := "http://" & Test.Server_IP.all & ":" & Server_Port & "/search";

      Good_File_Name : constant String := "../test/search_initial.html";
      Response       : constant Data   := AWS.Client.Get (URL);

   begin
      --  Test response to initial page request
      Check ("1 status", Status_Code (Response), AWS.Messages.S200);

      declare
         Computed : constant String := Message_Body (Response);
      begin
         Check_File (Computed, Good_File_Name);
      end;
   end Test_Search_Initial;

   procedure Test_Search_Results (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AWS.Response;
      use AWS.Messages.AUnit;

      Test : Test_Case renames Test_Case (T);

      procedure Check_One
        (Label              : in String;
         Query              : in String;
         Expected_File_Name : in String)
      is
         URL      : constant String := "http://" & Test.Server_IP.all & ":" & Server_Port & "/search?" & Query;
         Response : constant Data   := AWS.Client.Get (URL);
      begin
         Check (Label & ".status", Status_Code (Response), AWS.Messages.S200);
         declare
            Computed : constant String := Message_Body (Response);
         begin
            Check_File (Computed, Expected_File_Name);
         end;
      end Check_One;

   begin
      --  Test response to search submissions
      Check_One
        ("1",
         "text=" & Encode ("""1""") & ",field=" & Encode ("""Artist"""),
         "artist_1/album_1/1 - song_1.mp3");

      --  FIXME: album, song
   end Test_Search_Results;

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
      Register_Routine (T, Test_Meta'Access, "Test_Meta");
      Register_Routine (T, Test_Get_File'Access, "Test_Get_File");
      Register_Routine (T, Test_Send_Notes'Access, "Test_Send_Notes");
      Register_Routine (T, Test_Search_Initial'Access, "Test_Search_Initial");
      Register_Routine (T, Test_Search_Results'Access, "Test_Search_Results");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use SMM.ID3.Tag_Lists;
      use Test_Utils;

      Db_File      : File_Type;
      Dir          : constant String := Current_Directory;

      function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String renames
        Ada.Strings.Unbounded.To_Unbounded_String;

   begin
      if Dir (Dir'Last - 5 .. Dir'Last) /= "\build" then
         raise SAL.Programmer_Error with "current_directory = " & Current_Directory;
      end if;

      Verbose := T.Debug > 0;

      if T.Debug /= 1 then
         Cleanup;

         Create_Directory ("tmp");

         Create (Db_File, Out_File, Db_File_Name);

         Put_Line (Db_File, "Songs. 1.File = artist_1/album_1/1 - song_1.mp3");
         Put_Line (Db_File, "Songs. 1.Category = vocal");
         Put_Line (Db_File, "Songs. 1.Play_Before = 2");
         Put_Line (Db_File, "Songs. 2.File = artist_1/album_1/2 - song_2.mp3");
         Put_Line (Db_File, "Songs. 2.Category = vocal");
         Put_Line (Db_File, "Songs. 2.Play_After = 1");
         Put_Line (Db_File, "Songs. 3.File = artist_1/album_1/03 The Dance #1.mp3");
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

         SMM.ID3.Create
           ("tmp/source/artist_1/album_1/1 - song_1.mp3",
              (SMM.ID3.Artist, +"artist_1") &
              (SMM.ID3.Album, +"album_1") &
              (SMM.ID3.Title, +"1 - song_1"));

         SMM.ID3.Create
           ("tmp/source/artist_1/album_1/2 - song_2.mp3",
              (SMM.ID3.Artist, +"artist_1") &
              (SMM.ID3.Album, +"album_1") &
              (SMM.ID3.Title, +"2 - song_2"));

         SMM.ID3.Create
           ("tmp/source/artist_1/album_1/03 The Dance #1.mp3",
              (SMM.ID3.Artist, +"artist_1") &
              (SMM.ID3.Album, +"album_1") &
              (SMM.ID3.Title, +"03 The Dance #1"));

         Create_Directory ("tmp/source/artist_2");
         Create_Directory ("tmp/source/artist_2/album_1");
         Create_Test_File ("tmp/source/artist_2/album_1/liner_notes.pdf");
         Create_Test_File ("tmp/source/artist_2/album_1/AlbumArt_1.jpg");

         SMM.ID3.Create
           ("tmp/source/artist_2/album_1/1 - song_1.mp3",
              (SMM.ID3.Artist, +"artist_2") &
              (SMM.ID3.Album, +"album_1") &
              (SMM.ID3.Title, +"1 - song_1"));

         SMM.ID3.Create
           ("tmp/source/artist_2/album_1/2 - song_2.mp3",
              (SMM.ID3.Artist, +"artist_2") &
              (SMM.ID3.Album, +"album_1") &
              (SMM.ID3.Title, +"2 - song_2"));

         SMM.ID3.Create
           ("tmp/source/artist_2/album_1/3 - song_3.mp3",
              (SMM.ID3.Artist, +"artist_2") &
              (SMM.ID3.Album, +"album_1") &
              (SMM.ID3.Title, +"3 - song_3"));

         Create_Directory ("tmp/source/Jason Castro [Deluxe] [+Video] [+Digital Booklet]");
         Create_Test_File ("tmp/source/Jason Castro [Deluxe] [+Video] [+Digital Booklet]/liner_notes.pdf");

         --  Server-side html, css
         Create_Directory ("tmp/server");
         Copy_File ("../test/search_initial.html", "tmp/server/search_initial.html");

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
