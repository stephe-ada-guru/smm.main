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
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL.Calendar_More.AUnit;
with SMM.Database;
with SMM.ID3.AUnit;
with Test_Utils;
package body Test_Server is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   DB_File_Name : constant String := "tmp/smm.db";

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

      --  Song order depends on random engine. This is correct for GNAT GPL
      --  2017.
      Expected : constant String :=
        "artist_2/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
          "artist_1/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
          "artist_1/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
          "artist_2/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
          "artist_2/album_1/3 - song_3.mp3" & ASCII.CR & ASCII.LF;
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
      use SMM;
      use SMM.ID3;
      use Test_Utils;

      Test : Test_Case renames Test_Case (T);

      procedure Check_One
        (Directory : in String;
         Filename  : in String;
         Mime      : in String;
         Tags      : in SMM.ID3.Tag_Lists.List := SMM.ID3.Tag_Lists.Empty_List)
      is
         use SAL.Calendar_More.AUnit;
         use SMM.ID3.AUnit;

         URL : constant String := "http://" & Test.Server_IP.all & ":" & Server_Port & "/" & Directory &
           Encode (Filename);

         Response : constant Data   := AWS.Client.Get (URL);
         Msg      : constant String := Message_Body (Response);
         DB       : SMM.Database.Database;
         I        : SMM.Database.Cursor;
      begin
         Check (Filename & ".mode", Mode (Response), Message); --  Not clear why this is "message", not "file"
         Check (Filename & ".mime", Content_Type (Response), Mime);
         if Mime = "audio/mpeg" then
            Check (Filename & ".file content", Msg, Tags);
            DB.Open (DB_File_Name);
            I := DB.Find_File_Name (Directory & Filename);
            Check
              (Filename & ".last_downloaded",
               Ada.Calendar.Formatting.Value (I.Last_Downloaded),
               Ada.Calendar.Clock,
               Tolerance => 60.0);
         else
            Check (Filename & ".file content", Msg, "body: tmp/source/" & Directory & Filename & CRLF);
         end if;

      exception
      when Ada.IO_Exceptions.Use_Error =>
         Ada.Text_IO.Put_Line ("USE_ERROR: " & Filename);
         raise;
      end Check_One;

   begin
      Check_One
        ("artist_1/album_1/", "AlbumArt_1.jpg", "image/jpeg");

      Check_One
        ("artist_1/album_1/", "1 - song_1.mp3", "audio/mpeg",
           (Artist, +"artist_1") &
           (Album, +"album_1") &
           (Title, +"1 - song_1"));

      Check_One ("artist_1/album_1/", "liner_notes.pdf", "application/pdf");
      Check_One
        ("artist_1/album_1/", "03 The Dance #1.mp3", "audio/mpeg",
         (Artist, +"artist_1") &
           (Album, +"album_1") &
           (Title, +"03 The Dance #1"));
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

      Good_File_Name : constant String := "../source/search_initial.html";
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
      use AUnit.Assertions;

      Test : Test_Case renames Test_Case (T);

      procedure Check_One
        (Query            : in String;
         Expected_Message : in String)
      is
         use AUnit.Checks;

         URL      : constant String := "http://" & Test.Server_IP.all & ":" & Server_Port & "/search?" & Query;
         Response : constant Data   := AWS.Client.Get (URL);
      begin
         declare
            use all type AWS.Messages.Status_Code;
            Computed : constant String := Message_Body (Response);
         begin
            case Status_Code (Response) is
            when S200 | -- ok
              S405 -- bad param
              =>
               Check ("content", Computed, Expected_Message);

            when others =>
               Assert (False, Computed);
            end case;
         end;
      end Check_One;

   begin
      --  Test response to search submissions
      --  FIXME: all wrong; waiting for improved display
      Check_One
        ("text=" & Encode ("artist 1") & "&field=" & Encode ("artist"),
         "artist_1/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
           "artist_1/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
           "artist_1/album_1/03 The Dance #1.mp3" & ASCII.CR & ASCII.LF);

      Check_One
        ("text=" & Encode ("1") & "&field=" & Encode ("artist"),
         "artist_1/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF &
           "artist_1/album_1/2 - song_2.mp3" & ASCII.CR & ASCII.LF &
           "artist_1/album_1/03 The Dance #1.mp3" & ASCII.CR & ASCII.LF);

      Check_One
        ("text=" & Encode ("1") & "&field=" & Encode ("album") &
           "&text=" & Encode ("1") & "&field=" & Encode ("title"),
         "artist_1/album_1/1 - song_1.mp3" & ASCII.CR & ASCII.LF);

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
      use SMM;
      use SMM.ID3;
      use SMM.ID3.Tag_Lists;
      use Test_Utils;

      DB  : SMM.Database.Database;
      Dir : constant String := Current_Directory;
   begin
      if Dir (Dir'Last - 5 .. Dir'Last) /= "\build" then
         raise SAL.Programmer_Error with "current_directory = " & Current_Directory;
      end if;

      Verbose := T.Debug > 0;

      if T.Debug /= 1 then
         Cleanup;

         Create_Directory ("tmp");

         Create_Empty_DB (DB_File_Name);

         DB.Open (DB_File_Name);

         DB.Insert (1, "artist_1/album_1/1 - song_1.mp3", "vocal", "artist 1", "album 1", "1 - song_1");
         DB.Write_Play_Before_After (1, 2);
         DB.Insert (2, "artist_1/album_1/2 - song_2.mp3", "vocal", "artist 1", "album 1", "2 - song_2");
         DB.Insert
           (3, "artist_1/album_1/03 The Dance #1.mp3", "instrumental", "artist 1", "album 1", "03 The Dance #1");
         DB.Insert (4, "artist_2/album_1/1 - song_1.mp3", "vocal", "artist 2", "album 1", "1 - song_1");
         DB.Insert (5, "artist_2/album_1/2 - song_2.mp3", "vocal", "artist 2", "album 1", "2 - song_2");
         DB.Insert (6, "artist_2/album_1/3 - song_3.mp3", "vocal", "artist 2", "album 1", "3 - song_3");

         DB.Finalize;

         Create_Directory ("tmp/source");
         Create_Directory ("tmp/source/artist_1");
         Create_Directory ("tmp/source/artist_1/album_1");
         Create_Test_File ("tmp/source/artist_1/album_1/liner_notes.pdf");
         Create_Test_File ("tmp/source/artist_1/album_1/AlbumArt_1.jpg");

         Create
           ("tmp/source/artist_1/album_1/1 - song_1.mp3",
            (Artist, +"artist_1") &
              (Album, +"album_1") &
              (Title, +"1 - song_1"));

         Create
           ("tmp/source/artist_1/album_1/2 - song_2.mp3",
            (Artist, +"artist_1") &
              (Album, +"album_1") &
              (Title, +"2 - song_2"));

         Create
           ("tmp/source/artist_1/album_1/03 The Dance #1.mp3",
            (Artist, +"artist_1") &
              (Album, +"album_1") &
              (Title, +"03 The Dance #1"));

         Create_Directory ("tmp/source/artist_2");
         Create_Directory ("tmp/source/artist_2/album_1");
         Create_Test_File ("tmp/source/artist_2/album_1/liner_notes.pdf");
         Create_Test_File ("tmp/source/artist_2/album_1/AlbumArt_1.jpg");

         Create
           ("tmp/source/artist_2/album_1/1 - song_1.mp3",
            (Artist, +"artist_2") &
              (Album, +"album_1") &
              (Title, +"1 - song_1"));

         Create
           ("tmp/source/artist_2/album_1/2 - song_2.mp3",
            (Artist, +"artist_2") &
              (Album, +"album_1") &
              (Title, +"2 - song_2"));

         Create
           ("tmp/source/artist_2/album_1/3 - song_3.mp3",
            (Artist, +"artist_2") &
              (Album, +"album_1") &
              (Title, +"3 - song_3"));

         Create_Directory ("tmp/source/Jason Castro [Deluxe] [+Video] [+Digital Booklet]");
         Create_Test_File ("tmp/source/Jason Castro [Deluxe] [+Video] [+Digital Booklet]/liner_notes.pdf");

         --  Server-side html, css
         Create_Directory ("tmp/server");
         Copy_File ("../source/search_initial.html", "tmp/server/search_initial.html");

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
