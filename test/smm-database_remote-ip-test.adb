--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2016, 2018, 2019, 2020, 2026 Stephen Leake.  All Rights Reserved.
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
with AUnit.Checks;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNATCOLL.JSON.AUnit;
with SAL;
with SMM.Database.Diff;
with SMM.Database_Remote.Disk;
with SMM.Database_Remote.Do_Operation;
with Test_SMM;
package body SMM.Database_Remote.IP.Test is

   use GNATCOLL.JSON;

   Server : GNAT.OS_Lib.Process_Id;

   Jan_3_2000 : constant Time_String := "2000-01-03 00:00:00";
   Jan_4_2000 : constant Time_String := "2000-01-04 00:00:00";

   Song_1 : constant JSON_Value := Read
     ("{""ID"":1, " &
        """Modified"":""2000-01-02 00:00:00"", " &
        """Data"":" &
        "{""File_Name"":""Arthur/C./Clarke.mp3"", " &
        """Category"":""vocal"", " &
        """Album_Artist"":""Arthur"", " &
        """Album"":""C."", " &
        """Title"":""Clarke""}}");

   Song_2 : constant JSON_Value := Read
     ("{""ID"":2, " &
        """Modified"":""2000-01-02 00:00:00"", " &
        """Data"":" &
        "{""File_Name"":""Isaac/Asimov.mp3"", " &
        """Category"":""vocal"", " &
        """Album_Artist"":""Isaac"", " &
        --  no album
        """Title"":""Asimov""}}");

   Socket : GNAT.Sockets.Socket_Type;

   DB_2 : aliased SMM.Database.Database; -- Non-local for 'access

   function Open_Stream (Host : in String; Port : in GNAT.Sockets.Port_Type) return GNAT.Sockets.Stream_Access
   is
      use GNAT.Sockets;
      Hosts  : constant Host_Entry_Type := Get_Host_By_Name (Host);
      Status : Selector_Status;
   begin
      Create_Socket (Socket);

      --  This is supposed to raise Socket_Error for any errors. It seems to
      --  do that, but then bypasses _all_ exception handlers, and the
      --  process reports it was "killed".
      Connect_Socket
        (Socket,
         Sock_Addr_Type'(Family_Inet, Addresses (Hosts), Port),
         Timeout => 10.0,
         Status  => Status);

      case Status is
      when Completed =>
         null;
      when Expired | Aborted =>
         raise Socket_Error with "connect to '" & Host & "' timed out";
      end case;
      return Stream (Socket);
   end Open_Stream;

   procedure Close_Stream
   is
      use GNAT.Sockets;
   begin
      Shutdown_Socket (Socket);
      Close_Socket (Socket);
   end Close_Stream;

   Progress_Count  : Integer := 0;

   procedure Show_Progress
     (Label        : in String;
      Current, Max : in Integer)
   is begin
      --  Just test that the JSON message is handled properly
      if Verbosity > 1 then
         Ada.Text_IO.Put_Line ("got progress: " & Label & Integer'Image (Current) & Integer'Image (Max));
      end if;
      Progress_Count := Progress_Count + 1;
   end Show_Progress;

   ----------
   --  Test procedures

   procedure Test_Compute (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use SMM.Database.Diff; -- To_Insert etc
      use GNATCOLL.JSON.AUnit;

      Test : Test_Case renames Test_Case (T);

      Remote_DB : aliased SMM.Database_Remote.IP.Database (Open_Stream (Test.Server_IP.all, Test.Port), 0);

      Song_1a : constant JSON_Value := Read
        ("{""ID"":1, " &
           """Modified"":""2000-01-03 00:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Arthur/C./Clarke.mp3"", " &
           """Category"":""vocal, best"", " &
           """Album_Artist"":""Arthur"", " &
           """Album"":""C."", " &
           """Title"":""Clarke""}}");

      Song_2_Deleted : constant JSON_Value := Read ("{""ID"":2, ""Deleted"":""2000-01-03 00:00:00""}");
   begin
      --  This tests the parts of the remote protocol used by Compute (ie,
      --  functions in smm.database_remote.ip). See
      --  smm-database-diff-test_compute.adb for a test of the compute
      --  algorithm.

      Remote_DB.Init_Remote;

      Remote_DB.Apply (To_Insert (Song_1));
      Remote_DB.Apply (To_Insert (Song_2));

      Check ("Song_1", Remote_DB.Get_JSON (1), Song_1);
      Check ("Song_2", Remote_DB.Get_JSON (2), Song_2);
      Check ("last id", Remote_DB.Get_Last_ID, 2);

      Remote_DB.Apply
        (To_Update
           (Read ("{""ID"":1, ""Modified"":""2000-01-03 00:00:00"", " &
                    """Data"":" &
                    "{""Category"":""vocal, best""}}")));
      Check ("Song_1a", Remote_DB.Get_JSON (1), Song_1a);

      Remote_DB.Apply (To_Update (Song_2_Deleted));
      Check ("delete 2", Remote_DB.Get_JSON (2), Song_2_Deleted);

      Remote_DB.Send_Quit;

      Close_Stream;
   exception
   when E : others =>
      --  There doesn't seem to be a way to query DB.stream to see if it's
      --  open. And trying to shutdown nicely just obsures the original
      --  error message. Let the OS clean up.
      AUnit.Assertions.Assert (False, Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   end Test_Compute;

   procedure Test_Init (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use SMM.Database.Diff; -- To_Insert etc

      Test : Test_Case renames Test_Case (T);
      DB_1 : SMM.Database.Database;
   begin
      --  First get some data in Remote_DB
      DB_1.Open ("smm_test_1.db");
      DB_1.Insert_JSON (Song_1);
      DB_1.Insert_JSON (Song_2);
      DB_1.Close;

      declare
         Remote_DB : aliased SMM.Database_Remote.IP.Database (Open_Stream (Test.Server_IP.all, Test.Port), 0);

         procedure Test_Msg
           (Label    : in String;
            Expected : in JSON_Value)
         is
            use Ada.Text_IO;
            use GNATCOLL.JSON.AUnit;
            Msg : constant JSON_Value := Read (String (Network_String'Input (Remote_DB.Stream)));
         begin
            if Verbosity > 0 then
               Put_Line (Label & ": " & Msg.Write);
            end if;
            Check (Label, Msg, Expected);
            Send_Ack (Remote_DB.Stream);
         end Test_Msg;
      begin
         Remote_DB.Init_Compute
           (Action           => Init_Remote,
            Display_Progress => True,
            Last_Sync_Time   => Jan_3_2000,
            Last_Sync_ID     => 2);

         --  We are emulating the actual remote; test that we get the expected
         --  messages (progress and db insert).
         Test_Msg
           ("1",
            Read ("{""Operation"":""PROGRESS"",""Label"":""Init"",""Current"":2,""Max"":2}"));
         Test_Msg
           ("2",
            Read ("{""Operation"":""PROGRESS"",""Label"":""Apply remote changes"",""Current"":1,""Max"":2}"));

         Test_Msg ("insert data 1", To_Insert (Song_1));
         Test_Msg
           ("3",
            Read ("{""Operation"":""PROGRESS"",""Label"":""Apply remote changes"",""Current"":2,""Max"":2}"));
         Test_Msg ("insert data 2", To_Insert (Song_2));
         Test_Msg
           ("4",
            Read ("{""Operation"":""PROGRESS"",""Label"":""Apply remote changes"",""Current"":2,""Max"":2}"));

         Test_Msg
           ("5",
            Read ("{""Operation"":""PROGRESS"",""Label"":""Init"",""Current"":2,""Max"":2}"));

         Remote_DB.Send_Quit;

         Close_Stream;
      end;
   end Test_Init;

   procedure Test_Sync (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use GNATCOLL.JSON.AUnit;

      Test : Test_Case renames Test_Case (T);

      DB_1 : SMM.Database.Database;

      Song_3        : constant JSON_Value := Read
        ("{""ID"":3, " &
           """Modified"":""2000-01-02 00:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Joni/Miles/carie.mp3"", " &
           """Category"":""vocal"", " &
           """Album_Artist"":""Joni"", " &
           """Album"":""Miles"", " &
           """Title"":""Carie""}}");

      Song_4 : constant JSON_Value := Read
        ("{""ID"":4, " &
           """Modified"":""2000-01-04 00:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Joni/Miles/richard.mp3"", " &
           """Category"":""vocal"", " &
           """Album_Artist"":""Joni"", " &
           """Album"":""Miles"", " &
           """Title"":""Richard""}}");
   begin
      --  First get some data in DB_1, DB_2.
      --  DB_2 is the phone
      DB_1.Open ("smm_test_1.db");
      DB_2.Open ("smm_test_2.db");

      DB_1.Insert_JSON (Song_1);
      DB_1.Insert_JSON (Song_2);
      DB_1.Insert_JSON (Song_3);

      DB_2.Insert_JSON (Song_1);
      DB_2.Insert_JSON (Song_2);
      DB_2.Insert_JSON (Song_3);

      --  2000-01-03: Sync

      --  2000-01-04: Song_1 modified in DB_1
      --              Song_2 modified in DB_2
      --              Song_3 modified in both
      --              Song_4 new in DB_1
      DB_1.Update (DB_1.Find_ID (1), Category => "vocal, best", Modified => Jan_4_2000);
      DB_2.Update (DB_2.Find_ID (2), Category => "vocal, protest", Modified => Jan_4_2000);
      DB_1.Update (DB_1.Find_ID (3), Category => "vocal, best", Modified => Jan_4_2000);
      DB_2.Update (DB_2.Find_ID (3), Category => "vocal, protest", Modified => Jan_4_2000);
      DB_1.Insert_JSON (Song_4);

      DB_1.Close;
      --  DB_2 left open for Local_DB

      declare
         Stream : constant GNAT.Sockets.Stream_Access := Open_Stream (Test.Server_IP.all, Test.Port);
         Remote_DB : SMM.Database_Remote.Database_Access := new SMM.Database_Remote.IP.Database
           (Stream, 0); --  Remote server uses DB_1

         Local_DB : aliased SMM.Database_Remote.Disk.Database (DB_2'Access);

         Msg                : JSON_Value;
         Conflicts          : JSON_Array;
         Expected_Conflicts : JSON_Array;

         procedure Cleanup
         is
            use SMM.Database;
         begin
            Close_Stream;
            DB_2.Close;
            SMM.Database_Remote.Free (Remote_DB);
         end Cleanup;

      begin
         --  Test is that sync finishes; this proves the remote protocol
         --  is correctly implemented. See
         --  smm-database-diff-test_compute.adb for a test of
         --  the sync algorithm.

         --  We are emulating the phone; remote is the laptop.
         SMM.Database_Remote.IP.Database (Remote_DB.all).Init_Compute
           (Action           => Sync_Incremental,
            Display_Progress => True,
            Last_Sync_Time   => Jan_3_2000,
            Last_Sync_ID     => 3);

         begin
            loop
               --  exit on Quit_Operation
               Msg := Read (String (Network_String'Input (Stream)));

               if Verbosity > 0 then
                  Ada.Text_IO.Put_Line (Msg.Write);
               end if;

               Do_Operation (Msg, Stream, Local_DB'Unchecked_Access, Conflicts, Show_Progress'Access);
            end loop;
         exception
         when Quit_Operation =>
            null;
         end;

         Cleanup;
         Check ("progress count", Progress_Count, 23);

         declare
            Song_3a        : constant JSON_Value := Read
              ("{""ID"":1, " &
                 """Modified"":""2000-01-02 00:00:00"", " &
                 """Data"":" &
                 "{""File_Name"":""Joni/Miles/carie.mp3"", " &
                 """Category"":""vocal, best"", " &
                 """Album_Artist"":""Joni"", " &
                 """Album"":""Miles"", " &
                 """Title"":""Carie""}}");

            Song_3b        : constant JSON_Value := Read
              ("{""ID"":1, " &
                 """Modified"":""2000-01-02 00:00:00"", " &
                 """Data"":" &
                 "{""File_Name"":""Joni/Miles/carie.mp3"", " &
                 """Category"":""vocal, protest"", " &
                 """Album_Artist"":""Joni"", " &
                 """Album"":""Miles"", " &
                 """Title"":""Carie""}}");


         begin
            --  Remote/local are swapped on the server
            Append (Expected_Conflicts, SMM.Database.Diff.To_Conflict (Local_JSON => Song_3a, Remote_JSON => Song_3b));
            Check ("conflicts", Conflicts, Expected_Conflicts);
         end;
      exception
      when others =>
         Cleanup;
         raise;
      end;
   end Test_Sync;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("smm-database_remote-ip-test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Compute'Access, "Test_Compute");
      Register_Routine (T, Test_Init'Access, "Test_Init");
      Register_Routine (T, Test_Sync'Access, "Test_Sync");
   end Register_Tests;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      --  We start the server fresh for each test routine
      if T.Debug /= 1 then
         Test_SMM.Empty_Database_1;
         Test_SMM.Empty_Database_2;
      end if;

      if T.Debug /= 1 then
         declare
            use GNAT.OS_Lib;
            Args : String_List (1 .. 2) :=
              (1 => new String'("smm_test_1.config"),
               2 => null);

            Args_Last : Integer := 1;
         begin
            if T.Debug = 2 then
               Args (2) := new String'("2");
               Args_Last := 2;
            end if;

            Server := GNAT.OS_Lib.Non_Blocking_Spawn ("smm-db_sync_server.exe", Args (1 .. Args_Last));
            if Server = Invalid_Pid then
               raise SAL.Initialization_Error with "could not spawn smm-db_sync_server.exe";
            end if;
            delay 1.0; -- give server time to create listening socket
            if T.Debug = 2 then
               Ada.Text_IO.Put_Line ("spawned server");
            end if;
         end;
      end if;
   end Set_Up;

   overriding procedure Tear_Down (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      GNAT.OS_Lib.Kill (Server, Hard_Kill => True);
   end Tear_Down;

end SMM.Database_Remote.IP.Test;
