--  Abstract :
--
--  Main program to accept socket connections and either run
--  SMM.Database.Diff algorithms or respond to
--  SMM.Database_Remote.Operations.
--
--  Copyright (C) 2016 - 2020, 2025, 2026 Stephen Leake All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Exceptions.Traceback;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with GNAT.Traceback.Symbolic;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with SAL.Config_Files.Port;
with SAL.Progress;
with SMM.DB_Sync; use SMM.DB_Sync;
with SMM.Database.Diff;
with SMM.Database_Remote.Do_Operation;
with SMM.Database_Remote.IP;
with System;
procedure SMM.DB_Sync_Server
is
   use SMM.Database;
   use SMM.Database_Remote;

   procedure Usage
   is begin
      Put_Line ("usage: smm-db_sync_server <config filename> [verbosity]");
      Put_Line ("config file contains:");
      Put_Line ("Database_File");
      Put_Line ("Server_IP");
      Put_Line ("Server_Port");
   end Usage;

   Exit_Messages : exception;

   Verbosity : Integer := 0;

   Address  : Sock_Addr_Type;
   Listener : Socket_Type;
   Server   : Socket_Type;
   Stream   : Stream_Access;

   Msg                : JSON_Value;
   Local_DB_File_Name : Ada.Strings.Unbounded.Unbounded_String;

   Config : SAL.Config_Files.Configuration_Type;

   Init_Chunk_Size : constant := 100; -- or get from config

   Role             : Roles;
   Compute_Action   : Actions;
   Client_Host_Name : Ada.Strings.Unbounded.Unbounded_String;
   Send_Progress : Boolean := False;

   procedure Get_Msg
   --  Raises Socket_Error or End_Error is socket is closed (by peer).
   is
      Msg_String : constant String := String (Network_String'Input (Stream));
   begin
      Msg := Read (Msg_String);

      if Verbosity > 0 then
         Put_Line ("remote: '" & Msg_String & "'");
      end if;
   end Get_Msg;

begin
   declare
      use Ada.Command_Line;
      use SAL.Config_Files;
      use SAL.Config_Files.Port;
   begin
      case Argument_Count is
      when 1 | 2 =>
         Open (Config, Argument (1), Read_Only => False);

         Local_DB_File_Name := +Read (Config, "Database_File", Missing_Key => Raise_Exception);
         Disk_DB.Open (-Local_DB_File_Name);

         if Argument_Count = 2 then
            Verbosity := Integer'Value (Argument (2));
         end if;

      when others =>
         Usage;
         Set_Exit_Status (Failure);
         raise SAL.Parameter_Error;
      end case;

      --  Get_Host_By_Name includes VMware IP addresses, which are not
      --  useful for sync from outside this box. But there doesn't seem to
      --  be a way to tell which ones those are from here. So we get the
      --  address to use from the config file.
      --  FIXME: not on VMWare anymore
      Address.Addr := Inet_Addr (Read (Config, "Server_IP", Missing_Key => Raise_Exception));
      Address.Port := Read (Config, "Server_Port", Default => SMM.Database_Remote.IP_Port);
   end;

   if Verbosity > 0 then
      Put_Line ("using database " & (-Local_DB_File_Name));
      Put_Line ("using config file " & SAL.Config_Files.Writeable_File_Name (Config));
   end if;

   Create_Socket (Listener);
   Set_Socket_Option (Listener, Socket_Level, (Reuse_Address, True));

   Bind_Socket (Listener, Address);

   Connections :
   loop
      --  Exit on Ctrl_C
      if Verbosity > 0 then
         Put_Line ("listening on " & Host_Name & "(" & Image (Address) & ")");
      end if;

      Role := Waiting;

      Listen_Socket (Listener);
      Accept_Socket (Listener, Server, Address);

      begin
         Client_Host_Name := +Official_Name (Get_Host_By_Address (Address.Addr));
      exception
      when Host_Error =>
         --  Official_Name for Address not known. We need that to access cached
         --  info, so we use the address image.
         --
         --  This happened at Bakery Lofts; Paxio upstream router, local
         --  Netgear wifi router.
         Client_Host_Name := +Image (Address.Addr);
      end;

      if Verbosity > 0 then
         Put_Line ("accepted connection from " & Image (Address) & ": " & (-Client_Host_Name));
      end if;

      Stream := GNAT.Sockets.Stream (Server);

      --  We don't spawn a task here, because database access is not
      --  thread-safe. If another remote attempts to connect while we
      --  are busy, they'll have to try again later. This could be
      --  changed by wrapping SMM.Database in a task.
      Messages :
      loop
         begin
            case Role is
            when Waiting =>
               Get_Msg;

               if Msg.Has_Field (Prelude_Messages'Image (Database_Remote.Role)) then
                  Role := Roles'Value (Msg.Get (Prelude_Messages'Image (Database_Remote.Role)));
               else
                  raise Exit_Messages with "missing Role in prelude message";
               end if;

               if Role = Compute then
                  if Msg.Has_Field (Prelude_Messages'Image (Action)) then
                     Compute_Action := Actions'Value (Msg.Get (Prelude_Messages'Image (Action)));
                  else
                     raise Exit_Messages with "missing Compute_Action in prelude message";
                  end if;
               end if;

               Send_Progress :=
                 (if Msg.Has_Field (Prelude_Messages'Image (Display_Progress))
                  then Boolean'Value (Msg.Get (Prelude_Messages'Image (Display_Progress)))
                  else False);

               SMM.Database_Remote.IP.Send_Ack (Stream);

            when Compute =>
               case Compute_Action is
               when Init_Remote | Resume_Init_Remote =>
                  --  We assume the user has re-initialized the db on the remote.
                  --
                  --  If Resume_Init_Remote, a previous Init_Remote failed; don't repeat
                  --  the data sent successfully then.

                  declare
                     Remote_DB : SMM.Database_Remote.IP.Database_Access :=
                       new SMM.Database_Remote.IP.Database (Stream, Verbosity);

                     procedure Do_Send_Progress
                       (Label        : in String;
                        Current, Max : in Integer)
                     is begin
                        Remote_DB.Send_Progress (Label, Current, Max);
                     end Do_Send_Progress;

                     --  We don't use Sync_Time or Sync_ID for Init_Remote; we use Sync_ID
                     --  for Resume_Init_Remote

                     Sync_ID : Integer :=
                       (if Compute_Action = Resume_Init_Remote
                        then Msg.Get (Prelude_Messages'Image (Database_Remote.Sync_ID))
                        else Null_ID);

                     Diff : SMM.Database.Diff.Diff_Type :=
                       (Local_DB      => Local_DB'Access,
                        Remote_DB     => Database_Remote.Database_Access (Remote_DB),
                        Show_Progress => (if Send_Progress then Do_Send_Progress'Unrestricted_Access else null),
                        Verbosity     => Verbosity);

                     function Get_Count return Integer
                     is
                        Last_ID : constant Integer := Diff.Local_DB.Get_Last_ID;
                     begin
                        --  ignores deleted
                        if Last_ID = Invalid_Song_ID then
                           return 0;
                        elsif Sync_ID = Invalid_Song_ID then
                           return Last_ID;
                        else
                           return Last_ID - Sync_ID;
                        end if;
                     end Get_Count;

                     Progress : SAL.Progress.Progress_Type
                       (Max       => Get_Count,
                        Intervals => 100,
                        Show      => Diff.Show_Progress);

                     Local_Changes  : constant GNATCOLL.JSON.JSON_Array := GNATCOLL.JSON.Empty_Array;
                     Remote_Changes : GNATCOLL.JSON.JSON_Array;
                  begin
                     --  No exception handler in these loops or blocks; if the socket dies,
                     --  client must restart operation; it will resume with the chunk that
                     --  failed.

                     Progress.Label ("Init");

                     loop
                        Diff.Init_Remote (Sync_ID, Init_Chunk_Size, Remote_Changes);

                        exit when Length (Remote_Changes) = 0;

                        Progress.Next (Length (Remote_Changes));

                        Diff.Apply (Local_Changes, Remote_Changes, Show_Progress => False);

                        Sync_ID := Diff.Remote_DB.Get_Last_ID;
                     end loop;

                     Progress.Complete;
                     Remote_DB.Send_Quit;
                     SMM.Database_Remote.IP.Free (Remote_DB);
                  end;

               when Sync_Incremental =>
                  declare
                     use SMM.Database.Diff;

                     Remote_DB       : SMM.Database_Remote.IP.Database_Access :=
                       new SMM.Database_Remote.IP.Database (Stream, Verbosity);

                     procedure Do_Send_Progress
                       (Label        : in String;
                        Current, Max : in Integer)
                     is begin
                        Remote_DB.Send_Progress (Label, Current, Max);
                     end Do_Send_Progress;

                     Diff : SMM.Database.Diff.Diff_Type :=
                       (Local_DB      => Local_DB'Access,
                        Remote_DB     => Database_Remote.Database_Access (Remote_DB),
                        Show_Progress => (if Send_Progress then Do_Send_Progress'Unrestricted_Access else null),
                        Verbosity     => Verbosity);

                     Local_Changes  : GNATCOLL.JSON.JSON_Array;
                     Conflicts      : GNATCOLL.JSON.JSON_Array;
                     Remote_Changes : GNATCOLL.JSON.JSON_Array;
                     Sync_ID        : constant Song_ID := Msg.Get (Prelude_Messages'Image (Database_Remote.Sync_ID));
                     Sync_Time      : constant Time_String :=
                       Msg.Get (Prelude_Messages'Image (Database_Remote.Sync_Time));
                  begin
                     --  No exception handler in these loops or blocks; if the socket dies,
                     --  client must restart operation; it will repeat all compares
                     Diff.Inc_Diff (Sync_Time, Sync_ID, Local_Changes, Conflicts, Remote_Changes);

                     if Send_Progress then
                        --  We assume if remote can display progress, it can display
                        --  conflicts.
                        Remote_DB.Send_Messages (Conflicts, Verbosity);
                     end if;

                     Diff.Apply (Local_Changes, Remote_Changes, Show_Progress => True);

                     Remote_DB.Send_Quit;
                     SMM.Database_Remote.IP.Free (Remote_DB);
                  end;
               end case;

               exit Messages;

            when Remote =>
               Get_Msg;

               --  Used for unit testing; actual remote is in Kotlin for Android
               declare
                  Conflicts : JSON_Array; -- ignored, since we have no User Interface
               begin
                  Do_Operation (Msg, Stream, Local_DB'Access, Conflicts, Show_Progress => null);
               exception
               when Quit_Operation =>
                  exit Messages;

               when Bad_Operation =>
                  if Verbosity > 0 then
                     Put_Line ("bad operation: " & Msg.Write);
                  end if;
                  SMM.Database_Remote.IP.Send_Error (Stream, "bad operation");
                  raise Exit_Messages;
               end;
            end case;

         exception
         when E : Exit_Messages =>
            if Verbosity > 0 then
               Put_Line (Ada.Exceptions.Exception_Name (E) & ":" & Ada.Exceptions.Exception_Message (E));
               --  FIXME: need log file
            end if;

         when E : End_Error | Socket_Error =>
            --  Peer closed socket
            if Verbosity > 0 then
               Put_Line (Ada.Exceptions.Exception_Name (E) & ":" & Ada.Exceptions.Exception_Message (E));
            end if;
            exit Messages;

         when E : SAL.Invalid_Operation =>
            --  Error was detected by remote; it will have reported to the UI
            Put_Line (Ada.Exceptions.Exception_Name (E) & ":" & Ada.Exceptions.Exception_Message (E));
            exit Messages;

         when E : others =>
            declare
               use Ada.Exceptions;
               Err_Msg : constant String := Exception_Name (E) & ": " & Exception_Message (E);
            begin
               if Verbosity > 0 then
                  Put_Line (Standard_Error, Err_Msg);
                  Put_Line (Standard_Error,
                            GNAT.Traceback.Symbolic.Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (E)));
               end if;

               SMM.Database_Remote.IP.Send_Error (Stream, Err_Msg);
            exception
            when others =>
               --  Probably "socket reset by peer"; allow retry connection.
               null;
            end;
            exit Messages;
         end;
      end loop Messages;

      --  Safely shutdown Server socket
      begin
         Shutdown_Socket (Server);
      exception
      when Socket_Error =>
         --  already shut down
         null;
      end;
      begin
         Close_Socket (Server);
      exception
      when Socket_Error =>
         --  already closed
         null;
      end;
   end loop Connections;

exception
when E : others =>
   Put_Line
     (Standard_Error,
      "exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E));
   Put_Line
     (Standard_Error,
      GNAT.Traceback.Symbolic.Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (E)));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end SMM.DB_Sync_Server;
