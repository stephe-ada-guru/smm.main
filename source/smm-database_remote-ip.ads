--  Abstract :
--
--  Access to a remote database via books-remote_server.adb over an
--  Internet connection.
--
--  Copyright (C) 2016, 2018 - 2019, 2025, 2026  All Rights Reserved.
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

with Ada.Unchecked_Deallocation;
with GNAT.Sockets;
package SMM.Database_Remote.IP is

   type Database
     (Stream    : GNAT.Sockets.Stream_Access;
      Verbosity : Integer)
     is new Database_Remote.Database with null record;
   --  Stream is assumed to be connected to a host
   --  running either SMM.Central_Server or a SMM User Interface.

   type Database_Access is access all Database;
   --  Can't declare Free before any primitive ops.

   procedure Init_Compute
     (DB               : in out Database;
      Action           : in     Actions;
      Display_Progress : in     Boolean;
      Last_Sync_Time   : in     Time_String;
      Last_Sync_ID     : in     Song_ID);
   --  Send setup message for host of DB in Compute role (thus local
   --  in Remote role), wait for acknowledge.
   --
   --  Last_Sync_Time, Last_Sync_ID should be the time, max ID of the last sync.

   procedure Init_Remote (DB : in out Database);
   --  Send setup message for host of DB in Remote role (thus local in
   --  Compute role), wait for acknowledge.

   procedure Send_Error (Stream : in GNAT.Sockets.Stream_Access; Msg : in String);
   --  Send a Nack Status message containing Msg; do not wait for response.

   procedure Send_Data (Stream : in GNAT.Sockets.Stream_Access; Data : in GNATCOLL.JSON.JSON_Value);
   --  Send an Ack Status message containing Data; do not wait for response.

   procedure Send_Ack (Stream : in GNAT.Sockets.Stream_Access);
   --  Send an Ack Status message with no Data.

   procedure Send_Progress
     (DB           : in out Database;
      Label        : in     String;
      Current, Max : in     Integer);
   --  Send a Progress message; wait for an Ack.

   procedure Send_Messages
     (DB        : in out Database;
      Messages  : in     GNATCOLL.JSON.JSON_Array;
      Verbosity : in     Integer);
   --  Send several messages; wait for an Ack for each.

   procedure Send_Quit (DB : in out Database);
   --  Tell remote we are done, wait for acknowledge.

   overriding function Get_JSON
     (DB : in Database;
      ID : in Song_ID)
     return GNATCOLL.JSON.JSON_Value;

   overriding function Get_Last_ID (DB : in out Database) return Song_ID;

   overriding function Get_Modified
     (DB       : in out Database;
      ID       : in     Song_ID;
      Modified : in     Time_String)
     return ID_Lists.List;

   overriding function Get_New
     (DB        : in out Database;
      ID        : in     Song_ID;
      Max_Count : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last)
     return ID_Lists.List;

   overriding function Index_Fields_Equal
     (DB        : in out Database;
      ID        : in     Song_ID;
      New_Value : in     GNATCOLL.JSON.JSON_Value)
     return Boolean;

   overriding procedure Apply
     (DB  : in out Database;
      Msg : in     GNATCOLL.JSON.JSON_Value);

   procedure Free is new Ada.Unchecked_Deallocation (Database, Database_Access);

end SMM.Database_Remote.IP;
