--  Abstract :
--
--  Access to remote database, for sync
--
--  All times sent over a network connection are in UTC (Greenwich time zone).
--
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

with Ada.Finalization;
with Ada.Streams;
with GNAT.Sockets;
with GNATCOLL.JSON;
package SMM.Database_Remote is

   IP_Port : GNAT.Sockets.Port_Type := 16#9001#; -- 36865

   type Roles is (Waiting, Compute, Remote);
   subtype Active_Roles is Roles range Compute .. Remote;
   --  Waiting waits for a connection.
   --
   --  Compute runs Actions.
   --
   --  Remote responds to Operations.

   type Prelude_Messages is
     (Display_Progress, Role, Action, Sync_Time, Sync_ID);

   type Actions is (Init_Remote, Resume_Init_Remote, Sync_Incremental);
   subtype Init_Actions is Actions range Init_Remote .. Resume_Init_Remote;

   type Operations is
     (Quit,
      Get,
      Get_Last_ID,
      Get_Modified,           -- ID, Modified; return ids > ID and modified > Modified
      Get_Modified_With_Data, -- ID, Modified; return full JSON records (same as Get_JSON) in ID order
      Get_New,
      Conflict,
      Progress,
      Insert,
      Insert_Batch,
      Update,  -- Also used for "delete", since that just updates the Deleted field.
      Update_Batch,
      Renumber -- used to resolve add/add conflicts
     );
   subtype Apply_Operations is Operations range Insert .. Renumber;
   --  Operations on remote and local databases.

   type Ack_Nack is (Ack, Nack);
   --  Response from remote server, in JSON Status field.
   --  If Ack, there is also a Data field.
   --  If Nack, there is also a Message field.

   Bad_Operation : exception;
   --  Raised when a JSON message does not have a recognized Operations field.

   Quit_Operation : exception;
   --  Raised when a Quit operation is processed.

   type Network_String is new String;

   function Input_Network_String
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Network_String;
   --  Read string bounds as 32 bit big-byte-endian integers, read the
   --  indicated number of 8 bit characters.
   --
   --  If not all bytes are read (as determined by the bounds), raises
   --  End_Error.

   procedure Output_Network_String
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Network_String);
   --  Write string bounds as 32 bit big-byte-endian integers, write
   --  Item'Length 8 bit characters.

   for Network_String'Input use Input_Network_String;
   for Network_String'Output use Output_Network_String;

   type Database is abstract new Ada.Finalization.Limited_Controlled
   with null record;
   type Database_Access is access all Database'Class;
   procedure Free (Pointer : in out Database_Access);

   function Get_JSON
     (DB : in Database; ID : in Song_ID) return GNATCOLL.JSON.JSON_Value
   is abstract;
   --  Get a JSON representation of the data at remote ID.
   --  See smm-database.ads Get_JSON for format.
   --
   --  Returns Invalid_ID_JSON if ID is invalid.
   --
   --  Raises SAL.Invalid_Operation if remote responds with Nack.

   function Get_Last_ID (DB : in out Database) return Song_ID is abstract;
   --  Invalid_ID indicates no records.
   --
   --  Raises SAL.Invalid_Operation if remote responds with Nack.

   function Get_Modified
     (DB : in out Database; ID : in Song_ID; Modified : in Time_String)
      return ID_Lists.List
   is abstract;
   --  Get a list of Song IDs with Song.ID <= ID and Song.Modified |
   --  Song.Deleted > Modified.
   --
   --  Result is in ID order.

   function Get_Modified_With_Data
     (DB : in out Database; ID : in Song_ID; Modified : in Time_String)
      return GNATCOLL.JSON.JSON_Array
   is abstract;
   --  Get full JSON records (same format as Get_JSON) for all songs with
   --  Song.ID <= ID and Song.Modified | Song.Deleted > Modified.
   --
   --  Result is in ID order.

   function Get_New
     (DB        : in out Database;
      ID        : in Song_ID;
      Max_Count : in Ada.Containers.Count_Type :=
        Ada.Containers.Count_Type'Last) return ID_Lists.List
   is abstract;
   --  Get a list of up to Max_Count Song IDs > ID.
   --
   --  Result is in ID order.

   function Index_Fields_Equal
     (DB        : in out Database;
      ID        : in Song_ID;
      New_Value : in GNATCOLL.JSON.JSON_Value) return Boolean
   is abstract;
   --  If this returns True, Insert (New_Value) would raise a database
   --  exception for colliding values. If it returns False, Insert
   --  will not raise an exception.

   procedure Apply (DB : in out Database; Msg : in GNATCOLL.JSON.JSON_Value)
   is abstract;
   --  Apply Msg to DB. Msg must be from SMM.Database.Diff.Inc_Diff
   --  Local_Changes (for a Disk DB) or Remote_Changes (for an IP
   --  DB).
   --
   --  Raises SAL.Invalid_Operation if remote responds with Nack.

end SMM.Database_Remote;
