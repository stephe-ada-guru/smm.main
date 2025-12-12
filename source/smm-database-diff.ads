--  Abstract :
--
--  Compute difference between two databases, as SQL statements in JSON format.
--
--  Copyright (C) 2025  All Rights Reserved.
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

with SMM.Database_Remote;
with SAL.Progress;
with GNATCOLL.JSON;
package SMM.Database.Diff is

   type Diff_Type is tagged record
      Local_DB      : Database_Remote.Database_Access;
      Remote_DB     : Database_Remote.Database_Access;
      Sync_ID       : Song_ID;
      Show_Progress : SAL.Progress.Show_Progress_Type;
      Verbosity     : Integer;
   end record;
   --  Local_DB is nominally of type SMM.Database_Remote.Disk,
   --  Remote_DB is nominally of type SMM.Database_Remote.IP, but
   --  it doesn't actually matter.
   --
   --  For Init_Remote, Sync_ID tracks init progress.
   --
   --  For Sync_Incremental, Sync_ID is the max id in Song table after
   --  the last sync.

   procedure Init_Remote
     (Diff           : in out Diff_Type;
      Max_Changes    : in     Ada.Containers.Count_Type;
      Remote_Changes :    out GNATCOLL.JSON.JSON_Array);
   --  Collect Max_Changes records in Diff.DB_Local with ID >
   --  Diff.Sync_ID as JSON objects in Remote_Changes. The JSON
   --  objects are produced by To_* below.
   --
   --  Update Diff.Sync_ID to max id in changes.
   --
   --  Raises GNAT.Sockets.Socket_Error if remote closes socket.

   procedure Inc_Diff
     (Diff           : in     Diff_Type;
      Last_Sync_Time : in     Time_String;
      Local_Changes  :    out GNATCOLL.JSON.JSON_Array;
      Conflicts      :    out GNATCOLL.JSON.JSON_Array;
      Remote_Changes :    out GNATCOLL.JSON.JSON_Array);
   --  Compare Song tables in Diff.DB_Local, DB_Remote with Modified >
   --  Last_Sync_Time; store differences as JSON objects in
   --  Local_Changes, Conflicts, Remote_Changes. The JSON objects are
   --  produced by To_* below.
   --
   --  Raises Text_IO.End_Error if remote does not send enough data.
   --  Raises GNAT.Sockets.Socket_Error if remote closes socket.

   procedure Apply
     (Diff           : in out Diff_Type;
      Local_Changes  : in     GNATCOLL.JSON.JSON_Array;
      Remote_Changes : in     GNATCOLL.JSON.JSON_Array);
   --  Apply Local_Changes to Diff.Local_DB, Remote_Changes to
   --  Diff.Remote_DB.

   procedure Update_Sync_ID (Diff : in out Diff_Type);
   --  Set Diff.Sync_Data_IDs, Diff.Sync_Link_IDs to max IDs in
   --  Diff.Local_DB.

   ----------
   --  visible for unit tests

   function To_Conflict
     (Diff  : in Diff_Type;
      ID    : in Song_ID)
     return GNATCOLL.JSON.JSON_Value;

   function To_Conflict
     (Local_JSON  : in GNATCOLL.JSON.JSON_Value;
      Remote_JSON : in GNATCOLL.JSON.JSON_Value)
     return GNATCOLL.JSON.JSON_Value;

   function To_Insert (Item : in GNATCOLL.JSON.JSON_Value) return GNATCOLL.JSON.JSON_Value;

   function To_Update (Item : in GNATCOLL.JSON.JSON_Value) return GNATCOLL.JSON.JSON_Value;

   function To_Renumber
     (Old_ID : in Song_ID;
      New_ID : in Song_ID)
     return GNATCOLL.JSON.JSON_Value;

end SMM.Database.Diff;
