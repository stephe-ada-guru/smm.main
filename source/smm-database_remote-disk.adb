--  Abstract :
--
--  see spec.
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

package body SMM.Database_Remote.Disk is

   overriding procedure Apply
     (DB  : in out Database;
      Msg : in     GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;
      Operation : constant Apply_Operations := Operations'Value (Get (Msg, "Operation"));
   begin
      case Operation is
      when Insert =>
         DB.DB.Insert_JSON (Get (Msg, "Value"));

      when Update =>
         DB.DB.Update_JSON (Get (Msg, "Value"));

      when Renumber =>
         declare
            Old_ID    : constant Song_ID    := Get (Msg, "Old_ID");
            New_ID    : constant Song_ID    := Get (Msg, "New_ID");
            New_Value : constant JSON_Value := DB.DB.Get_JSON (Old_ID);
         begin
            --  Old_ID should not have been created in the local db, because it
            --  was used in the remote db. A later op will insert the remote
            --  values for Old_ID.
            DB.DB.Really_Delete (Old_ID);
            New_Value.Set_Field ("ID", New_ID);
            DB.DB.Insert_JSON (New_Value);

            --  We used to check Play_Before/Play_After here, but those are never
            --  set in real renumber use cases.
         end;
      end case;
   end Apply;

   overriding function Get_Last_ID (DB : in out Database) return Song_ID
   is begin
      return DB.DB.Last_ID;
   end Get_Last_ID;

   overriding function Get_JSON
     (DB : in Database;
      ID : in Song_ID)
     return GNATCOLL.JSON.JSON_Value
   is
      Cur : constant SMM.Database.Cursor := DB.DB.Find_ID (ID);
   begin
      if SMM.Database.Has_Element (Cur) then
         return Cur.Get_JSON;
      else
         return SMM.Invalid_Song_ID_JSON;
      end if;
   end Get_JSON;

   overriding function Get_Modified
     (DB       : in out Database;
      ID       : in     Song_ID;
      Modified : in     Time_String)
     return ID_Lists.List
   is begin
      return DB.DB.Get_Modified (ID, Modified);
   end Get_Modified;

   overriding function Get_Modified_With_Data
     (DB       : in out Database;
      ID       : in     Song_ID;
      Modified : in     Time_String)
     return GNATCOLL.JSON.JSON_Array
   is
      use GNATCOLL.JSON;
      Result : JSON_Array := Empty_Array;
   begin
      for I of DB.DB.Get_Modified (ID, Modified) loop
         Append (Result, DB.Get_JSON (I));
      end loop;
      return Result;
   end Get_Modified_With_Data;

   overriding function Get_New
     (DB        : in out Database;
      ID        : in     Song_ID;
      Max_Count : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last)
     return ID_Lists.List
   is begin
      return DB.DB.Get_New (ID, Max_Count);
   end Get_New;

   overriding function Index_Fields_Equal
     (DB        : in out Database;
      ID        : in     Song_ID;
      New_Value : in     GNATCOLL.JSON.JSON_Value)
     return Boolean
   is begin
      return DB.DB.Index_Fields_Equal (ID, New_Value);
   end Index_Fields_Equal;

end SMM.Database_Remote.Disk;
