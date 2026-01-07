--  Abstract :
--
--  see spec
--
--  Copyright (C) 2016, 2018 - 2020, 2025, 2026  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL;
package body SMM.Database.Diff is

   function To_Conflict (Diff  : in Diff_Type; ID : in Song_ID) return GNATCOLL.JSON.JSON_Value
   is
      use SMM.Database_Remote;
      use GNATCOLL.JSON;
      Local_JSON  : constant JSON_Value := Diff.Local_DB.Get_JSON (ID);
      Remote_JSON : constant JSON_Value := Diff.Remote_DB.Get_JSON (ID);
      Result      : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("Operation", Operations'Image (Conflict));
      Result.Set_Field ("Local_Value", Local_JSON);
      Result.Set_Field ("Remote_Value", Remote_JSON);

      return Result;
   end To_Conflict;

   function To_Conflict
     (Local_JSON  : in GNATCOLL.JSON.JSON_Value;
      Remote_JSON : in GNATCOLL.JSON.JSON_Value)
     return GNATCOLL.JSON.JSON_Value
   is
      use SMM.Database_Remote;
      use GNATCOLL.JSON;
   begin
      return Result : constant JSON_Value := Create_Object do
         Result.Set_Field ("Operation", Operations'Image (Conflict));
         Result.Set_Field ("Local_Value", Local_JSON);
         Result.Set_Field ("Remote_Value", Remote_JSON);
      end return;
   end To_Conflict;

   function To_Insert (Item : in GNATCOLL.JSON.JSON_Value) return GNATCOLL.JSON.JSON_Value
   is
      use SMM.Database_Remote;
      use GNATCOLL.JSON;
   begin
      return Result : constant JSON_Value := Create_Object do
         Result.Set_Field ("Operation", Operations'Image (Insert));
         Result.Set_Field ("Value", Item);
      end return;
   end To_Insert;

   function To_Update (Item : in GNATCOLL.JSON.JSON_Value) return GNATCOLL.JSON.JSON_Value
   is
      use SMM.Database_Remote;
      use GNATCOLL.JSON;
   begin
      return Result : constant JSON_Value := Create_Object do
         Result.Set_Field ("Operation", Operations'Image (Update));
         Result.Set_Field ("Value", Item);
      end return;
   end To_Update;

   function To_Renumber
     (Old_ID : in Song_ID;
      New_ID : in Song_ID)
     return GNATCOLL.JSON.JSON_Value
   is
      use SMM.Database_Remote;
      use GNATCOLL.JSON;
   begin
      return Result : constant JSON_Value := Create_Object do
         Result.Set_Field ("Operation", Operations'Image (Renumber));
         Result.Set_Field ("Old_ID", Old_ID);
         Result.Set_Field ("New_ID", New_ID);
      end return;
   end To_Renumber;

   procedure Compute_Changes
     (Diff            : in     Diff_Type;
      Local_Modified  : in     ID_Lists.List;
      Remote_Modified : in     ID_Lists.List;
      Local_Changes   : in out GNATCOLL.JSON.JSON_Array;
      Conflicts       : in out GNATCOLL.JSON.JSON_Array;
      Remote_Changes  : in out GNATCOLL.JSON.JSON_Array;
      Progress        : in out SAL.Progress.Progress_Type)
   is
      use GNATCOLL.JSON;
      use ID_Lists;
      Local_J  : ID_Lists.Cursor := Local_Modified.First;
      Remote_J : ID_Lists.Cursor := Remote_Modified.First;

      Local_Modified_ID  : Song_ID;
      Remote_Modified_ID : Song_ID;
      Current_ID         : Song_ID;

      procedure Update_IDs
      is
         procedure Update_Current_ID (ID : in Song_ID)
         is begin
            if ID /= Invalid_Song_ID then
               Current_ID :=
                 (if Current_ID = Invalid_Song_ID
                  then ID
                  else Song_ID'Min (Current_ID, ID));
            end if;
         end Update_Current_ID;

      begin
         Local_Modified_ID  := (if Local_J = No_Element then Invalid_Song_ID else Element (Local_J));
         Remote_Modified_ID := (if Remote_J = No_Element then Invalid_Song_ID else Element (Remote_J));

         Current_ID := Invalid_Song_ID;

         Update_Current_ID (Local_Modified_ID);
         Update_Current_ID (Remote_Modified_ID);
      end Update_IDs;
   begin
      Update_IDs;
      Progress.Label ("Compute modified");
      loop
         exit when Current_ID = Invalid_Song_ID;

         Progress.Next;

         if Current_ID = Local_Modified_ID then
            if Current_ID = Remote_Modified_ID then
               --  Modified | Deleted in both.
               --
               --  May be recovering from previous modified/modified
               --  conflict. Don't include modified time in compare.
               declare
                  Local_JSON  : constant JSON_Value := Diff.Local_DB.Get_JSON (Current_ID);
                  Remote_JSON : constant JSON_Value := Diff.Remote_DB.Get_JSON (Current_ID);
               begin
                  if Local_JSON.Has_Field ("Deleted") then
                     if Remote_JSON.Has_Field ("Deleted") then
                        --  Deleted in both; no action
                        null;
                     else
                        --  Deleted in Local, Modified in Remote
                        Append (Conflicts, To_Conflict (Diff, Current_ID));
                     end if;
                  else
                     --  Modified in local
                     if Remote_JSON.Has_Field ("Deleted") then
                        Append (Conflicts, To_Conflict (Diff, Current_ID));
                     elsif JSON_Value'(Local_JSON.Get ("Data")) = JSON_Value'(Remote_JSON.Get ("Data")) then
                        --  all fields except Modified or Deleted are equal; no action
                        null;
                     else
                        Append (Conflicts, To_Conflict (Diff, Current_ID));
                     end if;
                  end if;
               end;
               Next (Local_J);
               Next (Remote_J);
            else
               --  Modified | Deleted only in local
               --
               --  May be recovering from previous modified/modified
               --  conflict. Don't include modified time in compare.
               declare
                  Local_JSON  : constant JSON_Value := Diff.Local_DB.Get_JSON (Current_ID);
                  Remote_JSON : constant JSON_Value := Diff.Remote_DB.Get_JSON (Current_ID);
               begin
                  if Local_JSON.Has_Field ("Deleted") then
                     Append (Remote_Changes, To_Update (Local_JSON));

                  elsif JSON_Value'(Local_JSON.Get ("Data")) = JSON_Value'(Remote_JSON.Get ("Data")) then
                     --  all fields except Modified | Deleted are equal; no action
                     null;
                  else
                     Append (Remote_Changes, To_Update (Local_JSON));
                  end if;
               end;
               Next (Local_J);

            end if;

         elsif Current_ID = Remote_Modified_ID then
            --  Modified | Deleted only in remote
            --
            --  May be recovering from previous modified/modified
            --  conflict. Don't include modified time in compare.
            declare
               Local_JSON  : constant JSON_Value := Diff.Local_DB.Get_JSON (Current_ID);
               Remote_JSON : constant JSON_Value := Diff.Remote_DB.Get_JSON (Current_ID);
            begin
               if Remote_JSON.Has_Field ("Deleted") then
                  Append (Local_Changes, To_Update (Remote_JSON));

               elsif JSON_Value'(Local_JSON.Get ("Data")) = JSON_Value'(Remote_JSON.Get ("Data")) then
                  --  all fields except Modified | Deleted are equal; no action
                  null;
               else
                  Append (Local_Changes, To_Update (Remote_JSON));
               end if;
            end;
            Next (Remote_J);

         else
            raise SAL.Programmer_Error;
         end if;

         Update_IDs;
      end loop;
   exception
   when E : others =>
      raise SAL.Programmer_Error with "diff.compute_changes: " &
        Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E);
   end Compute_Changes;

   procedure Compute_New
     (Diff           : in     Diff_Type;
      Local_New      : in     ID_Lists.List;
      Remote_New     : in     ID_Lists.List;
      Local_Changes  : in out GNATCOLL.JSON.JSON_Array;
      Conflicts      : in out GNATCOLL.JSON.JSON_Array;
      Remote_Changes : in out GNATCOLL.JSON.JSON_Array)
   is
      use GNATCOLL.JSON;
      use ID_Lists;
      Local_I  : ID_Lists.Cursor := Local_New.First;
      Remote_I : ID_Lists.Cursor := Remote_New.First;

      Local_New_ID  : Song_ID;
      Remote_New_ID : Song_ID;
      Current_ID    : Song_ID;

      Renumber_ID : Song_ID := Diff.Local_DB.Get_Last_ID;

      procedure Update_IDs
      is begin
         Local_New_ID  := (if Local_I = No_Element then Invalid_Song_ID else Element (Local_I));
         Remote_New_ID := (if Remote_I = No_Element then Invalid_Song_ID else Element (Remote_I));
         Current_ID    := Song_ID'Max (Local_New_ID, Remote_New_ID);
      end Update_IDs;
   begin
      for ID of Local_New loop
         if ID >= Renumber_ID then
            Renumber_ID := ID;
         end if;
      end loop;
      for ID of Remote_New loop
         if ID >= Renumber_ID then
            Renumber_ID := ID;
         end if;
      end loop;

      Update_IDs;
      loop
         exit when Current_ID = Invalid_Song_ID;

         if Current_ID = Local_New_ID then
            if Current_ID = Remote_New_ID then
               --  New in both. We could also check if this could be a simple update,
               --  but that's not likely in practice.
               declare
                  Local_JSON  : constant JSON_Value := Diff.Local_DB.Get_JSON (Current_ID);
                  Remote_JSON : constant JSON_Value := Diff.Remote_DB.Get_JSON (Current_ID);
               begin
                  if JSON_Value'(Local_JSON.Get ("Data")) = JSON_Value'(Remote_JSON.Get ("Data")) then
                     --  no conflict
                     null;

                  elsif Diff.Local_DB.Index_Fields_Equal (Current_ID, Remote_JSON) then
                     --  Treat as Modified/modified conflict
                     Append (Conflicts, To_Conflict (Local_JSON, Remote_JSON));

                  else
                     --  add/add conflict; renumber in local, insert
                     Renumber_ID := Renumber_ID + 1;
                     Append (Local_Changes, To_Renumber (Old_ID => Current_ID, New_ID => Renumber_ID));
                     Append (Local_Changes, To_Insert (Remote_JSON));
                     Local_JSON.Set_Field ("ID", Renumber_ID);
                     Append (Remote_Changes, To_Insert (Local_JSON));
                  end if;
               end;
               Next (Local_I);
               Next (Remote_I);
            else
               --  New in local
               Append (Remote_Changes, To_Insert (Diff.Local_DB.Get_JSON (Current_ID)));
               Next (Local_I);

            end if;

         elsif Current_ID = Remote_New_ID then
            --  New in remote
            Append (Local_Changes, To_Insert (Diff.Remote_DB.Get_JSON (Current_ID)));
            Next (Remote_I);

         else
            raise SAL.Programmer_Error;
         end if;

         Update_IDs;
      end loop;
   exception
   when E : others =>
      raise SAL.Programmer_Error with "diff.compute new: " &
        Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E);
   end Compute_New;

   ----------
   --  Public subprograms

   procedure Init_Remote
     (Diff           : in out Diff_Type;
      Max_Changes    : in     Ada.Containers.Count_Type;
      Remote_Changes :    out GNATCOLL.JSON.JSON_Array)
   is
      use GNATCOLL.JSON;
      use type Ada.Containers.Count_Type;

      Local_New     : ID_Lists.List;
      Changes_Count : Ada.Containers.Count_Type := 0;
      Remote_New    : ID_Lists.List;
      Conflicts     : GNATCOLL.JSON.JSON_Array;
      Local_Changes : GNATCOLL.JSON.JSON_Array  := Empty_Array;
   begin
      if Diff.Verbosity > 0 then
         Ada.Text_IO.Put_Line ("new" & Song_ID'Image (Diff.Sync_ID));
      end if;
      Local_New := Diff.Local_DB.Get_New
        (Diff.Sync_ID, Max_Changes - Changes_Count);

      Changes_Count := Changes_Count + Local_New.Length;

      if Local_New.Length > 0 then
         Diff.Sync_ID := Max_ID (Local_New);
      end if;

      Remote_Changes := Empty_Array;

      if Local_New.Length > 0 then
         Compute_New
           (Diff, Local_New, Remote_New,
            Local_Changes, Conflicts, Remote_Changes);
      end if;
   end Init_Remote;

   procedure Inc_Diff
     (Diff           : in     Diff_Type;
      Last_Sync_Time : in     Time_String;
      Local_Changes  :    out GNATCOLL.JSON.JSON_Array;
      Conflicts      :    out GNATCOLL.JSON.JSON_Array;
      Remote_Changes :    out GNATCOLL.JSON.JSON_Array)
   is
      use GNATCOLL.JSON;
      use type Ada.Containers.Count_Type;

      Local_Modified  : ID_Lists.List;
      Remote_Modified : ID_Lists.List;
      Local_New       : ID_Lists.List;
      Remote_New      : ID_Lists.List;
   begin
      Local_Modified  := Diff.Local_DB.Get_Modified (Diff.Sync_ID, Last_Sync_Time);
      Remote_Modified := Diff.Remote_DB.Get_Modified (Diff.Sync_ID, Last_Sync_Time);
      Local_New       := Diff.Local_DB.Get_New (Diff.Sync_ID);
      Remote_New      := Diff.Remote_DB.Get_New (Diff.Sync_ID);

      Local_Changes  := Empty_Array;
      Conflicts      := Empty_Array;
      Remote_Changes := Empty_Array;

      declare
         Progress : SAL.Progress.Progress_Type
           (Integer (Local_Modified.Length + Remote_Modified.Length + Local_New.Length + Remote_New.Length),
            Intervals => 100,
            Show      => Diff.Show_Progress);
      begin
         Compute_Changes
           (Diff,
            Local_Modified, Remote_Modified,
            Local_Changes, Conflicts, Remote_Changes, Progress);

         Compute_New
           (Diff, Local_New, Remote_New,
            Local_Changes, Conflicts, Remote_Changes);

         Progress.Complete;
      end;
   end Inc_Diff;

   procedure Apply
     (Diff           : in out Diff_Type;
      Local_Changes  : in     GNATCOLL.JSON.JSON_Array;
      Remote_Changes : in     GNATCOLL.JSON.JSON_Array)
   is
      use Ada.Exceptions;
      use GNATCOLL.JSON;

      Progress : SAL.Progress.Progress_Type
        (Length (Local_Changes) + Length (Remote_Changes),
         Intervals => 100,
         Show      => Diff.Show_Progress);
   begin
      Progress.Label ("Apply local changes");
      for I in 1 .. Length (Local_Changes) loop
         begin
            Progress.Next;
            if Diff.Verbosity > 1 then
               Ada.Text_IO.Put_Line (Get (Local_Changes, I).Write);
            end if;
            Diff.Local_DB.Apply (Get (Local_Changes, I));
         exception
         when E : others =>
            if Diff.Verbosity > 1 then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
            end if;
            raise SAL.Programmer_Error with
              "Local apply: " & Exception_Name (E) & ": " &
              Exception_Message (E)  & ": " &
              Get (Local_Changes, I).Write;
         end;
      end loop;
      Progress.Complete;

      Progress.Label ("Apply remote changes");
      for I in 1 .. Length (Remote_Changes) loop
         begin
            Progress.Next;
            if Diff.Verbosity > 1 then
               Ada.Text_IO.Put_Line (Get (Remote_Changes, I).Write);
            end if;
            Diff.Remote_DB.Apply (Get (Remote_Changes, I));
         exception
         when E : others =>
            if Diff.Verbosity > 1 then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
            end if;
            raise SAL.Programmer_Error with "Remote apply: " & Exception_Name (E) & ": " & Exception_Message (E);
         end;
      end loop;
      Progress.Complete;

   end Apply;

   procedure Update_Sync_ID (Diff : in out Diff_Type)
   is begin
      Diff.Sync_ID := Diff.Local_DB.Get_Last_ID;
   end Update_Sync_ID;

end SMM.Database.Diff;
