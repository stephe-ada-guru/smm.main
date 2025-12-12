--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2016, 2018 - 2020, 2025  All Rights Reserved.
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

with Ada.Text_IO;
with GNATCOLL.JSON;
with SAL;
package body SMM.Database_Remote.IP is

   --  Local bodies, alphabetical

   function Get_Object (DB : in Database) return String
   is
   begin
      return Msg : constant String := String (Network_String'Input (DB.Stream)) do
         if DB.Verbosity > 1  then
            Ada.Text_IO.Put_Line ("Remote: " & Msg);
         end if;

      end return;
   end Get_Object;

   procedure Check_Ack (DB : in out Database)
   is
      use GNATCOLL.JSON;
      Msg : constant String := Get_Object (DB);
   begin
      declare
         Response : constant JSON_Value := Read (Msg);
      begin
         if Response.Get ("Status") /= Ack_Nack'Image (Ack) then
            raise SAL.Invalid_Operation with Response.Get ("Message");
         end if;
      end;
   end Check_Ack;

   function Check_Ack (DB : in Database) return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;
      Msg : constant String := Get_Object (DB);
   begin
      declare
         Response : constant JSON_Value := Read (Msg);
      begin
         if Response.Get ("Status") /= Ack_Nack'Image (Ack) then
            raise SAL.Invalid_Operation with Response.Get ("Message");
         end if;
         return Response.Get ("Data");
      end;
   end Check_Ack;

   procedure Send (DB : in Database; Msg : in String)
   is begin
      if DB.Verbosity > 1  then
         Ada.Text_IO.Put_Line ("Local: " & Msg);
      end if;

      Network_String'Output (DB.Stream, Network_String (Msg));
   end Send;

   ----------
   --  Public bodies, alphabetical

   overriding procedure Apply
     (DB  : in out Database;
      Msg : in     GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;
   begin
      Send (DB, Msg.Write);
      Check_Ack (DB);
   end Apply;

   overriding function Get_JSON
     (DB : in Database;
      ID : in Song_ID)
     return GNATCOLL.JSON.JSON_Value
   is
      use GNATCOLL.JSON;
      Msg : constant JSON_Value := Create_Object;
   begin
      Set_Field (Msg, "Operation", Operations'Image (Get));
      Set_Field (Msg, "ID", ID);
      Send (DB, Msg.Write);
      return Check_Ack (DB);
   end Get_JSON;

   overriding function Get_Last_ID (DB : in out Database) return Song_ID
   is
      use GNATCOLL.JSON;
      Msg : constant JSON_Value := Create_Object;
   begin
      Set_Field (Msg, "Operation", Operations'Image (Get_Last_ID));
      Send (DB, Msg.Write);
      return Get (Check_Ack (DB), "ID");
   end Get_Last_ID;

   overriding function Get_Modified
     (DB       : in out Database;
      ID       : in     Song_ID;
      Modified : in     Time_String)
     return ID_Lists.List
   is
      use GNATCOLL.JSON;
      Msg : constant JSON_Value := Create_Object;
   begin
      Msg.Set_Field ("Operation", Operations'Image (Get_Modified));
      Msg.Set_Field ("ID", ID);
      Msg.Set_Field ("Modified", Modified);
      Send (DB, Msg.Write);
      return To_List (Check_Ack (DB));
   end Get_Modified;

   overriding function Get_New
     (DB        : in out Database;
      ID        : in     Song_ID;
      Max_Count : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last)
     return ID_Lists.List
   is
      use GNATCOLL.JSON;
      Msg : constant JSON_Value := Create_Object;
   begin
      Msg.Set_Field ("Operation", Operations'Image (Get_New));
      Msg.Set_Field ("ID", ID);
      Msg.Set_Field ("Max_Count", Integer (Max_Count));
      Send (DB, Msg.Write);
      return To_List (Check_Ack (DB));
   end Get_New;

   overriding function Index_Fields_Equal
     (DB        : in out Database;
      ID        : in     Song_ID;
      New_Value : in     GNATCOLL.JSON.JSON_Value)
     return Boolean
   is begin
      --  Never sent to the remote.
      raise SAL.Programmer_Error;
      return False;
   end Index_Fields_Equal;

   procedure Init_Compute
     (DB               : in out Database;
      Action           : in     Actions;
      Display_Progress : in     Boolean;
      Sync_Time        : in     Time_String := Default_Time_String)
   is
      use GNATCOLL.JSON;

      Msg : constant JSON_Value := Create_Object;
   begin
      Msg.Set_Field (Prelude_Messages'Image (Role), Roles'Image (Compute));
      Msg.Set_Field (Prelude_Messages'Image (Database_Remote.Action), Actions'Image (Action));
      if Sync_Time /= Default_Time_String then
         Msg.Set_Field (Prelude_Messages'Image (Database_Remote.Sync_Time), Sync_Time);
      end if;
      Msg.Set_Field (Prelude_Messages'Image (Database_Remote.Display_Progress), Boolean'Image (Display_Progress));
      Send (DB, Msg.Write);

      Check_Ack (DB);
   end Init_Compute;

   procedure Init_Remote (DB : in out Database)
   is
      use GNATCOLL.JSON;
      Msg : constant JSON_Value := Create_Object;
   begin
      Msg.Set_Field (Prelude_Messages'Image (Role), Roles'Image (Remote));
      Send (DB, Msg.Write);

      Check_Ack (DB);
   end Init_Remote;

   procedure Send_Quit (DB : in out Database)
   is
      use GNATCOLL.JSON;
      Msg : constant JSON_Value := Create_Object;
   begin
      Set_Field (Msg, "Operation", Operations'Image (Quit));
      Network_String'Output (DB.Stream, Network_String (String'(Msg.Write)));
      Check_Ack (DB);
   end Send_Quit;

   procedure Send_Error (Stream : in GNAT.Sockets.Stream_Access; Msg : in String)
   is
      Response : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      Response.Set_Field ("Status", Ack_Nack'Image (Nack));
      Response.Set_Field ("Message", Msg);

      Network_String'Output (Stream, Network_String (String'(Response.Write)));
   end Send_Error;

   procedure Send_Data (Stream : in GNAT.Sockets.Stream_Access; Data : in GNATCOLL.JSON.JSON_Value)
   is
      Response : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      Response.Set_Field ("Status", Ack_Nack'Image (Ack));
      Response.Set_Field ("Data", Data);

      Network_String'Output (Stream, Network_String (String'(Response.Write)));
   end Send_Data;

   procedure Send_Ack (Stream : in GNAT.Sockets.Stream_Access)
   is
      Response : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      Response.Set_Field ("Status", Ack_Nack'Image (Ack));

      Network_String'Output (Stream, Network_String (String'(Response.Write)));
   end Send_Ack;

   procedure Send_Progress
     (DB           : in out Database;
      Label        : in              String;
      Current, Max : in              Integer)
   is
      use GNATCOLL.JSON;

      Msg  : constant JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      Msg.Set_Field ("Operation", Operations'Image (Progress));
      Msg.Set_Field ("Label", Label);
      Msg.Set_Field ("Current", Current);
      Msg.Set_Field ("Max", Max);

      Send (DB, Msg.Write);
      Check_Ack (DB);
   end Send_Progress;

   procedure Send_Messages
     (DB        : in out Database;
      Messages  : in              GNATCOLL.JSON.JSON_Array;
      Verbosity : in              Integer)
   is
      use GNATCOLL.JSON;
   begin
      for I in 1 .. Length (Messages) loop
         if Verbosity > 1  then
            Ada.Text_IO.Put_Line (Get (Messages, I).Write);
         end if;

         Send (DB, Get (Messages, I).Write);
         Check_Ack (DB);
      end loop;
   end Send_Messages;

end SMM.Database_Remote.IP;
