--  Abstract :
--
--  Process one Operation message (assumed from Stream); perform it on
--  Local_DB, send results on Stream.
--
--  Copyright (C) 2016, 2019, 2025, 2026 Stephen Leake All Rights Reserved.
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

with SAL.Progress;
with SMM.Database_Remote.IP; use SMM.Database_Remote.IP;
with GNAT.Sockets;
with GNATCOLL.JSON;
procedure SMM.Database_Remote.Do_Operation
  (Msg           : in     GNATCOLL.JSON.JSON_Value;
   Stream        : in     GNAT.Sockets.Stream_Access;
   Local_DB      : in     SMM.Database_Remote.Database_Access;
   Conflicts     : in out GNATCOLL.JSON.JSON_Array;
   Show_Progress : in     SAL.Progress.Show_Progress_Type)
is
   use GNATCOLL.JSON;
   use type SAL.Progress.Show_Progress_Type;
begin
   if not Msg.Has_Field ("Operation") then
      Send_Error (Stream, "no operation");
      raise Bad_Operation with "no Operation field: " & Msg.Write;
   end if;

   case Operations'Value (Msg.Get ("Operation")) is
   when Quit =>
      Send_Ack (Stream);
      raise Quit_Operation;

   when Get =>
      Send_Data (Stream, Local_DB.Get_JSON (Msg.Get ("ID")));

   when Get_Last_ID =>
      declare
         Result : constant JSON_Value := Create_Object;
      begin
         Set_Field (Result, "ID", Local_DB.Get_Last_ID);
         Send_Data (Stream, Result);
      end;

   when Get_Modified =>
      declare
         Result : constant JSON_Value := Create_Object;
      begin
         Set_Field
           (Result, "List",
            To_JSON
              (Local_DB.Get_Modified
                 (Msg.Get ("ID"),
                  Msg.Get ("Modified"))));
         Send_Data (Stream, Result);
      end;

   when Get_New =>
      declare
         Result : constant JSON_Value := Create_Object;
      begin
         Set_Field
           (Result, "List",
            To_JSON
              (Local_DB.Get_New
                 (Msg.Get ("ID"),
                  Ada.Containers.Count_Type (Integer'(Msg.Get ("Max_Count"))))));
         Send_Data (Stream, Result);
      end;

   when Conflict =>
      Append (Conflicts, Msg);
      Send_Ack (Stream);

   when Progress =>
      if Show_Progress /= null then
         Show_Progress
           (Label   => Msg.Get ("Label"),
            Current => Msg.Get ("Current"),
            Max     => Msg.Get ("Max"));
      end if;

      Send_Ack (Stream);

   when Apply_Operations =>
      Local_DB.Apply (Msg);
      Send_Ack (Stream);
   end case;
end SMM.Database_Remote.Do_Operation;
