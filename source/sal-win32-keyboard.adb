--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with SAL.Interfaces_More;
with System;
package body SAL.Win32.Keyboard is

   type Win32_Key_Event_Flag_Type is record
      Extended_Key : Boolean;
      Key_Up       : Boolean;
      Reserved     : Interfaces_More.Unsigned_30 := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, Win32_Key_Event_Flag_Type);
   for Win32_Key_Event_Flag_Type use record
      Extended_Key at 0 range 0 ..  0;
      Key_Up       at 0 range 1 ..  1;
      Reserved     at 0 range 2 .. 31;
   end record;
   for Win32_Key_Event_Flag_Type'Bit_Order use System.Low_Order_First;
   for Win32_Key_Event_Flag_Type'Size use 32;

   procedure Win32_Keybd_Event
      (BVk         : in Virtual_Key_Code_Type;
       bScan       : in Interfaces.C.unsigned_char;
       DwFlags     : in Win32_Key_Event_Flag_Type;
       dwExtraInfo : in Interfaces.C.unsigned_long);
   pragma Import (StdCall, Win32_Keybd_Event, "keybd_event");

   procedure Key_Event
     (Virtual_Key_Code : in Virtual_Key_Code_Type;
      Key_Up           : in Boolean)
   is
      Flags : Win32_Key_Event_Flag_Type;
   begin
      Flags.Key_Up := Key_Up;
      Win32_Keybd_Event (Virtual_Key_Code, 20, Flags, 0);
   end Key_Event;

end SAL.Win32.Keyboard;
