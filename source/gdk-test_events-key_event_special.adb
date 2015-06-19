--  Abstract :
--
--  Win32 version.
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

with SAL.Win32.Keyboard; use SAL.Win32.Keyboard;
separate (Gdk.Test_Events)
procedure Key_Event_Special (Key : in Key_Type; Key_Up : in Boolean) is
begin
   case Key is
   when Up =>
      Key_Event (VK_UP, Key_Up);

   when Down =>
      Key_Event (VK_DOWN, Key_Up);

   when Left =>
      Key_Event (VK_LEFT, Key_Up);

   when Right =>
      Key_Event (VK_RIGHT, Key_Up);

   when Enter =>
      Key_Event (VK_RETURN, Key_Up);

   when Ctrl =>
      Key_Event (VK_CONTROL, Key_Up);

   when Alt =>
      Key_Event (VK_MENU, Key_Up);

   when Shift =>
      Key_Event (VK_SHIFT, Key_Up);

   when Tab =>
      Key_Event (VK_TAB, Key_Up);

   end case;

   delay Small_Delay;

end Key_Event_Special;
