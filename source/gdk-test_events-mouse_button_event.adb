--  Abstract :
--
--  See spec. Win32 body.
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

with SAL.Win32.Mouse; use SAL.Win32.Mouse;
separate (Gdk.Test_Events)
procedure Mouse_Button_Event
  (Button    : in Glib.Gint;
   Button_Up : in Boolean)
is
begin
   case Button is
   when 1 =>
      if Button_Up then
         Mouse_Event (Left_Up => True);
      else
         Mouse_Event (Left_Down => True);
      end if;

   when 2 =>
      if Button_Up then
         Mouse_Event (Middle_Up => True);
      else
         Mouse_Event (Middle_Down => True);
      end if;

   when 3 =>
      if Button_Up then
         Mouse_Event (Right_Up => True);
      else
         Mouse_Event (Right_Down => True);
      end if;

   when others =>
      null;
   end case;
end Mouse_Button_Event;
