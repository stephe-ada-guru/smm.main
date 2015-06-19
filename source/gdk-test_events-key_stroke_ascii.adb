--  Abstract :
--
--  See spec. Win32 version.
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

with Ada.Characters.Handling;
with SAL.Win32.Keyboard; use SAL.Win32.Keyboard;
separate (Gdk.Test_Events)
procedure Key_Stroke_ASCII (Key : in Character)
is
   procedure Set_Shifted
   is begin
      Key_Event (VK_SHIFT, Key_Up => False);
      delay Small_Delay;
   end Set_Shifted;

   procedure Set_Not_Shifted
   is begin
      Key_Event (VK_SHIFT, Key_Up => True);
      delay Small_Delay;
   end Set_Not_Shifted;

   procedure Key_Stroke (Key : in Virtual_Key_Code_Type)
   is begin
      begin
         Key_Event (Key, Key_Up => False);
         delay Small_Delay;
      exception
      when others =>
         --  Action taken on key event had a problem; ensure we finish
         --  the key stroke, but still propagate exception so test
         --  reports it.
         Key_Event (Key, Key_Up => True);
         raise;
      end;

      Key_Event (Key, Key_Up => True);
      delay Small_Delay;
   end Key_Stroke;

begin

   case Key is
   when ' ' =>
      Key_Stroke (VK_SPACE);

   when '!' =>
      Set_Shifted;
      Key_Stroke (VK_1);
      Set_Not_Shifted;

   when '"' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_7);
      Set_Not_Shifted;

   when '#' =>
      Set_Shifted;
      Key_Stroke (VK_3);
      Set_Not_Shifted;

   when '$' =>
      Set_Shifted;
      Key_Stroke (VK_4);
      Set_Not_Shifted;

   when '%' =>
      Set_Shifted;
      Key_Stroke (VK_5);
      Set_Not_Shifted;

   when '&' =>
      Set_Shifted;
      Key_Stroke (VK_7);
      Set_Not_Shifted;

   when ''' =>
      Set_Not_Shifted;
      Key_Stroke (VK_OEM_7);
      Set_Not_Shifted;

   when '(' =>
      Set_Shifted;
      Key_Stroke (VK_9);
      Set_Not_Shifted;

   when ')' =>
      Set_Shifted;
      Key_Stroke (VK_0);
      Set_Not_Shifted;

   when '*' =>
      Set_Shifted;
      Key_Stroke (VK_8);
      Set_Not_Shifted;

   when '+' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_PLUS);
      Set_Not_Shifted;

   when ',' =>
      Key_Stroke (VK_OEM_COMMA);

   when '-' =>
      Key_Stroke (VK_OEM_MINUS);

   when '.' =>
      Key_Stroke (VK_OEM_PERIOD);

   when '/' =>
      Key_Stroke (VK_OEM_2);

   when '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  =>
      Key_Stroke (To_Virtual (Key));

   when ':' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_1);
      Set_Not_Shifted;

   when ';' =>
      Key_Stroke (VK_OEM_1);

   when '<' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_COMMA);
      Set_Not_Shifted;

   when '=' =>
      Key_Stroke (VK_OEM_PLUS);

   when '>' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_PERIOD);
      Set_Not_Shifted;

   when '?' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_2);
      Set_Not_Shifted;

   when '@' =>
      Set_Shifted;
      Key_Stroke (VK_2);
      Set_Not_Shifted;

   when  'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' |
     'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' |
     'W' | 'X' | 'Y' | 'Z' =>
      Set_Shifted;
      Key_Stroke (To_Virtual (Key));
      Set_Not_Shifted;

   when '[' =>
      Key_Stroke (VK_OEM_4);

   when '\' =>
      Key_Stroke (VK_OEM_5);

   when ']' =>
      Key_Stroke (VK_OEM_6);

   when '^' =>
      Set_Shifted;
      Key_Stroke (VK_6);
      Set_Not_Shifted;

   when '_' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_MINUS);
      Set_Not_Shifted;

   when '`' =>
      Key_Stroke (VK_OEM_3);

   when  'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' |
     'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' |
     'w' | 'x' | 'y' | 'z' =>
      Key_Stroke (To_Virtual (Ada.Characters.Handling.To_Upper (Key)));

   when '{' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_4);
      Set_Not_Shifted;

   when '|' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_5);
      Set_Not_Shifted;

   when '}' =>
      Set_Shifted;
      Key_Stroke (VK_OEM_6);
      Set_Not_Shifted;

   when '~' =>
      Key_Stroke (VK_OEM_3);

   when others =>
      null;
   end case;
end Key_Stroke_ASCII;
