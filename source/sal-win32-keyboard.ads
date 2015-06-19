--  Abstract :
--
--  Facilities for generating keyboard events, on Windows using the
--  Win32 API.
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

with Ada.Unchecked_Conversion;
with Interfaces.C;
package SAL.Win32.Keyboard is
   pragma Preelaborate; -- SAL.Interfaces_More in body

   type Virtual_Key_Code_Type is new Interfaces.C.unsigned_char;

   --  Virtual key codes from "Win32 Programming", Brent E. Rector,
   --  Joseph M. Newcomer, table 7.3 Codes not in Win32 programming
   --  are from winuser.h

   VK_LBUTTON    : constant Virtual_Key_Code_Type := 16#01#;
   VK_RBUTTON    : constant Virtual_Key_Code_Type := 16#02#;
   VK_CANCEL     : constant Virtual_Key_Code_Type := 16#03#;
   VK_MBUTTON    : constant Virtual_Key_Code_Type := 16#04#;
   --  05 .. 07 undefined
   VK_BACK       : constant Virtual_Key_Code_Type := 16#08#;
   VK_TAB        : constant Virtual_Key_Code_Type := 16#09#;
   --  0A .. 0B undefined
   VK_CLEAR      : constant Virtual_Key_Code_Type := 16#0C#;
   VK_RETURN     : constant Virtual_Key_Code_Type := 16#0D#;
   --  0E .. 0F undefined
   VK_SHIFT      : constant Virtual_Key_Code_Type := 16#10#;
   VK_CONTROL    : constant Virtual_Key_Code_Type := 16#11#;
   VK_MENU       : constant Virtual_Key_Code_Type := 16#12#;
   VK_PAUSE      : constant Virtual_Key_Code_Type := 16#13#;
   VK_CAPITAL    : constant Virtual_Key_Code_Type := 16#14#;
   --  15 .. 19 Kanji
   --  1A undefined
   VK_ESCAPE     : constant Virtual_Key_Code_Type := 16#1B#;
   --  1C .. 1F undefined
   VK_SPACE      : constant Virtual_Key_Code_Type := 16#20#;
   VK_PRIOR      : constant Virtual_Key_Code_Type := 16#21#;
   VK_NEXT       : constant Virtual_Key_Code_Type := 16#22#;
   VK_END        : constant Virtual_Key_Code_Type := 16#23#;
   VK_HOME       : constant Virtual_Key_Code_Type := 16#24#;
   VK_LEFT       : constant Virtual_Key_Code_Type := 16#25#;
   VK_UP         : constant Virtual_Key_Code_Type := 16#26#;
   VK_RIGHT      : constant Virtual_Key_Code_Type := 16#27#;
   VK_DOWN       : constant Virtual_Key_Code_Type := 16#28#;
   VK_SELECT     : constant Virtual_Key_Code_Type := 16#29#;
   VK_PRINT      : constant Virtual_Key_Code_Type := 16#2A#;
   VK_EXECUTE    : constant Virtual_Key_Code_Type := 16#2B#;
   VK_SNAPSHOT   : constant Virtual_Key_Code_Type := 16#2C#;
   VK_INSERT     : constant Virtual_Key_Code_Type := 16#2D#;
   VK_DELETE     : constant Virtual_Key_Code_Type := 16#2E#;
   VK_HELP       : constant Virtual_Key_Code_Type := 16#2F#;
   VK_0          : constant Virtual_Key_Code_Type := 16#30#;
   VK_1          : constant Virtual_Key_Code_Type := 16#31#;
   VK_2          : constant Virtual_Key_Code_Type := 16#32#;
   VK_3          : constant Virtual_Key_Code_Type := 16#33#;
   VK_4          : constant Virtual_Key_Code_Type := 16#34#;
   VK_5          : constant Virtual_Key_Code_Type := 16#35#;
   VK_6          : constant Virtual_Key_Code_Type := 16#36#;
   VK_7          : constant Virtual_Key_Code_Type := 16#37#;
   VK_8          : constant Virtual_Key_Code_Type := 16#38#;
   VK_9          : constant Virtual_Key_Code_Type := 16#39#;
   --  3A .. 40 undefined
   VK_A          : constant Virtual_Key_Code_Type := 16#41#;
   VK_B          : constant Virtual_Key_Code_Type := 16#42#;
   VK_C          : constant Virtual_Key_Code_Type := 16#43#;
   VK_D          : constant Virtual_Key_Code_Type := 16#44#;
   VK_E          : constant Virtual_Key_Code_Type := 16#45#;
   VK_F          : constant Virtual_Key_Code_Type := 16#46#;
   VK_G          : constant Virtual_Key_Code_Type := 16#47#;
   VK_H          : constant Virtual_Key_Code_Type := 16#48#;
   VK_I          : constant Virtual_Key_Code_Type := 16#49#;
   VK_J          : constant Virtual_Key_Code_Type := 16#4A#;
   VK_K          : constant Virtual_Key_Code_Type := 16#4B#;
   VK_L          : constant Virtual_Key_Code_Type := 16#4C#;
   VK_M          : constant Virtual_Key_Code_Type := 16#4D#;
   VK_N          : constant Virtual_Key_Code_Type := 16#4E#;
   VK_O          : constant Virtual_Key_Code_Type := 16#4F#;
   VK_P          : constant Virtual_Key_Code_Type := 16#50#;
   VK_Q          : constant Virtual_Key_Code_Type := 16#51#;
   VK_R          : constant Virtual_Key_Code_Type := 16#52#;
   VK_S          : constant Virtual_Key_Code_Type := 16#53#;
   VK_T          : constant Virtual_Key_Code_Type := 16#54#;
   VK_U          : constant Virtual_Key_Code_Type := 16#55#;
   VK_V          : constant Virtual_Key_Code_Type := 16#56#;
   VK_W          : constant Virtual_Key_Code_Type := 16#57#;
   VK_X          : constant Virtual_Key_Code_Type := 16#58#;
   VK_Y          : constant Virtual_Key_Code_Type := 16#59#;
   VK_Z          : constant Virtual_Key_Code_Type := 16#5A#;
   VK_LWIN       : constant Virtual_Key_Code_Type := 16#5B#; -- not in "win32 Programming"
   VK_RWIN       : constant Virtual_Key_Code_Type := 16#5C#; -- ""
   VK_APPS       : constant Virtual_Key_Code_Type := 16#5D#; -- ""
   --  5F undefined
   VK_NUMPAD0    : constant Virtual_Key_Code_Type := 16#60#;
   VK_NUMPAD1    : constant Virtual_Key_Code_Type := 16#61#;
   VK_NUMPAD2    : constant Virtual_Key_Code_Type := 16#62#;
   VK_NUMPAD3    : constant Virtual_Key_Code_Type := 16#63#;
   VK_NUMPAD4    : constant Virtual_Key_Code_Type := 16#64#;
   VK_NUMPAD5    : constant Virtual_Key_Code_Type := 16#65#;
   VK_NUMPAD6    : constant Virtual_Key_Code_Type := 16#66#;
   VK_NUMPAD7    : constant Virtual_Key_Code_Type := 16#67#;
   VK_NUMPAD8    : constant Virtual_Key_Code_Type := 16#68#;
   VK_NUMPAD9    : constant Virtual_Key_Code_Type := 16#69#;
   VK_MULTIPLY   : constant Virtual_Key_Code_Type := 16#6A#;
   VK_ADD        : constant Virtual_Key_Code_Type := 16#6B#;
   VK_SEPARATOR  : constant Virtual_Key_Code_Type := 16#6C#;
   VK_SUBTRACT   : constant Virtual_Key_Code_Type := 16#6D#;
   VK_DECIMAL    : constant Virtual_Key_Code_Type := 16#6E#;
   VK_DIVIDE     : constant Virtual_Key_Code_Type := 16#6F#;
   VK_F1         : constant Virtual_Key_Code_Type := 16#70#;
   VK_F2         : constant Virtual_Key_Code_Type := 16#71#;
   VK_F3         : constant Virtual_Key_Code_Type := 16#72#;
   VK_F4         : constant Virtual_Key_Code_Type := 16#73#;
   VK_F5         : constant Virtual_Key_Code_Type := 16#74#;
   VK_F6         : constant Virtual_Key_Code_Type := 16#75#;
   VK_F7         : constant Virtual_Key_Code_Type := 16#76#;
   VK_F8         : constant Virtual_Key_Code_Type := 16#77#;
   VK_F9         : constant Virtual_Key_Code_Type := 16#78#;
   VK_F10        : constant Virtual_Key_Code_Type := 16#79#;
   VK_F11        : constant Virtual_Key_Code_Type := 16#7A#;
   VK_F12        : constant Virtual_Key_Code_Type := 16#7B#;
   VK_F13        : constant Virtual_Key_Code_Type := 16#7C#;
   VK_F14        : constant Virtual_Key_Code_Type := 16#7D#;
   VK_F15        : constant Virtual_Key_Code_Type := 16#7E#;
   VK_F16        : constant Virtual_Key_Code_Type := 16#7F#;
   --  80 .. 87 OEM specific
   --  88 .. 8F undefined
   VK_NUMLOCK    : constant Virtual_Key_Code_Type := 16#90#;
   VK_SCROLL     : constant Virtual_Key_Code_Type := 16#91#;
   --  92 .. 9F undefined
   VK_LSHIFT     : constant Virtual_Key_Code_Type := 16#A0#; -- not in "Win32 Programming"
   VK_RSHIFT     : constant Virtual_Key_Code_Type := 16#A1#; -- ""
   VK_LCONTROL   : constant Virtual_Key_Code_Type := 16#A2#; -- ""
   VK_RCONTROL   : constant Virtual_Key_Code_Type := 16#A3#; -- ""
   VK_LMENU      : constant Virtual_Key_Code_Type := 16#A4#; -- ""
   VK_RMENU      : constant Virtual_Key_Code_Type := 16#A5#; -- ""
   --  A6 .. B9 undefined
   VK_OEM_1      : constant Virtual_Key_Code_Type := 16#BA#; -- ; :
   VK_OEM_PLUS   : constant Virtual_Key_Code_Type := 16#BB#; -- = +
   VK_OEM_COMMA  : constant Virtual_Key_Code_Type := 16#BC#; -- , <
   VK_OEM_MINUS  : constant Virtual_Key_Code_Type := 16#BD#; -- - _
   VK_OEM_PERIOD : constant Virtual_Key_Code_Type := 16#BE#; -- . >
   VK_OEM_2      : constant Virtual_Key_Code_Type := 16#BF#; -- / ?
   VK_OEM_3      : constant Virtual_Key_Code_Type := 16#C0#; -- ` ~
   --  C1 .. DA undefined
   VK_OEM_4      : constant Virtual_Key_Code_Type := 16#DB#; -- [ {
   VK_OEM_5      : constant Virtual_Key_Code_Type := 16#DC#; -- \ |
   VK_OEM_6      : constant Virtual_Key_Code_Type := 16#DD#; -- ] {
   VK_OEM_7      : constant Virtual_Key_Code_Type := 16#DE#; -- ' "
   --  DF .. E3 undefined
   VK_OEM_8      : constant Virtual_Key_Code_Type := 16#E4#;
   VK_PROCESSKEY : constant Virtual_Key_Code_Type := 16#E5#;
   VK_ATTN       : constant Virtual_Key_Code_Type := 16#F6#;
   VK_CRSEL      : constant Virtual_Key_Code_Type := 16#F7#;
   VK_EXSEL      : constant Virtual_Key_Code_Type := 16#F8#;
   VK_EREOF      : constant Virtual_Key_Code_Type := 16#F9#;
   VK_PLAY       : constant Virtual_Key_Code_Type := 16#FA#;
   VK_ZOOM       : constant Virtual_Key_Code_Type := 16#FB#;
   VK_NONAME     : constant Virtual_Key_Code_Type := 16#FC#;
   VK_PA1        : constant Virtual_Key_Code_Type := 16#FD#;
   VK_OEM_CLEAR  : constant Virtual_Key_Code_Type := 16#FE#;

   function To_Virtual is new Ada.Unchecked_Conversion
      (Source => Character,
       Target => Virtual_Key_Code_Type);

   procedure Key_Event
     (Virtual_Key_Code : in Virtual_Key_Code_Type;
      Key_Up           : in Boolean);
   --  Generate a key-up or key-down event. The scan code is not
   --  correct.

end SAL.Win32.Keyboard;
