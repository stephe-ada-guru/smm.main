--  Abstract :
--
--  Utilities for GUI tests for Books
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Strings.Unbounded;
with Gdk.Test_Events;
package Test_Books.GUI_Utils is

   --  Absolute window origins, set by Set_Window_Origins
   Main_Origin   : Gdk.Test_Events.Point_Type;
   Author_Origin : Gdk.Test_Events.Point_Type;
   Title_Origin  : Gdk.Test_Events.Point_Type;

   --  Common mouse locations, relative to *_Origin. Use mnemonics when possible.
   Find_Entry            : constant Gdk.Test_Events.Point_Type := (72, 63);
   Title_Add_Link_Button : constant Gdk.Test_Events.Point_Type := (45, 220);

   procedure Set_Window_Origins (Config_File : in Ada.Strings.Unbounded.Unbounded_String);
   --  Set window origins from Config_File.

   procedure Empty_Database;
   --  Run 'make empty_database_test'.

   procedure Add_Author
     (First  : in String;
      Middle : in String;
      Last   : in String);
   --  Use Mouse & Keyboard events to add author. Assumes Author
   --  window is showing main display. Last is typed into the Find
   --  window, as the user normally will do.

   procedure Add_Title
     (Title   : in String;
      Year    : in String;
      Comment : in String;
      Rating  : in String);
   --  Use Mouse & Keyboard events to add title. Assumes Title
   --  window is showing main display. Title is typed into the Find
   --  window, as the user normally will do.

end Test_Books.GUI_Utils;
