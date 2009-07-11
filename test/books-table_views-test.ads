--  Abstract :
--
--  Access to contents of Table_View windows for testing.
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Test_Books.String_Lists;
package Books.Table_Views.Test is

   Clist_Contents : Test_Books.String_Lists.String_Table_Type;
   --  Elements of Clist_Contents are rows; elements of rows are columns

   procedure Set_Test_Hook (Hook : in Test_Hook_Type);

   procedure Dump_Clist (Table_View : in Gtk_Table_View);
   --  Dump the currently displayed Clist to Clist_Contents.

   function Add_Link_Button (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type;
   function Find_Entry (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type;
   function First_Link (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type;
   function Second_Link (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type;
   --  Return absolute coordinates of left top of GUI item.

end Books.Table_Views.Test;
