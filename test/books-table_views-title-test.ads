--  Abstract :
--
--  Access to contents of Table_View.Title windows for testing.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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
package Books.Table_Views.Title.Test is

   Title_Contents : Test_Books.String_Lists.String_List_Type (1 .. 2);
   Title_Contents_Full : Test_Books.String_Lists.String_List_Type (1 .. 4);

   --  use Books.Table_Views.Test.Set_Test_Hook

   procedure Dump_Title (Table_View : in Gtk_Table_View);
   --  Dump the currently displayed title, year to Title_Contents.

   procedure Dump_Title_Full (Table_View : in Gtk_Table_View);
   --  Dump the currently displayed title, year, comment, rating to Title_Contents_Full.

end Books.Table_Views.Title.Test;
