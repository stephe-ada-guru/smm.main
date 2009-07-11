--  Abstract :
--
--  See spec
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

package body Books.Table_Views.Title.Test is

   procedure Dump_Title (Table_View : in Gtk_Table_View)
   is
      use Test_Books.String_Lists;
      Title_View : Gtk_Title_View_Record renames Gtk_Title_View_Record (Table_View.all);
   begin
      Title_Contents (1) := +(Gtk.GEntry.Get_Text (Title_View.Title_Text));
      Title_Contents (2) := +(Gtk.GEntry.Get_Text (Title_View.Year_Text));
   end Dump_Title;

end Books.Table_Views.Title.Test;
