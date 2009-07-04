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

package body Books.Table_Views.Author.Test is

   procedure Dump_Author (Table_View : in Gtk_Table_View)
   is
      use Test_Books.String_Lists;
      Author_View : Gtk_Author_View_Record renames Gtk_Author_View_Record (Table_View.all);
   begin
      Author_Contents (1) := +(Gtk.GEntry.Get_Text (Author_View.First_Text));
      Author_Contents (2) := +(Gtk.GEntry.Get_Text (Author_View.Middle_Text));
      Author_Contents (3) := +(Gtk.GEntry.Get_Text (Author_View.Last_Text));
   end Dump_Author;

end Books.Table_Views.Author.Test;
