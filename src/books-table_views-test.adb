--  Abstract :
--
--  See spec
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

with Glib;
package body Books.Table_Views.Test is

   procedure Set_Test_Hook (Hook : in Test_Hook_Type)
   is begin
      Books.Table_Views.Test_Hook := Hook;
   end Set_Test_Hook;

   procedure Dump_Clist (Table_View : in Gtk_Table_View)
   is
      use Gtk.Clist, Test_Books.String_Lists;
      Clist             : constant Gtk_Clist := Table_View.List_Display (Table_View.Current_List);
      Column_Last       : constant Glib.Gint := Get_Columns (Clist);
      Row_Last          : constant Glib.Gint := Get_Rows (Clist);
      use type Glib.Gint;
   begin
      String_Tables.Finalize (Clist_Contents);
      for Row in 0 .. Row_Last - 1 loop
         declare
            Row_Strings  : String_List_Type (0 .. Column_Last - 1);
         begin
            for Column in 0 .. Column_Last - 1 loop
               Row_Strings (Column) := +Get_Text (Clist, Row, Column);
            end loop;
            String_Tables.Add (Clist_Contents, Row_Strings);
         end;
      end loop;
   end Dump_Clist;

end Books.Table_Views.Test;
