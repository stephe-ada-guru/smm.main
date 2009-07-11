--  Abstract :
--
--  See spec
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
            Row_Strings : String_List_Type (0 .. Column_Last - 1);
         begin
            for Column in 0 .. Column_Last - 1 loop
               Row_Strings (Column) := +Get_Text (Clist, Row, Column);
            end loop;
            String_Tables.Add (Clist_Contents, Row_Strings);
         end;
      end loop;
   end Dump_Clist;

   function Add_Link_Button (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type
   is begin
      return Books.Table_Views.Add_Link_Button (Table_View);
   end Add_Link_Button;

   function Find_Entry (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type
   is begin
      return Books.Table_Views.Find_Entry (Table_View);
   end Find_Entry;

   function First_Link (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type
   is
      use Gdk.Test_Events;
   begin
      return Books.Table_Views.First_Link (Table_View);
   end First_Link;

   function Second_Link (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type
   is
      use Gdk.Test_Events;
   begin
      --  The offset is dependent on the font size, but this should be
      --  ok for most fonts. First_Link already includes a small amount to ensure mouse
      --  double click works.
      return First_Link (Table_View) + (0, 17);
   end Second_Link;

end Books.Table_Views.Test;
