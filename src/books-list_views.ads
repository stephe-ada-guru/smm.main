--  Abstract :
--
--  Base database list view widget for Books application.
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
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

with Books.Database.Link_Tables;
with Gtk.Clist;
package Books.List_Views is

   type Gtk_List_View_Record is abstract new Gtk.Clist.Gtk_Clist_Record with private;
   type Gtk_List_View is access all Gtk_List_View_Record'Class;

   procedure Insert_List_Row
     (List_View : access Gtk_List_View_Record;
      Table     : access Books.Database.Data_Tables.Table'Class;
      ID        : in     Books.Database.ID_Type)
      is abstract;
   --  Insert data from table for ID into List_View.
   --
   --  Called in loop while updating display; List_View is frozen.

private

   type Gtk_List_View_Record is abstract new Gtk.Clist.Gtk_Clist_Record with record
      Links         : access Books.Database.Link_Tables.Table;
      Primary_Index : Books.Database.Link_Tables.Link_Index;
   end record;

end Books.List_Views;
