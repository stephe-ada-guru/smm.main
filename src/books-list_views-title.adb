--  Abstract :
--
--  See spec.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Books.Database.Data_Tables.Title;
with Interfaces.C.Strings;
package body Books.List_Views.Title is

   procedure Gtk_New (Title_View : out Gtk_List_View)
   is begin
      Gtk.Clist.Gtk_New
        (Gtk.Clist.Gtk_Clist (Title_View),
         Columns => 3,
         Titles  =>
           (1    => Interfaces.C.Strings.New_String ("ID"),
            2    => Interfaces.C.Strings.New_String ("Title"),
            3    => Interfaces.C.Strings.New_String ("Year")));
   end Gtk_New;

   overriding procedure Insert_List_Row
     (List_View : access Gtk_Title_List_Record;
      Table     : access Books.Database.Data_Tables.Table'Class;
      ID        : in     Books.Database.ID_Type)
   is
      use Database;
      use Interfaces.C.Strings; -- New_String
   begin
      if Table.Valid then
         List_View.Insert
           (0,
            (1 => New_String (Image (ID)),
             2 => New_String (Table.Field (Data_Tables.Title.Title_Index)),
             3 => New_String (Table.Field (Data_Tables.Title.Year_Index))));
      else
         --  bad IDs left over from delete
         List_View.Insert
           (0,
            (1 => New_String (Image (ID)),
             2 => New_String ("<bad id>"),
             3 => Null_Ptr));
      end if;
   end Insert_List_Row;

end Books.List_Views.Title;
