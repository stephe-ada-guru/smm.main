--  Abstract :
--
--  Series view widget for Books application.
--
--  Copyright (C) 2002, 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Books.Database;
with Gtk.GEntry;
with Gtk.Label;
package Books.Table_Views.Series is

   type Gtk_Series_View_Record is new Gtk_Table_View_Record with private;
   type Gtk_Series_View is access all Gtk_Series_View_Record'Class;

   procedure Gtk_New
     (Series_View :    out Gtk_Series_View;
      DB          : in     Books.Database.Database_Access;
      Config      : in     SAL.Config_Files.Configuration_Access_Type);

   overriding procedure Create_List_View (Table_View : access Gtk_Series_View_Record; List : in Table_Names);
   overriding procedure Default_Add (Series_View : access Gtk_Series_View_Record);
   overriding function Main_Index_Name       (Series_View : access Gtk_Series_View_Record) return String;
   overriding procedure Update_Database      (Series_View : access Gtk_Series_View_Record);
   overriding procedure Insert_Database      (Series_View : access Gtk_Series_View_Record);
   overriding procedure Update_Primary_Display (Series_View : access Gtk_Series_View_Record);
   overriding procedure Clear_Primary_Display (Series_View : access Gtk_Series_View_Record);

private

   type Gtk_Series_View_Record is new Gtk_Table_View_Record with record
      --  Contents of Data_Table
      --  Row 0:
      Title_Label : Gtk.Label.Gtk_Label;
      Title_Text  : Gtk.GEntry.Gtk_Entry;

      --  End of Data_Table

   end record;

end Books.Table_Views.Series;
