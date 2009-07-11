--  Abstract :
--
--  Author list widget for Books application.
--
--  Copyright (C) 2002, 2004, 2009 Stephen Leake.  All Rights Reserved.
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
--

with Books.Database;
with Gtk.GEntry;
with Gtk.Label;
package Books.Table_Views.Author is

   type Gtk_Author_View_Record is new Gtk_Table_View_Record with private;
   type Gtk_Author_View is access all Gtk_Author_View_Record'Class;

   procedure Gtk_New
     (Author_View :    out Gtk_Author_View;
      Parameters  : in     Create_Parameters_Type);

   procedure Initialize
     (Author_View : access Gtk_Author_View_Record'Class;
      Parameters  : in     Create_Parameters_Type);
   --  Create GUI elements, copy Tables, set initial database values.
   --  DB is assumed already Connected.

   ----------
   --  Override Table_View operations.
   overriding procedure Default_Add (Author_View : access Gtk_Author_View_Record);
   overriding function Main_Index_Name       (Author_View : access Gtk_Author_View_Record) return String;
   overriding procedure Update_Display_Child (Author_View : access Gtk_Author_View_Record);
   overriding procedure Update_Database      (Author_View : access Gtk_Author_View_Record);
   overriding procedure Insert_Database      (Author_View : access Gtk_Author_View_Record);

private

   type Gtk_Author_View_Record is new Gtk_Table_View_Record with record
      --  Contents of Data_Table
      --  Row 0:
      First_Label : Gtk.Label.Gtk_Label;
      First_Text  : Gtk.GEntry.Gtk_Entry;

      --  Row 1:
      Middle_Label : Gtk.Label.Gtk_Label;
      Middle_Text  : Gtk.GEntry.Gtk_Entry;

      --  Row 2:
      Last_Label : Gtk.Label.Gtk_Label;
      Last_Text  : Gtk.GEntry.Gtk_Entry;
      --  End of Data_Table

   end record;

end Books.Table_Views.Author;
