--  Abstract :
--
--  Title view widget for Books application.
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

with Ada.Strings.Unbounded;
with Books.Database;
with Gtk.GEntry;
with Gtk.Label;
package Books.Table_Views.Title is

   type Gtk_Title_View_Record is new Gtk_Table_View_Record with private;
   type Gtk_Title_View is access all Gtk_Title_View_Record'Class;

   procedure Gtk_New
     (Title_View :    out Gtk_Title_View;
      Parameters : in     Create_Parameters_Type);

   procedure Initialize
     (Title_View : access Gtk_Title_View_Record'Class;
      Parameters : in     Create_Parameters_Type);
   --  Create GUI elements, set initial database values. DB is assumed
   --  already Connected.

   ----------
   --  Override Table_View operations.

   overriding procedure Add_Link
     (Title_View : access Gtk_Title_View_Record;
      ID         : in     Books.Database.ID_Type;
      List       : in     Table_Name_Type);
   overriding procedure Default_Add (Title_View : access Gtk_Title_View_Record);
   overriding procedure Delete_Link
     (Title_View : access Gtk_Title_View_Record;
      ID         : in     Books.Database.ID_Type);
   overriding function Main_Index_Name       (Title_View : access Gtk_Title_View_Record) return String;
   overriding procedure Update_Display_Child (Title_View : access Gtk_Title_View_Record);
   overriding procedure Update_Database      (Title_View : access Gtk_Title_View_Record);
   overriding procedure Insert_Database      (Title_View : access Gtk_Title_View_Record);

private

   type Gtk_Title_View_Record is new Gtk_Table_View_Record with record
      --  Contents of Data_Table
      --  Row 0:
      Title_Label : Gtk.Label.Gtk_Label;
      Title_Text  : Gtk.GEntry.Gtk_Entry;

      --  Row 1:
      Year_Label : Gtk.Label.Gtk_Label;
      Year_Text  : Gtk.GEntry.Gtk_Entry;

      --  Row 2:
      Comment_Label : Gtk.Label.Gtk_Label;
      Comment_Text  : Gtk.GEntry.Gtk_Entry;

      --  Row 3:
      Rating_Label : Gtk.Label.Gtk_Label;
      Rating_Text  : Gtk.GEntry.Gtk_Entry;
      --  End of Data_Table

      Default_Year : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Books.Table_Views.Title;
