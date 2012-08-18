--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Books.Database.Data_Tables.Author;
with Books.List_Views.Collection;
with Books.List_Views.Series;
with Books.List_Views.Title;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
package body Books.Table_Views.Author is

   ----------
   --  Bodies (alphabetical order)

   procedure Create_GUI
     (Author_View : access Gtk_Author_View_Record'Class;
      Config      : in     SAL.Config_Files.Configuration_Access_Type)
   is begin
      Books.Table_Views.Create_GUI (Author_View, Config);

      --  Data_Table
      --  Row 0
      Gtk.Label.Gtk_New (Author_View.First_Label, "First");
      Gtk.Label.Set_Justify (Author_View.First_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Author_View.First_Text);

      Gtk.Table.Attach (Author_View.Data_Table, Author_View.First_Label, 0, 1, 0, 1);
      Gtk.Table.Attach (Author_View.Data_Table, Author_View.First_Text, 1, 3, 0, 1);

      --  Row 1
      Gtk.Label.Gtk_New (Author_View.Middle_Label, "Middle");
      Gtk.Label.Set_Justify (Author_View.Middle_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Author_View.Middle_Text);

      Gtk.Table.Attach (Author_View.Data_Table, Author_View.Middle_Label, 0, 1, 1, 2);
      Gtk.Table.Attach (Author_View.Data_Table, Author_View.Middle_Text, 1, 3, 1, 2);

      --  Row 2
      Gtk.Label.Gtk_New (Author_View.Last_Label, "Last");
      Gtk.Label.Set_Justify (Author_View.Last_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Author_View.Last_Text);

      Gtk.Table.Attach (Author_View.Data_Table, Author_View.Last_Label, 0, 1, 2, 3);
      Gtk.Table.Attach (Author_View.Data_Table, Author_View.Last_Text, 1, 3, 2, 3);

      Gtk.Table.Show_All (Author_View.Data_Table);

      --  Hide invalid stuff
      Gtk.Check_Button.Hide (Author_View.Links_Buttons (Books.Author));
      Gtk.Check_Button.Hide (Author_View.Links_Buttons (Collection));

      Gtk.Radio_Button.Hide (Author_View.List_Select (Books.Author));
   end Create_GUI;

   overriding procedure Create_List_View (Table_View : access Gtk_Author_View_Record; List : in Table_Names)
   is begin
      case List is
      when Books.Author =>
         null;

      when Collection =>
         Books.List_Views.Collection.Gtk_New (Table_View.List_Display (Collection));

      when Series =>
         Books.List_Views.Series.Gtk_New (Table_View.List_Display (Series));

      when Title =>
         Books.List_Views.Title.Gtk_New (Table_View.List_Display (Title));

      end case;
   end Create_List_View;

   overriding procedure Default_Add (Author_View : access Gtk_Author_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Author_View.First_Text, "");
      Gtk.GEntry.Set_Text (Author_View.Middle_Text, "");
      Gtk.GEntry.Set_Text (Author_View.Last_Text, Gtk.GEntry.Get_Text (Author_View.Find_Text));
      Gtk.GEntry.Grab_Focus (Author_View.Last_Text);
   end Default_Add;

   procedure Gtk_New
     (Author_View :    out Gtk_Author_View;
      Parameters  : in     Create_Parameters_Type)
   is
      use Books.Database;
   begin
      Author_View := new Gtk_Author_View_Record;

      Author_View.Siblings := Parameters.Siblings;
      Author_View.Links    := Parameters.Links;

      Author_View.Primary_Kind  := Books.Author;
      Author_View.Primary_Table := Data_Tables.Table_Access (Author_View.Siblings (Books.Author));

      Author.Create_GUI (Author_View, Parameters.Config);

      Gtk.Radio_Button.Set_Active (Author_View.List_Select (Title), True);

      To_Main (Author_View);

      --  Clear_Display (Author_View); FIXME: need this?
   end Gtk_New;

   overriding procedure Insert_Database (Author_View : access Gtk_Author_View_Record)
   is begin
      Database.Data_Tables.Author.Table (Author_View.Primary_Table.all).Insert
        (First_Name  => Gtk.GEntry.Get_Text (Author_View.First_Text),
         Middle_Name => Gtk.GEntry.Get_Text (Author_View.Middle_Text),
         Last_Name   => Gtk.GEntry.Get_Text (Author_View.Last_Text));
   end Insert_Database;

   overriding function Main_Index_Name (Author_View : access Gtk_Author_View_Record) return String
   is
      pragma Unreferenced (Author_View);
   begin
      return "Author";
   end Main_Index_Name;

   overriding procedure Update_Database (Author_View : access Gtk_Author_View_Record)
   is begin
      Database.Data_Tables.Author.Update
        (Database.Data_Tables.Author.Table (Author_View.Primary_Table.all),
         First_Name  => Gtk.GEntry.Get_Text (Author_View.First_Text),
         Middle_Name => Gtk.GEntry.Get_Text (Author_View.Middle_Text),
         Last_Name   => Gtk.GEntry.Get_Text (Author_View.Last_Text));
   end Update_Database;

   overriding procedure Update_Primary_Display (Author_View : access Gtk_Author_View_Record)
   is
      use Database.Data_Tables.Author;
   begin
      Author_View.First_Text.Set_Text (Author_View.Primary_Table.Field (First_Name_Index));
      Author_View.Middle_Text.Set_Text (Author_View.Primary_Table.Field (Middle_Name_Index));
      Author_View.Last_Text.Set_Text (Author_View.Primary_Table.Field (Last_Name_Index));
   end Update_Primary_Display;

   overriding procedure Clear_Primary_Display (Author_View : access Gtk_Author_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Author_View.First_Text, "");
      Gtk.GEntry.Set_Text (Author_View.Middle_Text, "");
      Gtk.GEntry.Set_Text (Author_View.Last_Text, "");
   end Clear_Primary_Display;

end Books.Table_Views.Author;
