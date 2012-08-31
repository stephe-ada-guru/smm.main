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
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
with Books.Database.Link_Tables;
with Books.List_Views.Author;
with Books.List_Views.Title;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
package body Books.Table_Views.Series is

   ----------
   --  Bodies (alphabetical order)

   procedure Create_GUI
     (Series_View : access Gtk_Series_View_Record'Class;
      Config      : in     SAL.Config_Files.Configuration_Access_Type)
   is begin
      Books.Table_Views.Create_GUI (Series_View, Config);

      --  Data_Table
      --  Row 0
      Gtk.Label.Gtk_New (Series_View.Title_Label, "Title");
      Gtk.Label.Set_Justify (Series_View.Title_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Series_View.Title_Text);

      Gtk.Table.Attach (Series_View.Data_Table, Series_View.Title_Label, 0, 1, 0, 1);
      Gtk.Table.Attach (Series_View.Data_Table, Series_View.Title_Text, 1, 3, 0, 1);

      Gtk.Table.Show_All (Series_View.Data_Table);

      --  Hide invalid stuff
      Gtk.Check_Button.Hide (Series_View.Links_Buttons (Collection));
      Gtk.Check_Button.Hide (Series_View.Links_Buttons (Books.Series));

      Gtk.Radio_Button.Hide (Series_View.List_Select (Books.Series));
      Gtk.Radio_Button.Hide (Series_View.List_Select (Collection));
   end Create_GUI;

   overriding procedure Create_List_View (Table_View : access Gtk_Series_View_Record; List : in Table_Names)
   is begin
      case List is
      when Author =>
         Books.List_Views.Author.Gtk_New
           (Table_View.List_Display (Author),
            Table_View.Links (Author, Books.Series),
            Primary_Index => 1);

      when Collection | Books.Series =>
         null;

      when Title =>
         Books.List_Views.Title.Gtk_New
           (Table_View.List_Display (Title),
            Table_View.Links (Books.Series, Title),
            Primary_Index => 0);

      end case;
   end Create_List_View;

   overriding procedure Default_Add (Series_View : access Gtk_Series_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Series_View.Title_Text, Gtk.GEntry.Get_Text (Series_View.Find_Text));
      Gtk.GEntry.Grab_Focus (Series_View.Title_Text);
   end Default_Add;

   procedure Gtk_New
     (Series_View :    out Gtk_Series_View;
      DB          : in     Books.Database.Database_Access;
      Config      : in     SAL.Config_Files.Configuration_Access_Type)
   is
      use Books.Database;
   begin
      Series_View := new Gtk_Series_View_Record;

      Series_View.Siblings (Author) := new Data_Tables.Author.Table (DB);
      Series_View.Siblings (Title)  := new Data_Tables.Title.Table (DB);

      Series_View.Links (Author, Books.Series) := new Link_Tables.Table
        (new Link_Tables.Link_Names'(Author, Books.Series), DB);
      Series_View.Links (Books.Series, Title)  := new Link_Tables.Table
        (new Link_Tables.Link_Names'(Books.Series, Title), DB);

      Series_View.Primary_Kind  := Books.Series;
      Series_View.Primary_Table := new Data_Tables.Series.Table (DB);

      Series.Create_GUI (Series_View, Config);

      Series_View.Current_List := Title;

      Gtk.Radio_Button.Set_Active (Series_View.List_Select (Series_View.Current_List), True);

      To_Main (Series_View);

   end Gtk_New;

   overriding procedure Insert_Database (Series_View : access Gtk_Series_View_Record)
   is begin
      Database.Data_Tables.Series.Table (Series_View.Primary_Table.all).Insert
        (Title => Gtk.GEntry.Get_Text (Series_View.Title_Text));
   end Insert_Database;

   overriding function Main_Index_Name (Series_View : access Gtk_Series_View_Record) return String
   is
      pragma Unreferenced (Series_View);
   begin
      return "Series";
   end Main_Index_Name;

   overriding procedure Update_Database (Series_View : access Gtk_Series_View_Record)
   is begin
      Database.Data_Tables.Series.Table (Series_View.Primary_Table.all).Update
        (Title => Gtk.GEntry.Get_Text (Series_View.Title_Text));
   end Update_Database;

   overriding procedure Update_Primary_Display (Series_View : access Gtk_Series_View_Record)
   is
      use Database.Data_Tables.Series;
   begin
      Series_View.Title_Text.Set_Text (Series_View.Primary_Table.Field (Title_Index));
   end Update_Primary_Display;

   overriding procedure Clear_Primary_Display (Series_View : access Gtk_Series_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Series_View.Title_Text, "");
   end Clear_Primary_Display;

end Books.Table_Views.Series;
