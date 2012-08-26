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
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
with Books.Database.Link_Tables;
with Books.List_Views.Author;
with Books.List_Views.Title;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
package body Books.Table_Views.Collection is

   ----------
   --  Bodies (alphabetical order)

   procedure Create_GUI
     (Collection_View : access Gtk_Collection_View_Record'Class;
      Config      : in     SAL.Config_Files.Configuration_Access_Type)
   is begin
      Books.Table_Views.Create_GUI (Collection_View, Config);

      --  Data_Table
      --  Row 0
      Gtk.Label.Gtk_New (Collection_View.Title_Label, "Title");
      Gtk.Label.Set_Justify (Collection_View.Title_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Collection_View.Title_Text);

      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Title_Label, 0, 1, 0, 1);
      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Title_Text, 1, 3, 0, 1);

      --  Row 2
      Gtk.Label.Gtk_New (Collection_View.Year_Label, "Year");
      Gtk.Label.Set_Justify (Collection_View.Year_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Collection_View.Year_Text);

      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Year_Label, 0, 1, 1, 2);
      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Year_Text, 1, 3, 1, 2);

      Gtk.Table.Show_All (Collection_View.Data_Table);

      --  Hide invalid stuff
      Gtk.Check_Button.Hide (Collection_View.Links_Buttons (Books.Collection));
      Gtk.Check_Button.Hide (Collection_View.Links_Buttons (Series));

      Gtk.Radio_Button.Hide (Collection_View.List_Select (Books.Collection));
      Gtk.Radio_Button.Hide (Collection_View.List_Select (Series));
   end Create_GUI;

   overriding procedure Create_List_View (Table_View : access Gtk_Collection_View_Record; List : in Table_Names)
   is begin
      case List is
      when Author =>
         Books.List_Views.Author.Gtk_New
           (Table_View.List_Display (Author),
            Table_View.Links (Author, Books.Collection),
            Primary_Index => 2);

      when Books.Collection =>
         null;

      when Series =>
         null;

      when Title =>
         Books.List_Views.Title.Gtk_New
           (Table_View.List_Display (Title),
            Table_View.Links (Author, Books.Title),
            Primary_Index => 2);

      end case;

   end Create_List_View;

   overriding procedure Default_Add (Collection_View : access Gtk_Collection_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Collection_View.Title_Text, Gtk.GEntry.Get_Text (Collection_View.Find_Text));
      Gtk.GEntry.Set_Text (Collection_View.Year_Text, "");
      Gtk.GEntry.Grab_Focus (Collection_View.Title_Text);
   end Default_Add;

   procedure Gtk_New
     (Collection_View :    out Gtk_Collection_View;
      DB              : in     Books.Database.Database_Access;
      Config          : in     SAL.Config_Files.Configuration_Access_Type)
   is
      use Books.Database;
   begin
      Collection_View := new Gtk_Collection_View_Record;

      Collection_View.Siblings (Author) := new Data_Tables.Author.Table (DB);
      Collection_View.Siblings (Title)  := new Data_Tables.Title.Table (DB);
      Collection_View.Siblings (Series) := new Data_Tables.Series.Table (DB);

      Collection_View.Links (Author, Books.Collection) := new Link_Tables.Table
        (new Link_Tables.Link_Names'(Author, Books.Collection), DB);
      Collection_View.Links (Books.Collection, Title)  := new Link_Tables.Table
        (new Link_Tables.Link_Names'(Books.Collection, Title), DB);

      Collection_View.Primary_Kind  := Books.Collection;
      Collection_View.Primary_Table := new Data_Tables.Collection.Table (DB);

      Collection.Create_GUI (Collection_View, Config);

      Collection_View.Current_List := Title;

      Gtk.Radio_Button.Set_Active (Collection_View.List_Select (Collection_View.Current_List), True);

      To_Main (Collection_View);
   end Gtk_New;

   overriding procedure Insert_Database (Collection_View : access Gtk_Collection_View_Record)
   is
      Year       : Integer;
      Year_Valid : Boolean := True;
   begin
      begin
         Year := Integer'Value (Gtk.GEntry.Get_Text (Collection_View.Year_Text));
      exception
      when others =>
         Year_Valid := False;
      end;

      Database.Data_Tables.Collection.Insert
        (Database.Data_Tables.Collection.Table (Collection_View.Primary_Table.all),
         Title      => Gtk.GEntry.Get_Text (Collection_View.Title_Text),
         Year       => Year,
         Year_Valid => Year_Valid);
   end Insert_Database;

   overriding function Main_Index_Name (Collection_View : access Gtk_Collection_View_Record) return String
   is
      pragma Unreferenced (Collection_View);
   begin
      return "Collection";
   end Main_Index_Name;

   overriding procedure Update_Database (Collection_View : access Gtk_Collection_View_Record)
   is
      Year       : Integer;
      Year_Valid : Boolean := True;
   begin
      begin
         Year := Integer'Value (Gtk.GEntry.Get_Text (Collection_View.Year_Text));
      exception
      when others =>
         Year_Valid := False;
      end;

      Database.Data_Tables.Collection.Update
        (Database.Data_Tables.Collection.Table (Collection_View.Primary_Table.all),
         Title      => Gtk.GEntry.Get_Text (Collection_View.Title_Text),
         Year       => Year,
         Year_Valid => Year_Valid);
   end Update_Database;

   overriding procedure Update_Primary_Display (Collection_View : access Gtk_Collection_View_Record)
   is
      use Database.Data_Tables.Collection;
   begin
      Collection_View.Title_Text.Set_Text (Collection_View.Primary_Table.Field (Title_Index));

      if Collection_View.Primary_Table.Valid_Field (Year_Index) then
         Collection_View.Year_Text.Set_Text (Collection_View.Primary_Table.Field (Year_Index));
      else
         Collection_View.Year_Text.Set_Text ("");
      end if;
   end Update_Primary_Display;

   overriding procedure Clear_Primary_Display (Collection_View : access Gtk_Collection_View_Record)
   is begin
      Collection_View.Title_Text.Set_Text ("");
      Collection_View.Year_Text.Set_Text ("");
   end Clear_Primary_Display;

end Books.Table_Views.Collection;
