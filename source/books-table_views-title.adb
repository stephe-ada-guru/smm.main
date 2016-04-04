--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2004, 2009, 2012, 2016 Stephen Leake.  All Rights Reserved.
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
with Books.List_Views.Collection;
with Books.List_Views.Series;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
package body Books.Table_Views.Title is

   ----------
   --  Bodies (alphabetical order)

   procedure Create_GUI
     (Title_View : access Gtk_Title_View_Record'Class;
      Config     : in     SAL.Config_Files.Configuration_Access_Type)
   is begin
      Books.Table_Views.Create_GUI (Title_View, Config);

      --  Data_Table
      --  Row 0
      Gtk.Label.Gtk_New (Title_View.Title_Label, "Title");
      Gtk.Label.Set_Justify (Title_View.Title_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Title_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Title_Label, 0, 1, 0, 1);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Title_Text, 1, 3, 0, 1);

      --  Row 1
      Gtk.Label.Gtk_New (Title_View.Year_Label, "Year");
      Gtk.Label.Set_Justify (Title_View.Year_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Year_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Year_Label, 0, 1, 1, 2);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Year_Text, 1, 3, 1, 2);

      --  Row 2
      Gtk.Label.Gtk_New (Title_View.Comment_Label, "Comment");
      Gtk.Label.Set_Justify (Title_View.Comment_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Comment_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Comment_Label, 0, 1, 2, 3);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Comment_Text, 1, 3, 2, 3);

      --  Row 3
      Gtk.Label.Gtk_New (Title_View.Location_Label, "Location");
      Gtk.Label.Set_Justify (Title_View.Location_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Location_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Location_Label, 0, 1, 3, 4);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Location_Text, 1, 3, 3, 4);

      Gtk.Table.Show_All (Title_View.Data_Table);

      --  Hide invalid stuff
      Gtk.Check_Button.Hide (Title_View.Links_Buttons (Books.Title));

      Gtk.Radio_Button.Hide (Title_View.List_Select (Books.Title));
   end Create_GUI;

   overriding procedure Create_List_View (Table_View : access Gtk_Title_View_Record; List : in Table_Names)
   is begin
      case List is
      when Author =>
         Books.List_Views.Author.Gtk_New
           (Table_View.List_Display (Author),
            Table_View.Links (Author, Books.Title),
            Primary_Index => 1);

      when Collection =>
         Books.List_Views.Collection.Gtk_New
           (Table_View.List_Display (Collection),
            Table_View.Links (Collection, Books.Title),
            Primary_Index => 1);

      when Series =>
         Books.List_Views.Series.Gtk_New
           (Table_View.List_Display (Series),
            Table_View.Links (Series, Books.Title),
            Primary_Index => 1);

      when Books.Title =>
         null;
      end case;
   end Create_List_View;

   overriding procedure Default_Add (Title_View : access Gtk_Title_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Title_View.Title_Text, Gtk.GEntry.Get_Text (Title_View.Find_Text));
      Gtk.GEntry.Set_Text (Title_View.Year_Text, Ada.Strings.Unbounded.To_String (Title_View.Default_Year));
      Gtk.GEntry.Set_Text (Title_View.Location_Text, "");
      Gtk.GEntry.Set_Text (Title_View.Comment_Text, "");
      Gtk.GEntry.Grab_Focus (Title_View.Title_Text);
   end Default_Add;

   procedure Gtk_New
     (Title_View :    out Gtk_Title_View;
      DB         : in     Books.Database.Database_Access;
      Config     : in     SAL.Config_Files.Configuration_Access_Type)
   is
      use Books.Database;
   begin
      Title_View := new Gtk_Title_View_Record;

      Title_View.Siblings (Author)     := new Data_Tables.Author.Table (DB);
      Title_View.Siblings (Collection) := new Data_Tables.Collection.Table (DB);
      Title_View.Siblings (Series)     := new Data_Tables.Series.Table (DB);

      Title_View.Links (Author, Books.Title)     := new Link_Tables.Table
        (new Link_Tables.Link_Names'(Author, Books.Title), DB);
      Title_View.Links (Collection, Books.Title) := new Link_Tables.Table
        (new Link_Tables.Link_Names'(Collection, Books.Title), DB);
      Title_View.Links (Series, Books.Title)     := new Link_Tables.Table
        (new Link_Tables.Link_Names'(Series, Books.Title), DB);

      Title_View.Primary_Kind  := Books.Title;
      Title_View.Primary_Table := new Data_Tables.Title.Table (DB);

      Title.Create_GUI (Title_View, Config);

      Title_View.Current_List := Author;

      Gtk.Radio_Button.Set_Active (Title_View.List_Select (Title_View.Current_List), True);

      To_Main (Title_View);

   end Gtk_New;

   overriding procedure Insert_Database (Title_View : access Gtk_Title_View_Record)
   is
      use Ada.Strings.Unbounded;

      Year       : Integer;
      Year_Valid : Boolean := True;
   begin
      begin
         Year                    := Integer'Value (Gtk.GEntry.Get_Text (Title_View.Year_Text));
         Title_View.Default_Year := To_Unbounded_String (Gtk.GEntry.Get_Text (Title_View.Year_Text));
      exception
      when others =>
         --  Assume Year text box is empty. FIXME: report typos!
         Year_Valid := False;
      end;

      Database.Data_Tables.Title.Table (Title_View.Primary_Table.all).Insert
        (Title      => Gtk.GEntry.Get_Text (Title_View.Title_Text),
         Year       => Year,
         Year_Valid => Year_Valid,
         Comment    => Gtk.GEntry.Get_Text (Title_View.Comment_Text),
         Location   => Gtk.GEntry.Get_Text (Title_View.Location_Text));

   end Insert_Database;

   overriding function Main_Index_Name (Title_View : access Gtk_Title_View_Record) return String
   is
      pragma Unreferenced (Title_View);
   begin
      return "Title";
   end Main_Index_Name;

   overriding procedure Update_Database (Title_View : access Gtk_Title_View_Record)
   is
      Year       : Integer;
      Year_Valid : Boolean := True;
   begin
      begin
         Year := Integer'Value (Gtk.GEntry.Get_Text (Title_View.Year_Text));
      exception
      when others =>
         Year_Valid := False;
      end;

      Database.Data_Tables.Title.Table (Title_View.Primary_Table.all).Update
        (Title      => Gtk.GEntry.Get_Text (Title_View.Title_Text),
         Year       => Year,
         Year_Valid => Year_Valid,
         Comment    => Gtk.GEntry.Get_Text (Title_View.Comment_Text),
         Location   => Gtk.GEntry.Get_Text (Title_View.Location_Text));
   end Update_Database;

   overriding procedure Update_Primary_Display (Title_View : access Gtk_Title_View_Record)
   is
      use Database.Data_Tables.Title;
   begin
      Set_Text (Title_View.Title_Text,   Title_View.Primary_Table, Title_Index);
      Set_Text (Title_View.Year_Text,    Title_View.Primary_Table, Year_Index);
      Set_Text (Title_View.Comment_Text, Title_View.Primary_Table, Comment_Index);
      Set_Text (Title_View.Location_Text,  Title_View.Primary_Table, Location_Index);
   end Update_Primary_Display;

   overriding procedure Clear_Primary_Display (Title_View : access Gtk_Title_View_Record)
   is begin
      Title_View.Title_Text.Set_Text ("");
      Title_View.Year_Text.Set_Text ("");
      Title_View.Comment_Text.Set_Text ("");
      Title_View.Location_Text.Set_Text ("");
   end Clear_Primary_Display;

end Books.Table_Views.Title;
