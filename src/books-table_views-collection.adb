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
with Books.Database.Data_Tables.Title;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
with Interfaces.C.Strings;
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

   overriding procedure Default_Add (Collection_View : access Gtk_Collection_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Collection_View.Title_Text, Gtk.GEntry.Get_Text (Collection_View.Find_Text));
      Gtk.GEntry.Set_Text (Collection_View.Year_Text, "");
      Gtk.GEntry.Grab_Focus (Collection_View.Title_Text);
   end Default_Add;

   procedure Gtk_New
     (Collection_View :    out Gtk_Collection_View;
      Parameters      : in     Create_Parameters_Type)
   is begin
      Collection_View := new Gtk_Collection_View_Record;
      Initialize (Collection_View, Parameters);
   end Gtk_New;

   procedure Initialize
     (Collection_View : access Gtk_Collection_View_Record'Class;
      Parameters      : in     Create_Parameters_Type)
   is begin
      Collection.Create_GUI (Collection_View, Parameters.Config);

      Collection_View.Tables := Parameters.Tables;

      Collection_View.Primary_Kind  := Books.Collection;
      Collection_View.Primary_Table := Collection_View.Tables.Sibling (Books.Collection);

      Gtk.Radio_Button.Set_Active (Collection_View.List_Select (Title), True);

      To_Main (Collection_View);
   end Initialize;

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

   overriding function Main_Index_Name
     (Collection_View : access Gtk_Collection_View_Record)
     return String
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

   overriding procedure Insert_List_Row
     (Table_View : access Gtk_Collection_View_Record;
      Sibling_ID : in     Books.Database.ID_Type)
   is
      use Books.Database;
      use Interfaces.C.Strings; -- New_String
      Primary_ID    : constant ID_Type := Data_Tables.ID (Table_View.Primary_Table.all);
      Sibling_Table : Data_Tables.Table_Access renames Table_View.Tables.Sibling (Table_View.Current_List);
   begin
      Sibling_Table.Fetch (Sibling_ID);

      case Table_View.Current_List is
      when Books.Author =>
         Gtk.Clist.Insert
           (Table_View.List_Display (Author),
            0,
            (1 => New_String (Image (Sibling_ID)),
             2 => New_String (Sibling_Table.Field (Data_Tables.Author.First_Name_Index)),
             3 => New_String (Sibling_Table.Field (Data_Tables.Author.Middle_Name_Index)),
             4 => New_String (Sibling_Table.Field (Data_Tables.Author.Last_Name_Index))));

      when Books.Collection =>
         raise SAL.Programmer_Error;

      when Books.Series =>
         raise SAL.Programmer_Error;

      when Books.Title =>
            Gtk.Clist.Insert
              (Table_View.List_Display (Title),
               0,
               (1 => New_String (Image (Sibling_ID)),
                2 => New_String (Sibling_Table.Field (Data_Tables.Title.Title_Index)),
                3 => New_String (Sibling_Table.Field (Data_Tables.Title.Year_Index))));
      end case;

   end Insert_List_Row;

   overriding procedure Update_Primary_Display (Collection_View : access Gtk_Collection_View_Record)
   is
      use Database.Data_Tables.Collection;
   begin
      Collection_View.Title_Text.Set_Text (Collection_View.Primary_Table.Field (Title_Index));

      if Collection_View.Primary_Table.Valid_Field (Collection_View.Primary_Table.Field (Year_Index)) then
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
