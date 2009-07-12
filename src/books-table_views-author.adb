--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Fixed;
with Books.Database.Data_Tables.Author;
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Title;
with Books.Database.Data_Tables.Series;
with Glib;
with Gtk.Clist;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
with Interfaces.C.Strings;
package body Books.Table_Views.Author is

   procedure Update_Display_AuthorTitle (Author_View : access Gtk_Author_View_Record);
   --  Update Author_View.Title_List.

   ----------
   --  Bodies (alphabetical order)

   procedure Create_GUI
     (Author_View : access Gtk_Author_View_Record'Class;
      Config      : in     SAL.Config_Files.Configuration_Access_Type)
   is
   begin
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
   begin
      Author_View := new Gtk_Author_View_Record;
      Initialize (Author_View, Parameters);
   end Gtk_New;

   procedure Initialize
     (Author_View : access Gtk_Author_View_Record'Class;
      Parameters  : in     Create_Parameters_Type)
   is begin
      Author.Create_GUI (Author_View, Parameters.Config);

      Author_View.Tables := Parameters.Tables;

      Author_View.Primary_Kind  := Books.Author;
      Author_View.Primary_Table := Author_View.Tables.Sibling (Books.Author);

      Gtk.Radio_Button.Set_Active (Author_View.List_Select (Title), True);

      To_Main (Author_View);

      Set_Display (Author_View, Database.Invalid_ID);
   end Initialize;

   overriding procedure Insert_Database (Author_View : access Gtk_Author_View_Record)
   is
   begin
      Database.Data_Tables.Author.Insert
        (Database.Data_Tables.Author.Table (Author_View.Primary_Table.all),
         First_Name  => Gtk.GEntry.Get_Text (Author_View.First_Text),
         Middle_Name => Gtk.GEntry.Get_Text (Author_View.Middle_Text),
         Last_Name   => Gtk.GEntry.Get_Text (Author_View.Last_Text));

      Author_View.Displayed_ID := Database.Data_Tables.ID (Author_View.Primary_Table.all);
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

   procedure Update_Display_AuthorTitle (Author_View : access Gtk_Author_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width     : Glib.Gint;
      pragma Unreferenced (Width);
      Author_ID : constant ID_Type := Data_Tables.ID (Author_View.Primary_Table.all);
   begin
      Link_Tables.AuthorTitle.Fetch_Links_Of
        (Author_View.Tables.AuthorTitle.all, Link_Tables.Author, Author_ID);

      if not Valid (Author_View.Tables.AuthorTitle.all) then
         Gtk.Clist.Clear (Author_View.List_Display (Title));
         return;
      end if;

      Gtk.Clist.Freeze (Author_View.List_Display (Title));
      Gtk.Clist.Clear (Author_View.List_Display (Title));

      loop
         declare
            use Ada.Strings, Ada.Strings.Fixed;
            Title_ID : constant ID_Type :=
              Link_Tables.AuthorTitle.ID (Author_View.Tables.AuthorTitle.all, Link_Tables.Title);
         begin
            Data_Tables.Fetch (Author_View.Tables.Sibling (Title).all, Title_ID);

            Gtk.Clist.Insert
              (Author_View.List_Display (Title),
               0,
               (1 => New_String (Image (Title_ID)),
                2 => New_String (Data_Tables.Title.Title (Author_View.Tables.Sibling (Title))),
                3 => New_String
                  (Trim
                     (Interfaces.Unsigned_16'Image
                        (Data_Tables.Title.Year (Author_View.Tables.Sibling (Title))), Left))));

            Next (Author_View.Tables.AuthorTitle.all);
            exit when not Valid (Author_View.Tables.AuthorTitle.all);
         end;
      end loop;

      Gtk.Clist.Sort (Author_View.List_Display (Title));
      Width := Gtk.Clist.Columns_Autosize (Author_View.List_Display (Title));
      Gtk.Clist.Thaw (Author_View.List_Display (Title));

   end Update_Display_AuthorTitle;

   procedure Update_Display_AuthorCollection (Author_View : access Gtk_Author_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width : Glib.Gint;
      pragma Unreferenced (Width);
   begin
      Data_Tables.Collection.Find_Editor
        (Author_View.Tables.Sibling (Collection), Data_Tables.ID (Author_View.Primary_Table.all));

      if not Valid (Author_View.Tables.Sibling (Collection).all) then
         Gtk.Clist.Clear (Author_View.List_Display (Collection));
         return;
      end if;

      Gtk.Clist.Freeze (Author_View.List_Display (Collection));
      Gtk.Clist.Clear (Author_View.List_Display (Collection));

      loop
         Gtk.Clist.Insert
           (Author_View.List_Display (Collection),
            0,
            (1 => New_String (Image (Data_Tables.ID (Author_View.Tables.Sibling (Collection).all))),
             2 => New_String (Data_Tables.Collection.Name (Author_View.Tables.Sibling (Collection)))));

         Next (Author_View.Tables.Sibling (Collection).all);
         exit when not Valid (Author_View.Tables.Sibling (Collection).all);
      end loop;

      Gtk.Clist.Sort (Author_View.List_Display (Collection));
      Width := Gtk.Clist.Columns_Autosize (Author_View.List_Display (Collection));
      Gtk.Clist.Thaw (Author_View.List_Display (Collection));

   end Update_Display_AuthorCollection;

   procedure Update_Display_AuthorSeries (Author_View : access Gtk_Author_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width : Glib.Gint;
      pragma Unreferenced (Width);
   begin
      Data_Tables.Series.Find_Author (Author_View.Tables.Sibling (Series), Author_View.Displayed_ID);

      if not Valid (Author_View.Tables.Sibling (Series).all) then
         Gtk.Clist.Clear (Author_View.List_Display (Series));
         return;
      end if;

      Gtk.Clist.Freeze (Author_View.List_Display (Series));
      Gtk.Clist.Clear (Author_View.List_Display (Series));

      loop
         Gtk.Clist.Insert
           (Author_View.List_Display (Series),
            0,
            (1 => New_String (Image (Data_Tables.ID (Author_View.Tables.Sibling (Series).all))),
             2 => New_String (Data_Tables.Series.Title (Author_View.Tables.Sibling (Series)))));

         Next (Author_View.Tables.Sibling (Series).all);
         exit when not Valid (Author_View.Tables.Sibling (Series).all);
      end loop;

      Gtk.Clist.Sort (Author_View.List_Display (Series));
      Width := Gtk.Clist.Columns_Autosize (Author_View.List_Display (Series));
      Gtk.Clist.Thaw (Author_View.List_Display (Series));

   end Update_Display_AuthorSeries;

   overriding procedure Update_Display_Child (Author_View : access Gtk_Author_View_Record)
   is begin
      if Database.Valid (Author_View.Primary_Table.all) then
         declare
            use Database.Data_Tables.Author;
         begin
            Gtk.GEntry.Set_Text (Author_View.First_Text, First_Name (Author_View.Primary_Table));
            Gtk.GEntry.Set_Text (Author_View.Middle_Text, Middle_Name (Author_View.Primary_Table));
            Gtk.GEntry.Set_Text (Author_View.Last_Text, Last_Name (Author_View.Primary_Table));
         end;

         case Author_View.Current_List is
         when Books.Author =>
            null;
         when Collection =>
            Update_Display_AuthorCollection (Author_View);
         when Series =>
            Update_Display_AuthorSeries (Author_View);
         when Title =>
            Update_Display_AuthorTitle (Author_View);
         end case;

      else
         Gtk.GEntry.Set_Text (Author_View.First_Text, "");
         Gtk.GEntry.Set_Text (Author_View.Middle_Text, "");
         Gtk.GEntry.Set_Text (Author_View.Last_Text, "");
         Gtk.Clist.Clear (Author_View.List_Display (Author_View.Current_List));
      end if;

   end Update_Display_Child;

end Books.Table_Views.Author;
