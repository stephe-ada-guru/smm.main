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
with Gdk.Main;
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

   overriding procedure Add_Link
     (Author_View : access Gtk_Author_View_Record;
      ID          : in     Books.Database.ID_Type;
      List        : in     Table_Name_Type)
   is
      use Books.Database;
   begin
      case List is
      when Books.Author =>
         --  Not actually possible.
         Gdk.Main.Beep;

      when Collection =>
         --  Not a link table (collections only have one editor).
         Gdk.Main.Beep;

      when Title =>

         Books.Database.Link_Tables.AuthorTitle.Insert
           (Author_View.AuthorTitle_Table.all,
            (Link_Tables.Author => Data_Tables.ID (Author_View.Author_Table.all),
             Link_Tables.Title  => ID));

         Update_Display_AuthorTitle (Author_View);

      when Series =>
         --  Not a link table (series only have one author).
         Gdk.Main.Beep;

      end case;

   end Add_Link;

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

   overriding procedure Delete_Link (Author_View : access Gtk_Author_View_Record; ID : in Books.Database.ID_Type)
   is
      use Books.Database;
      Author_ID : constant ID_Type := Data_Tables.ID (Author_View.Author_Table.all);
   begin
      case Author_View.Current_List is
      when Title =>

         Link_Tables.AuthorTitle.Delete
           (Author_View.AuthorTitle_Table.all,
            (Link_Tables.Author => Author_ID,
             Link_Tables.Title  => ID));

         Update_Display_AuthorTitle (Author_View);

      when others =>
         --  others are not link tables.
         Gdk.Main.Beep;
      end case;

   end Delete_Link;

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
      Parameters  : in Create_Parameters_Type)
   is begin
      Author.Create_GUI (Author_View, Parameters.Config);

      Table_Views.Initialize_DB (Author_View, Parameters.DB);

      Author_View.Primary_Table := Books.Database.Data_Tables.Table_Access (Author_View.Author_Table);

      Gtk.Radio_Button.Set_Active (Author_View.List_Select (Title), True);

      To_Main (Author_View);

      Update_Display (Author_View);
   end Initialize;

   overriding procedure Insert_Database (Author_View : access Gtk_Author_View_Record)
   is
   begin
      Books.Database.Data_Tables.Author.Insert
        (Author_View.Author_Table.all,
         First_Name  => Gtk.GEntry.Get_Text (Author_View.First_Text),
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
      Books.Database.Data_Tables.Author.Update
        (Author_View.Author_Table.all,
         First_Name  => Gtk.GEntry.Get_Text (Author_View.First_Text),
         Middle_Name => Gtk.GEntry.Get_Text (Author_View.Middle_Text),
         Last_Name   => Gtk.GEntry.Get_Text (Author_View.Last_Text));
   end Update_Database;

   procedure Update_Display_AuthorTitle (Author_View : access Gtk_Author_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width     : Glib.Gint;
      pragma Unreferenced (Width);
      Author_ID : constant ID_Type := Data_Tables.ID (Author_View.Author_Table.all);
   begin
      begin
         Link_Tables.AuthorTitle.Fetch_Links_Of
           (Author_View.AuthorTitle_Table.all, Link_Tables.Author, Author_ID);
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Author_View.List_Display (Title));
         return;
      end;

      Gtk.Clist.Freeze (Author_View.List_Display (Title));
      Gtk.Clist.Clear (Author_View.List_Display (Title));

      loop
         declare
            use Ada.Strings, Ada.Strings.Fixed;
            Title_ID : constant ID_Type :=
              Link_Tables.AuthorTitle.ID (Author_View.AuthorTitle_Table.all, Link_Tables.Title);
         begin
            Data_Tables.Fetch (Author_View.Title_Table.all, Title_ID);

            Gtk.Clist.Insert
              (Author_View.List_Display (Title),
               0,
               (1 => New_String (Image (Title_ID)),
                2 => New_String (Data_Tables.Title.Title (Author_View.Title_Table.all)),
                3 => New_String
                  (Trim (Interfaces.Unsigned_16'Image (Data_Tables.Title.Year (Author_View.Title_Table.all)), Left))));

            Books.Database.Next (Author_View.AuthorTitle_Table.all);
         exception
         when Books.Database.No_Data =>
            exit;
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
      begin
         Data_Tables.Collection.Find_Editor
           (Author_View.Collection_Table.all, Data_Tables.ID (Author_View.Author_Table.all));
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Author_View.List_Display (Collection));
         return;
      end;

      Gtk.Clist.Freeze (Author_View.List_Display (Collection));
      Gtk.Clist.Clear (Author_View.List_Display (Collection));

      loop
         Gtk.Clist.Insert
           (Author_View.List_Display (Collection),
            0,
            (1 => New_String (Image (Data_Tables.ID (Author_View.Collection_Table.all))),
             2 => New_String (Data_Tables.Collection.Name (Author_View.Collection_Table.all))));

         begin
            Books.Database.Next (Author_View.Collection_Table.all);
         exception
         when Books.Database.No_Data =>
            exit;
         end;
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
      begin
         Data_Tables.Series.Find_Author
           (Author_View.Series_Table.all, Data_Tables.ID (Author_View.Author_Table.all));
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Author_View.List_Display (Series));
         return;
      end;

      Gtk.Clist.Freeze (Author_View.List_Display (Series));
      Gtk.Clist.Clear (Author_View.List_Display (Series));

      loop
         Gtk.Clist.Insert
           (Author_View.List_Display (Series),
            0,
            (1 => New_String (Image (Data_Tables.ID (Author_View.Series_Table.all))),
             2 => New_String (Data_Tables.Series.Title (Author_View.Series_Table.all))));

         begin
            Books.Database.Next (Author_View.Series_Table.all);
         exception
         when Books.Database.No_Data =>
            exit;
         end;
      end loop;

      Gtk.Clist.Sort (Author_View.List_Display (Series));
      Width := Gtk.Clist.Columns_Autosize (Author_View.List_Display (Series));
      Gtk.Clist.Thaw (Author_View.List_Display (Series));

   end Update_Display_AuthorSeries;

   overriding procedure Update_Display_Child (Author_View : access Gtk_Author_View_Record)
   is begin
      declare
         use Database.Data_Tables.Author;
      begin
         Gtk.GEntry.Set_Text (Author_View.First_Text, First_Name (Author_View.Author_Table.all));
         Gtk.GEntry.Set_Text (Author_View.Middle_Text, Middle_Name (Author_View.Author_Table.all));
         Gtk.GEntry.Set_Text (Author_View.Last_Text, Last_Name (Author_View.Author_Table.all));
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

   end Update_Display_Child;

end Books.Table_Views.Author;
