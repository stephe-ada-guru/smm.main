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

with Books.Database.Data_Tables.Series;
with Gdk.Main;
with Glib;
with Gtk.Clist;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
with Interfaces.C.Strings;
package body Books.Table_Views.Series is

   procedure Update_Display_SeriesTitle (Series_View : access Gtk_Series_View_Record);

   ----------
   --  Bodies (alphabetical order)

   overriding procedure Add_Link
     (Series_View : access Gtk_Series_View_Record;
      ID          : in     Books.Database.ID_Type;
      List        : in     Table_Name_Type)
   is
      use Database;
   begin
      case List is
      when Author =>
         --  Not a link table (series only have one author).
         Gdk.Main.Beep;

      when Collection =>
         --  Collections are not in series.
         Gdk.Main.Beep;

      when Books.Series =>
         --  not possible
         Gdk.Main.Beep;

      when Title =>

         Books.Database.Link_Tables.SeriesTitle.Insert
           (Series_View.SeriesTitle_Table.all,
            (Link_Tables.Series => Data_Tables.ID (Series_View.Series_Table.all),
             Link_Tables.Title  => ID));

         Update_Display_SeriesTitle (Series_View);

      end case;

   end Add_Link;

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

      --  Row 2
      Gtk.Label.Gtk_New (Series_View.Author_Label, "Author");
      Gtk.Label.Set_Justify (Series_View.Author_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Series_View.Author_Text);

      Gtk.Table.Attach (Series_View.Data_Table, Series_View.Author_Label, 0, 1, 3, 4);
      Gtk.Table.Attach (Series_View.Data_Table, Series_View.Author_Text, 1, 3, 3, 4);

      Gtk.Table.Show_All (Series_View.Data_Table);

      --  Hide invalid stuff
      Gtk.Check_Button.Hide (Series_View.Links_Buttons (Author));
      Gtk.Check_Button.Hide (Series_View.Links_Buttons (Collection));
      Gtk.Check_Button.Hide (Series_View.Links_Buttons (Books.Series));

      Gtk.Radio_Button.Hide (Series_View.List_Select (Books.Series));
      Gtk.Radio_Button.Hide (Series_View.List_Select (Collection));
   end Create_GUI;

   overriding procedure Default_Add (Series_View : access Gtk_Series_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Series_View.Title_Text, Gtk.GEntry.Get_Text (Series_View.Find_Text));
      Gtk.GEntry.Set_Text (Series_View.Author_Text, "");
      Gtk.GEntry.Grab_Focus (Series_View.Title_Text);
   end Default_Add;

   overriding procedure Delete_Link
     (Series_View : access Gtk_Series_View_Record;
      ID          : in     Books.Database.ID_Type)
   is
      use Books.Database;
      Series_ID : constant ID_Type := Data_Tables.ID (Series_View.Series_Table.all);
   begin
      case Series_View.Current_List is
      when Title =>

         Link_Tables.SeriesTitle.Delete
           (Series_View.SeriesTitle_Table.all,
            (Link_Tables.Series => Series_ID,
             Link_Tables.Title  => ID));

         Update_Display_SeriesTitle (Series_View);

      when others =>
         --  others are not link tables.
         Gdk.Main.Beep;
      end case;
   end Delete_Link;

   procedure Gtk_New
     (Series_View :    out Gtk_Series_View;
      Parameters  : in     Create_Parameters_Type)
   is begin
      Series_View := new Gtk_Series_View_Record;
      Initialize (Series_View, Parameters);
   end Gtk_New;

   procedure Initialize
     (Series_View : access Gtk_Series_View_Record'Class;
      Parameters  : in     Create_Parameters_Type)
   is begin
      Series.Create_GUI (Series_View, Parameters.Config);

      Table_Views.Initialize_DB (Series_View, Parameters.DB);

      Series_View.Primary_Table := Books.Database.Data_Tables.Table_Access (Series_View.Series_Table);

      Gtk.Radio_Button.Set_Active (Series_View.List_Select (Title), True);

      To_Main (Series_View);

      Update_Display (Series_View);
   end Initialize;

   overriding procedure Insert_Database (Series_View : access Gtk_Series_View_Record)
   is
      Author       : Database.ID_Type;
      Author_Valid : Boolean := True;
   begin
      begin
         Author := Database.Value (Gtk.GEntry.Get_Text (Series_View.Author_Text));
      exception
      when others =>
         Author_Valid := False;
      end;

      Books.Database.Data_Tables.Series.Insert
        (Series_View.Series_Table.all,
         Title        => Gtk.GEntry.Get_Text (Series_View.Title_Text),
         Author       => Author,
         Author_Valid => Author_Valid);
   end Insert_Database;

   overriding function Main_Index_Name (Series_View : access Gtk_Series_View_Record) return String
   is
      pragma Unreferenced (Series_View);
   begin
      return "Series";
   end Main_Index_Name;

   overriding procedure Update_Database (Series_View : access Gtk_Series_View_Record)
   is
      Author       : Database.ID_Type;
      Author_Valid : Boolean := True;
   begin
      begin
         Author := Database.Value (Gtk.GEntry.Get_Text (Series_View.Author_Text));
      exception
      when others =>
         Author_Valid := False;
      end;

      Books.Database.Data_Tables.Series.Update
        (Series_View.Series_Table.all,
         Title        => Gtk.GEntry.Get_Text (Series_View.Title_Text),
         Author       => Author,
         Author_Valid => Author_Valid);
   end Update_Database;

   procedure Update_Display_Author (Series_View : access Gtk_Series_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width     : Glib.Gint;
      pragma Unreferenced (Width);
      Author_ID : constant ID_Type := Data_Tables.Series.Author (Series_View.Series_Table.all);
   begin
      begin
         Data_Tables.Fetch (Series_View.Author_Table.all, Author_ID);
      exception
      when No_Data =>
         Gtk.Clist.Clear (Series_View.List_Display (Author));
         return;
      end;

      Gtk.Clist.Freeze (Series_View.List_Display (Author));
      Gtk.Clist.Clear (Series_View.List_Display (Author));

      Gtk.Clist.Insert
        (Series_View.List_Display (Author),
         0,
         (1 => New_String (Image (Author_ID)),
          2 => New_String (Data_Tables.Author.First_Name (Series_View.Author_Table.all)),
          3 => New_String (Data_Tables.Author.Middle_Name (Series_View.Author_Table.all)),
          4 => New_String (Data_Tables.Author.Last_Name (Series_View.Author_Table.all))));

      Width := Gtk.Clist.Columns_Autosize (Series_View.List_Display (Author));
      Gtk.Clist.Thaw (Series_View.List_Display (Author));

   end Update_Display_Author;

   procedure Update_Display_SeriesTitle (Series_View : access Gtk_Series_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width : Glib.Gint;
      pragma Unreferenced (Width);
      Series_ID : constant ID_Type := Data_Tables.ID (Series_View.Series_Table.all);
   begin
      Gtk.GEntry.Set_Text (Series_View.Title_Text, Database.Data_Tables.Series.Title (Series_View.Series_Table.all));
      Gtk.GEntry.Set_Text
        (Series_View.Author_Text,
         Database.Image (Database.Data_Tables.Series.Author (Series_View.Series_Table.all)));

      begin
         Link_Tables.SeriesTitle.Fetch_Links_Of (Series_View.SeriesTitle_Table.all, Link_Tables.Series, Series_ID);
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Series_View.List_Display (Title));
         return;
      end;

      Gtk.Clist.Freeze (Series_View.List_Display (Title));
      Gtk.Clist.Clear (Series_View.List_Display (Title));

      loop
         declare
            Title_ID : constant ID_Type :=
              Link_Tables.SeriesTitle.ID (Series_View.SeriesTitle_Table.all, Link_Tables.Title);
         begin
            Data_Tables.Fetch (Series_View.Title_Table.all, Title_ID);

            Gtk.Clist.Insert
              (Series_View.List_Display (Title),
               0,
               (1 => New_String (Image (Title_ID)),
                2 => New_String (Data_Tables.Title.Title (Series_View.Title_Table.all)),
                3 => New_String
                  (Interfaces.Unsigned_16'Image (Data_Tables.Title.Year (Series_View.Title_Table.all)))));

            Books.Database.Next (Series_View.SeriesTitle_Table.all);
         exception
         when Database.No_Data =>
            exit;
         end;
      end loop;

      Gtk.Clist.Sort (Series_View.List_Display (Title));
      Width := Gtk.Clist.Columns_Autosize (Series_View.List_Display (Title));
      Gtk.Clist.Thaw (Series_View.List_Display (Title));

   end Update_Display_SeriesTitle;

   overriding procedure Update_Display_Child (Series_View : access Gtk_Series_View_Record)
   is begin
      Gtk.Radio_Button.Set_Active (Series_View.List_Select (Title), True);
      case Series_View.Current_List is
      when Author =>
         Update_Display_Author (Series_View);
      when Collection =>
         null;
      when Books.Series =>
         null;
      when Title =>
         Update_Display_SeriesTitle (Series_View);
      end case;
   end Update_Display_Child;

end Books.Table_Views.Series;
