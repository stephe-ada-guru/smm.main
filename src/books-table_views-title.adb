--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2003, 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with Books.Database.Data_Tables.Title;
with Gdk.Main;
with Glib;
with Gtk.Clist;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
with Interfaces.C.Strings;
package body Books.Table_Views.Title is

   procedure Update_Display_AuthorTitle (Title_View : access Gtk_Title_View_Record);
   procedure Update_Display_CollectionTitle (Title_View : access Gtk_Title_View_Record);
   procedure Update_Display_SeriesTitle (Title_View : access Gtk_Title_View_Record);


   ----------
   --  Bodies (alphabetical order)

   procedure Add_Link
     (Title_View : access Gtk_Title_View_Record;
      ID         : in     Books.Database.ID_Type;
      List       : in     Table_Name_Type)
   is
      use Books.Database;
   begin
      case List is
      when Author =>

         Books.Database.Link_Tables.AuthorTitle.Insert
           (Title_View.AuthorTitle_Table.all,
            (Link_Tables.Title  => Data_Tables.ID (Title_View.Title_Table.all),
             Link_Tables.Author => ID));

         Update_Display_AuthorTitle (Title_View);

      when Collection =>

         Books.Database.Link_Tables.CollectionTitle.Insert
           (Title_View.CollectionTitle_Table.all,
            (Link_Tables.Title  => Data_Tables.ID (Title_View.Title_Table.all),
             Link_Tables.Collection => ID));

         Update_Display_CollectionTitle (Title_View);

      when Series =>

         Books.Database.Link_Tables.SeriesTitle.Insert
           (Title_View.SeriesTitle_Table.all,
            (Link_Tables.Title  => Data_Tables.ID (Title_View.Title_Table.all),
             Link_Tables.Series => ID));

         Update_Display_SeriesTitle (Title_View);

      when Books.Title =>
         --  not possible
         Gdk.Main.Beep;
      end case;

   end Add_Link;

   procedure Create_GUI
     (Title_View : access Gtk_Title_View_Record'Class;
      Config     : in     SAL.Config_Files.Configuration_Access_Type)
   is begin
      Books.Table_Views.Create_GUI (Title_View, Config);

      -- Data_Table
      -- Row 0
      Gtk.Label.Gtk_New (Title_View.Title_Label, "Title");
      Gtk.Label.Set_Justify (Title_View.Title_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Title_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Title_Label, 0, 1, 0, 1);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Title_Text, 1, 3, 0, 1);

      -- Row 1
      Gtk.Label.Gtk_New (Title_View.Year_Label, "Year");
      Gtk.Label.Set_Justify (Title_View.Year_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Year_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Year_Label, 0, 1, 1, 2);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Year_Text, 1, 3, 1, 2);

      -- Row 2
      Gtk.Label.Gtk_New (Title_View.Comment_Label, "Comment");
      Gtk.Label.Set_Justify (Title_View.Comment_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Comment_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Comment_Label, 0, 1, 2, 3);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Comment_Text, 1, 3, 2, 3);

      -- Row 3
      Gtk.Label.Gtk_New (Title_View.Rating_Label, "Rating");
      Gtk.Label.Set_Justify (Title_View.Rating_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Title_View.Rating_Text);

      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Rating_Label, 0, 1, 3, 4);
      Gtk.Table.Attach (Title_View.Data_Table, Title_View.Rating_Text, 1, 3, 3, 4);

      Gtk.Table.Show_All (Title_View.Data_Table);

      --  Hide invalid stuff
      Gtk.Check_Button.Hide (Title_View.Links_Buttons (Books.Title));

      Gtk.Radio_Button.Hide (Title_View.List_Select (Books.Title));
   end Create_GUI;

   procedure Default_Add (Title_View : access Gtk_Title_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Title_View.Title_Text, Gtk.GEntry.Get_Text (Title_View.Find_Text));
      Gtk.GEntry.Set_Text (Title_View.Year_Text, Ada.Strings.Unbounded.To_String (Title_View.Default_Year));
      Gtk.GEntry.Set_Text (Title_View.Rating_Text, "");
      Gtk.GEntry.Set_Text (Title_View.Comment_Text, "");
      Gtk.GEntry.Grab_Focus (Title_View.Title_Text);
   end Default_Add;

   procedure Delete_Link (Title_View : access Gtk_Title_View_Record; ID : in Books.Database.ID_Type)
   is
      use Books.Database;
      Title_ID : constant ID_Type := Data_Tables.ID (Title_View.Title_Table.all);
   begin
      case Title_View.Current_List is
      when Author =>

         Books.Database.Link_Tables.AuthorTitle.Delete
           (Title_View.AuthorTitle_Table.all,
            (Link_Tables.Title  => Title_ID,
             Link_Tables.Author => ID));

         Update_Display_AuthorTitle (Title_View);

      when Collection =>

         Books.Database.Link_Tables.CollectionTitle.Delete
           (Title_View.CollectionTitle_Table.all,
            (Link_Tables.Title      => Title_ID,
             Link_Tables.Collection => ID));

         Update_Display_CollectionTitle (Title_View);

      when Series =>

         Books.Database.Link_Tables.SeriesTitle.Delete
           (Title_View.SeriesTitle_Table.all,
            (Link_Tables.Title  => Title_ID,
             Link_Tables.Series => ID));

         Update_Display_SeriesTitle (Title_View);

      when Books.Title =>
         --  not possible
         Gdk.Main.Beep;
      end case;

   end Delete_Link;

   procedure Gtk_New
     (Title_View :    out Gtk_Title_View;
      Parameters : in     Create_Parameters_Type)
   is begin
      Title_View := new Gtk_Title_View_Record;
      Initialize (Title_View, Parameters);
   end Gtk_New;

   procedure Initialize
     (Title_View : access Gtk_Title_View_Record'Class;
      Parameters : in     Create_Parameters_Type)
   is begin
      Title.Create_GUI (Title_View, Parameters.Config);

      Table_Views.Initialize_DB (Title_View, Parameters.DB);

      Title_View.Primary_Table := Books.Database.Data_Tables.Table_Access (Title_View.Title_Table);

      To_Main (Title_View);

      --  Author is active by default, so need to switch to another to
      --  get hide actions to run.

      Gtk.Radio_Button.Set_Active (Title_View.List_Select (Series), True);
      Gtk.Radio_Button.Set_Active (Title_View.List_Select (Author), True);

      Update_Display (Title_View);
   end Initialize;

   procedure Insert_Database (Title_View : access Gtk_Title_View_Record)
   is
      use Ada.Strings.Unbounded;

      Year         : Interfaces.Unsigned_16;
      Year_Valid   : Boolean := True;
      Rating       : Interfaces.Unsigned_8;
      Rating_Valid : Boolean := True;
   begin
      begin
         Year                    := Interfaces.Unsigned_16'Value (Gtk.GEntry.Get_Text (Title_View.Year_Text));
         Title_View.Default_Year := To_Unbounded_String (Gtk.GEntry.Get_Text (Title_View.Year_Text));
      exception
      when others =>
         Year_Valid := False;
      end;

      begin
         Rating := Interfaces.Unsigned_8'Value (Gtk.GEntry.Get_Text (Title_View.Rating_Text));
      exception
      when others =>
         Rating_Valid := False;
      end;

      Books.Database.Data_Tables.Title.Insert
        (Title_View.Title_Table.all,
         Title        => Gtk.GEntry.Get_Text (Title_View.Title_Text),
         Year         => Year,
         Year_Valid   => Year_Valid,
         Comment      => Gtk.GEntry.Get_Text (Title_View.Comment_Text),
         Rating       => Rating,
         Rating_Valid => Rating_Valid);
   end Insert_Database;

   function Main_Index_Name (Title_View : access Gtk_Title_View_Record) return String
   is
      pragma Unreferenced (Title_View);
   begin
      return "Title";
   end Main_Index_Name;

   procedure Update_Database (Title_View : access Gtk_Title_View_Record)
   is
      Year         : Interfaces.Unsigned_16;
      Year_Valid   : Boolean := True;
      Rating       : Interfaces.Unsigned_8;
      Rating_Valid : Boolean := True;
   begin
      begin
         Year := Interfaces.Unsigned_16'Value (Gtk.GEntry.Get_Text (Title_View.Year_Text));
      exception
      when others =>
         Year_Valid := False;
      end;

      begin
         Rating := Interfaces.Unsigned_8'Value (Gtk.GEntry.Get_Text (Title_View.Rating_Text));
      exception
      when others =>
         Rating_Valid := False;
      end;

      Books.Database.Data_Tables.Title.Update
        (Title_View.Title_Table.all,
         Title        => Gtk.GEntry.Get_Text (Title_View.Title_Text),
         Year         => Year,
         Year_Valid   => Year_Valid,
         Comment      => Gtk.GEntry.Get_Text (Title_View.Comment_Text),
         Rating       => Rating,
         Rating_Valid => Rating_Valid);
   end Update_Database;

   procedure Update_Display_AuthorTitle (Title_View : access Gtk_Title_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width    : Glib.Gint;
      pragma Unreferenced (Width);
      Title_ID : constant ID_Type := Data_Tables.ID (Title_View.Title_Table.all);
   begin
      begin
         Link_Tables.AuthorTitle.Fetch_Links_Of (Title_View.AuthorTitle_Table.all, Link_Tables.Title, Title_ID);
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Title_View.List_Display (Author));
         return;
      end;

      Gtk.Clist.Freeze (Title_View.List_Display (Author));
      Gtk.Clist.Clear (Title_View.List_Display (Author));

      loop
         declare
            Author_ID : constant ID_Type :=
              Link_Tables.AuthorTitle.ID (Title_View.AuthorTitle_Table.all, Link_Tables.Author);
         begin
            Data_Tables.Fetch (Title_View.Author_Table.all, Author_ID);

            Gtk.Clist.Insert
              (Title_View.List_Display (Author),
               0,
               (1 => New_String (Image (Author_ID)),
                2 => New_String (Data_Tables.Author.First_Name (Title_View.Author_Table.all)),
                3 => New_String (Data_Tables.Author.Middle_Name (Title_View.Author_Table.all)),
                4 => New_String (Data_Tables.Author.Last_Name (Title_View.Author_Table.all))));

            Books.Database.Next (Title_View.AuthorTitle_Table.all);
         exception
         when Database.No_Data =>
            exit;
         end;
      end loop;

      Gtk.Clist.Sort (Title_View.List_Display (Author));
      Width := Gtk.Clist.Columns_Autosize (Title_View.List_Display (Author));
      Gtk.Clist.Thaw (Title_View.List_Display (Author));

   end Update_Display_AuthorTitle;

   procedure Update_Display_CollectionTitle (Title_View : access Gtk_Title_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width : Glib.Gint;
      pragma Unreferenced (Width);
   begin
      begin
         Link_Tables.CollectionTitle.Fetch_Links_Of
           (Title_View.CollectionTitle_Table.all, Link_Tables.Title, Data_Tables.ID (Title_View.Title_Table.all));
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Title_View.List_Display (Collection));
         return;
      end;

      Gtk.Clist.Freeze (Title_View.List_Display (Collection));
      Gtk.Clist.Clear (Title_View.List_Display (Collection));

      loop
         Data_Tables.Fetch
           (Title_View.Collection_Table.all,
            Link_Tables.CollectionTitle.ID (Title_View.CollectionTitle_Table.all, Link_Tables.Collection));

         Gtk.Clist.Insert
           (Title_View.List_Display (Collection),
            0,
            (1 => New_String (Image (Data_Tables.ID (Title_View.Collection_Table.all))),
             2 => New_String (Data_Tables.Collection.Name (Title_View.Collection_Table.all))));

         begin
            Books.Database.Next (Title_View.CollectionTitle_Table.all);
         exception
         when Database.No_Data =>
            exit;
         end;
      end loop;

      Gtk.Clist.Sort (Title_View.List_Display (Collection));
      Width := Gtk.Clist.Columns_Autosize (Title_View.List_Display (Collection));
      Gtk.Clist.Thaw (Title_View.List_Display (Collection));

   end Update_Display_CollectionTitle;

   procedure Update_Display_SeriesTitle (Title_View : access Gtk_Title_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width : Glib.Gint;
      pragma Unreferenced (Width);
   begin
      begin
         Link_Tables.SeriesTitle.Fetch_Links_Of
           (Title_View.SeriesTitle_Table.all, Link_Tables.Title, Data_Tables.ID (Title_View.Title_Table.all));
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Title_View.List_Display (Series));
         return;
      end;

      Gtk.Clist.Freeze (Title_View.List_Display (Series));
      Gtk.Clist.Clear (Title_View.List_Display (Series));

      loop
         Data_Tables.Fetch
           (Title_View.Series_Table.all,
            Link_Tables.SeriesTitle.ID (Title_View.SeriesTitle_Table.all, Link_Tables.Series));

         Gtk.Clist.Insert
           (Title_View.List_Display (Series),
            0,
            (1 => New_String (Image (Data_Tables.ID (Title_View.Series_Table.all))),
             2 => New_String (Data_Tables.Series.Title (Title_View.Series_Table.all))));

         begin
            Books.Database.Next (Title_View.SeriesTitle_Table.all);
         exception
         when Database.No_Data =>
            exit;
         end;
      end loop;

      Gtk.Clist.Sort (Title_View.List_Display (Series));
      Width := Gtk.Clist.Columns_Autosize (Title_View.List_Display (Series));
      Gtk.Clist.Thaw (Title_View.List_Display (Series));

   end Update_Display_SeriesTitle;

   procedure Update_Display_Child (Title_View : access Gtk_Title_View_Record)
   is begin
      declare
         use Database.Data_Tables.Title;
      begin
         Gtk.GEntry.Set_Text
           (Title_View.Title_Text,
            Database.Data_Tables.Title.Title (Title_View.Title_Table.all));

         Gtk.GEntry.Set_Text
           (Title_View.Year_Text,
            Interfaces.Unsigned_16'Image (Year (Title_View.Title_Table.all)));

         Gtk.GEntry.Set_Text
           (Title_View.Comment_Text,
            Comment (Title_View.Title_Table.all));

         Gtk.GEntry.Set_Text
           (Title_View.Rating_Text,
            Interfaces.Unsigned_8'Image (Rating (Title_View.Title_Table.all)));
      end;

      case Title_View.Current_List is
      when Author =>
         Update_Display_AuthorTitle (Title_View);
      when Collection =>
         Update_Display_CollectionTitle (Title_View);
      when Series =>
         Update_Display_SeriesTitle (Title_View);
      when Books.Title =>
         null;
      end case;

   end Update_Display_Child;

end Books.Table_Views.Title;
