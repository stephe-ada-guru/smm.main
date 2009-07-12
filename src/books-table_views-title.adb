--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2003, 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Fixed;
with Books.Database.Data_Tables.Author;
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
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

   overriding procedure Default_Add (Title_View : access Gtk_Title_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Title_View.Title_Text, Gtk.GEntry.Get_Text (Title_View.Find_Text));
      Gtk.GEntry.Set_Text (Title_View.Year_Text, Ada.Strings.Unbounded.To_String (Title_View.Default_Year));
      Gtk.GEntry.Set_Text (Title_View.Rating_Text, "");
      Gtk.GEntry.Set_Text (Title_View.Comment_Text, "");
      Gtk.GEntry.Grab_Focus (Title_View.Title_Text);
   end Default_Add;

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

      Title_View.Tables := Parameters.Tables;

      Title_View.Primary_Kind  := Books.Title;
      Title_View.Primary_Table := Title_View.Tables.Sibling (Books.Title);

      To_Main (Title_View);

      --  Author is active by default, so need to switch to another to
      --  get hide actions to run.

      Gtk.Radio_Button.Set_Active (Title_View.List_Select (Series), True);
      Gtk.Radio_Button.Set_Active (Title_View.List_Select (Author), True);

      Set_Display (Title_View, Database.Invalid_ID);
   end Initialize;

   overriding procedure Insert_Database (Title_View : access Gtk_Title_View_Record)
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

      Database.Data_Tables.Title.Insert
        (Database.Data_Tables.Title.Table (Title_View.Primary_Table.all),
         Title        => Gtk.GEntry.Get_Text (Title_View.Title_Text),
         Year         => Year,
         Year_Valid   => Year_Valid,
         Comment      => Gtk.GEntry.Get_Text (Title_View.Comment_Text),
         Rating       => Rating,
         Rating_Valid => Rating_Valid);

      Title_View.Displayed_ID := Database.Data_Tables.ID (Title_View.Primary_Table.all);
   end Insert_Database;

   overriding function Main_Index_Name (Title_View : access Gtk_Title_View_Record) return String
   is
      pragma Unreferenced (Title_View);
   begin
      return "Title";
   end Main_Index_Name;

   overriding procedure Update_Database (Title_View : access Gtk_Title_View_Record)
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

      Database.Data_Tables.Title.Update
        (Database.Data_Tables.Title.Table (Title_View.Primary_Table.all),
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
   begin
      Link_Tables.AuthorTitle.Fetch_Links_Of
        (Title_View.Tables.AuthorTitle.all, Link_Tables.Title, Title_View.Displayed_ID);

      if not Valid (Title_View.Tables.AuthorTitle.all) then
         Gtk.Clist.Clear (Title_View.List_Display (Author));
         return;
      end if;

      Gtk.Clist.Freeze (Title_View.List_Display (Author));
      Gtk.Clist.Clear (Title_View.List_Display (Author));

      loop
         declare
            Author_ID : constant ID_Type :=
              Link_Tables.AuthorTitle.ID (Title_View.Tables.AuthorTitle.all, Link_Tables.Author);
         begin
            Data_Tables.Fetch (Title_View.Tables.Sibling (Author).all, Author_ID);

            Gtk.Clist.Insert
              (Title_View.List_Display (Author),
               0,
               (1 => New_String (Image (Author_ID)),
                2 => New_String (Data_Tables.Author.First_Name (Title_View.Tables.Sibling (Author))),
                3 => New_String (Data_Tables.Author.Middle_Name (Title_View.Tables.Sibling (Author))),
                4 => New_String (Data_Tables.Author.Last_Name (Title_View.Tables.Sibling (Author)))));

            Next (Title_View.Tables.AuthorTitle.all);
            exit when not Valid (Title_View.Tables.AuthorTitle.all);
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
      Link_Tables.CollectionTitle.Fetch_Links_Of
        (Title_View.Tables.CollectionTitle.all, Link_Tables.Title, Title_View.Displayed_ID);

      if not Valid (Title_View.Tables.CollectionTitle.all) then
         Gtk.Clist.Clear (Title_View.List_Display (Collection));
         return;
      end if;

      Gtk.Clist.Freeze (Title_View.List_Display (Collection));
      Gtk.Clist.Clear (Title_View.List_Display (Collection));

      loop
         Data_Tables.Fetch
           (Title_View.Tables.Sibling (Collection).all,
            Link_Tables.CollectionTitle.ID (Title_View.Tables.CollectionTitle.all, Link_Tables.Collection));

         if Valid (Title_View.Tables.Sibling (Collection).all) then
            Gtk.Clist.Insert
              (Title_View.List_Display (Collection),
               0,
               (1 => New_String (Image (Data_Tables.ID (Title_View.Tables.Sibling (Collection).all))),
                2 => New_String (Data_Tables.Collection.Name (Title_View.Tables.Sibling (Collection)))));
         else
            --  bad IDs accidently entered; allow deleting
            Gtk.Clist.Insert
              (Title_View.List_Display (Collection),
               0,
               (1 => New_String
                  (Image
                     (Link_Tables.CollectionTitle.ID (Title_View.Tables.CollectionTitle.all, Link_Tables.Collection))),
                2 => New_String ("<bad id>")));
         end if;

         Next (Title_View.Tables.CollectionTitle.all);
         exit when not Valid (Title_View.Tables.CollectionTitle.all);
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
      Link_Tables.SeriesTitle.Fetch_Links_Of
        (Title_View.Tables.SeriesTitle.all, Link_Tables.Title, Title_View.Displayed_ID);

      if not Valid (Title_View.Tables.SeriesTitle.all) then
         Gtk.Clist.Clear (Title_View.List_Display (Series));
         return;
      end if;

      Gtk.Clist.Freeze (Title_View.List_Display (Series));
      Gtk.Clist.Clear (Title_View.List_Display (Series));

      loop
         Data_Tables.Fetch
           (Title_View.Tables.Sibling (Series).all,
            Link_Tables.SeriesTitle.ID (Title_View.Tables.SeriesTitle.all, Link_Tables.Series));

         Gtk.Clist.Insert
           (Title_View.List_Display (Series),
            0,
            (1 => New_String (Image (Data_Tables.ID (Title_View.Tables.Sibling (Series).all))),
             2 => New_String (Data_Tables.Series.Title (Title_View.Tables.Sibling (Series)))));

         Books.Database.Next (Title_View.Tables.SeriesTitle.all);
         exit when not Valid (Title_View.Tables.SeriesTitle.all);
      end loop;

      Gtk.Clist.Sort (Title_View.List_Display (Series));
      Width := Gtk.Clist.Columns_Autosize (Title_View.List_Display (Series));
      Gtk.Clist.Thaw (Title_View.List_Display (Series));

   end Update_Display_SeriesTitle;

   overriding procedure Update_Display_Child (Title_View : access Gtk_Title_View_Record)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      if Database.Valid (Title_View.Primary_Table.all) then
         declare
            use Database.Data_Tables.Title;
         begin
            Gtk.GEntry.Set_Text (Title_View.Title_Text, Database.Data_Tables.Title.Title (Title_View.Primary_Table));

            Gtk.GEntry.Set_Text
              (Title_View.Year_Text,
               Trim (Interfaces.Unsigned_16'Image (Year (Title_View.Primary_Table)), Left));

            Gtk.GEntry.Set_Text (Title_View.Comment_Text, Comment (Title_View.Primary_Table));

            if Is_Rating_Valid (Title_View.Primary_Table) then
               Gtk.GEntry.Set_Text
                 (Title_View.Rating_Text, Interfaces.Unsigned_8'Image (Rating (Title_View.Primary_Table)));
            else
               Gtk.GEntry.Set_Text (Title_View.Rating_Text, "");
            end if;
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

      else
         Gtk.GEntry.Set_Text (Title_View.Title_Text, "");
         Gtk.GEntry.Set_Text (Title_View.Year_Text, "");
         Gtk.GEntry.Set_Text (Title_View.Comment_Text, "");
         Gtk.GEntry.Set_Text (Title_View.Rating_Text, "");
         Gtk.Clist.Clear (Title_View.List_Display (Title_View.Current_List));
      end if;
   end Update_Display_Child;

end Books.Table_Views.Title;
