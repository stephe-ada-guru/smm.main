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

with Books.Database.Data_Tables.Collection;
with Gdk.Main;
with Glib;
with Gtk.Clist;
with Gtk.Enums;
with Gtk.Radio_Button;
with Gtk.Table;
with Interfaces.C.Strings;
package body Books.Table_Views.Collection is

   procedure Update_Display_CollectionTitle (Collection_View : access Gtk_Collection_View_Record);

   ----------
   --  Bodies (alphabetical order)

   overriding procedure Add_Link
     (Collection_View : access Gtk_Collection_View_Record;
      ID              : in     Books.Database.ID_Type;
      List            : in     Table_Name_Type)
   is
      use Books.Database;
   begin
      case List is
      when Author =>
         --  Not a link table (collections only have one editor).
         Gdk.Main.Beep;

      when Books.Collection =>
         --  not possible
         Gdk.Main.Beep;

      when Series =>
         --  Collections are not in series.
         Gdk.Main.Beep;

      when Title =>

         Books.Database.Link_Tables.CollectionTitle.Insert
           (Collection_View.CollectionTitle_Table.all,
            (Link_Tables.Collection => Data_Tables.ID (Collection_View.Collection_Table.all),
             Link_Tables.Title      => ID));

         Update_Display_CollectionTitle (Collection_View);

      end case;

   end Add_Link;

   procedure Create_GUI
     (Collection_View : access Gtk_Collection_View_Record'Class;
      Config      : in     SAL.Config_Files.Configuration_Access_Type)
   is begin
      Books.Table_Views.Create_GUI (Collection_View, Config);

      --  Data_Table
      --  Row 0
      Gtk.Label.Gtk_New (Collection_View.Name_Label, "Name");
      Gtk.Label.Set_Justify (Collection_View.Name_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Collection_View.Name_Text);

      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Name_Label, 0, 1, 0, 1);
      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Name_Text, 1, 3, 0, 1);

      --  Row 2
      Gtk.Label.Gtk_New (Collection_View.Editor_Label, "Editor");
      Gtk.Label.Set_Justify (Collection_View.Editor_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Collection_View.Editor_Text);

      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Editor_Label, 0, 1, 3, 4);
      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Editor_Text, 1, 3, 3, 4);

      --  Row 3
      Gtk.Label.Gtk_New (Collection_View.Year_Label, "Year");
      Gtk.Label.Set_Justify (Collection_View.Year_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Collection_View.Year_Text);

      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Year_Label, 0, 1, 1, 2);
      Gtk.Table.Attach (Collection_View.Data_Table, Collection_View.Year_Text, 1, 3, 1, 2);

      Gtk.Table.Show_All (Collection_View.Data_Table);

      --  Hide invalid stuff
      Gtk.Check_Button.Hide (Collection_View.Links_Buttons (Author));
      Gtk.Check_Button.Hide (Collection_View.Links_Buttons (Books.Collection));
      Gtk.Check_Button.Hide (Collection_View.Links_Buttons (Series));

      Gtk.Radio_Button.Hide (Collection_View.List_Select (Books.Collection));
      Gtk.Radio_Button.Hide (Collection_View.List_Select (Series));
   end Create_GUI;

   overriding procedure Default_Add (Collection_View : access Gtk_Collection_View_Record)
   is begin
      Gtk.GEntry.Set_Text (Collection_View.Name_Text, Gtk.GEntry.Get_Text (Collection_View.Find_Text));
      Gtk.GEntry.Set_Text (Collection_View.Editor_Text, "");
      Gtk.GEntry.Set_Text (Collection_View.Year_Text, "");
      Gtk.GEntry.Grab_Focus (Collection_View.Name_Text);
   end Default_Add;

   overriding procedure Delete_Link
     (Collection_View : access Gtk_Collection_View_Record;
      ID              : in     Books.Database.ID_Type)
   is
      use Books.Database;
      Collection_ID : constant ID_Type := Data_Tables.ID (Collection_View.Collection_Table.all);
   begin
      case Collection_View.Current_List is
      when Title =>

         Link_Tables.CollectionTitle.Delete
           (Collection_View.CollectionTitle_Table.all,
            (Link_Tables.Collection => Collection_ID,
             Link_Tables.Title      => ID));

         Update_Display_CollectionTitle (Collection_View);

      when others =>
         --  others are not link tables.
         Gdk.Main.Beep;
      end case;

   end Delete_Link;

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

      Table_Views.Initialize_DB (Collection_View, Parameters.DB);

      Collection_View.Primary_Table := Books.Database.Data_Tables.Table_Access (Collection_View.Collection_Table);

      Gtk.Radio_Button.Set_Active (Collection_View.List_Select (Title), True);

      To_Main (Collection_View);

      Update_Display (Collection_View);
   end Initialize;

   overriding procedure Insert_Database (Collection_View : access Gtk_Collection_View_Record)
   is
      Editor       : Database.ID_Type;
      Editor_Valid : Boolean                := True;
      Year         : Interfaces.Unsigned_16;
      Year_Valid   : Boolean                := True;
   begin
      begin
         Editor := Database.Value (Gtk.GEntry.Get_Text (Collection_View.Editor_Text));
      exception
      when others =>
         Editor_Valid := False;
      end;

      begin
         Year := Interfaces.Unsigned_16'Value (Gtk.GEntry.Get_Text (Collection_View.Year_Text));
      exception
      when others =>
         Year_Valid := False;
      end;

      Books.Database.Data_Tables.Collection.Insert
        (Collection_View.Collection_Table.all,
         Name         => Gtk.GEntry.Get_Text (Collection_View.Name_Text),
         Editor       => Editor,
         Editor_Valid => Editor_Valid,
         Year         => Year,
         Year_Valid   => Year_Valid);
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
      Editor       : Database.ID_Type;
      Editor_Valid : Boolean                := True;
      Year         : Interfaces.Unsigned_16;
      Year_Valid   : Boolean                := True;
   begin
      begin
         Editor := Database.Value (Gtk.GEntry.Get_Text (Collection_View.Editor_Text));
      exception
      when others =>
         Editor_Valid := False;
      end;

      begin
         Year := Interfaces.Unsigned_16'Value (Gtk.GEntry.Get_Text (Collection_View.Year_Text));
      exception
      when others =>
         Year_Valid := False;
      end;

      Books.Database.Data_Tables.Collection.Update
        (Collection_View.Collection_Table.all,
         Name         => Gtk.GEntry.Get_Text (Collection_View.Name_Text),
         Editor       => Editor,
         Editor_Valid => Editor_Valid,
         Year         => Year,
         Year_Valid   => Year_Valid);
   end Update_Database;

   procedure Update_Display_Editor (Collection_View : access Gtk_Collection_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width     : Glib.Gint;
      pragma Unreferenced (Width);
      Editor_ID : constant ID_Type := Data_Tables.Collection.Editor (Collection_View.Collection_Table.all);
   begin
      begin
         Data_Tables.Fetch (Collection_View.Author_Table.all, Editor_ID);
      exception
      when No_Data =>
         Gtk.Clist.Clear (Collection_View.List_Display (Author));
         return;
      end;

      Gtk.Clist.Freeze (Collection_View.List_Display (Author));
      Gtk.Clist.Clear (Collection_View.List_Display (Author));

      Gtk.Clist.Insert
        (Collection_View.List_Display (Author),
         0,
         (1 => New_String (Image (Editor_ID)),
          2 => New_String (Data_Tables.Author.First_Name (Collection_View.Author_Table.all)),
          3 => New_String (Data_Tables.Author.Middle_Name (Collection_View.Author_Table.all)),
          4 => New_String (Data_Tables.Author.Last_Name (Collection_View.Author_Table.all))));

      Width := Gtk.Clist.Columns_Autosize (Collection_View.List_Display (Author));
      Gtk.Clist.Thaw (Collection_View.List_Display (Author));

   end Update_Display_Editor;

   procedure Update_Display_CollectionTitle (Collection_View : access Gtk_Collection_View_Record)
   is
      use Database, Interfaces.C.Strings;
      Width         : Glib.Gint;
      pragma Unreferenced (Width);
      Collection_ID : constant ID_Type := Data_Tables.ID (Collection_View.Collection_Table.all);
   begin
      declare
         use Database.Data_Tables.Collection;
      begin
         Gtk.GEntry.Set_Text (Collection_View.Name_Text,
                              Name (Collection_View.Collection_Table.all));
         Gtk.GEntry.Set_Text (Collection_View.Editor_Text,
                              Database.Image (Editor (Collection_View.Collection_Table.all)));
         Gtk.GEntry.Set_Text (Collection_View.Year_Text,
                              Interfaces.Unsigned_16'Image (Year (Collection_View.Collection_Table.all)));
      end;

      begin
         Link_Tables.CollectionTitle.Fetch_Links_Of
           (Collection_View.CollectionTitle_Table.all,
            Link_Tables.Collection,
            Collection_ID);
      exception
      when Database.No_Data =>
         Gtk.Clist.Clear (Collection_View.List_Display (Title));
         return;
      end;

      Gtk.Clist.Freeze (Collection_View.List_Display (Title));
      Gtk.Clist.Clear (Collection_View.List_Display (Title));

      loop
         declare
            Title_ID : constant ID_Type :=
              Link_Tables.CollectionTitle.ID (Collection_View.CollectionTitle_Table.all, Link_Tables.Title);
         begin
            Data_Tables.Fetch (Collection_View.Title_Table.all, Title_ID);

            Gtk.Clist.Insert
              (Collection_View.List_Display (Title),
               0,
               (1 => New_String (Image (Title_ID)),
                2 => New_String (Data_Tables.Title.Title (Collection_View.Title_Table.all)),
                3 => New_String
                  (Interfaces.Unsigned_16'Image (Data_Tables.Title.Year (Collection_View.Title_Table.all)))));

            Books.Database.Next (Collection_View.CollectionTitle_Table.all);
         exception
         when Database.No_Data =>
            exit;
         end;
      end loop;

      Gtk.Clist.Sort (Collection_View.List_Display (Title));
      Width := Gtk.Clist.Columns_Autosize (Collection_View.List_Display (Title));
      Gtk.Clist.Thaw (Collection_View.List_Display (Title));

   end Update_Display_CollectionTitle;

   overriding procedure Update_Display_Child (Collection_View : access Gtk_Collection_View_Record)
   is begin
      Gtk.Radio_Button.Set_Active (Collection_View.List_Select (Title), True);
      case Collection_View.Current_List is
      when Author =>
         Update_Display_Editor (Collection_View);
      when Books.Collection =>
         null;
      when Series =>
         null;
      when Title =>
         Update_Display_CollectionTitle (Collection_View);
      end case;
   end Update_Display_Child;

end Books.Table_Views.Collection;
