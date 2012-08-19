--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2009, 2010, 2012 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Gdk.Event;
with Gdk.Main;
with Glib;
with Gtk.Box;
with Gtk.Button.Signal;
with Gtk.Clist.Signal;
with Gtk.Combo;
with Gtk.Enums;
with Gtk.GEntry.Signal;
with Gtk.Label;
with Gtk.List;
with Gtk.List_Item;
with Gtk.Object.Signal;
with Gtk.Scrolled_Window;
with Gtk.Widget;
with Gtk.Window.Config;
with Gtk.Window.Signal;
with SAL.Config_Files.Boolean;
package body Books.Table_Views is

   type Table_Array_Scroll_Type is array (Table_Names) of Gtk.Scrolled_Window.Gtk_Scrolled_Window;

   type Private_Stuff_Record is record
      Button_Box    : Gtk.Box.Gtk_Box;
      --  Main buttons
      Add_Button    : Gtk.Button.Gtk_Button;
      Edit_Button   : Gtk.Button.Gtk_Button;
      Delete_Button : Gtk.Button.Gtk_Button;
      Test_Button   : Gtk.Button.Gtk_Button;

      Enable_Test_Button : Boolean;

      --  Add buttons
      Insert_Button   : Gtk.Button.Gtk_Button;
      Cancel_Button   : Gtk.Button.Gtk_Button;

      --  Edit buttons
      Update_Button : Gtk.Button.Gtk_Button;

      Search_Table : Gtk.Table.Gtk_Table;
      --  Row 0:
      Find_Label : Gtk.Label.Gtk_Label;
      --  Find_Text is public
      Find_Next  : Gtk.Button.Gtk_Button;

      --  Row 1:
      Index_Label : Gtk.Label.Gtk_Label;
      Index_List  : Gtk.Combo.Gtk_Combo;
      Index_Last  : Gtk.Button.Gtk_Button;

      --  Row 2:
      ID_Label   : Gtk.Label.Gtk_Label;
      ID_Display : Gtk.Label.Gtk_Label;
      --  End of Search_Table

      Links_Table   : Gtk.Table.Gtk_Table;
      --  Row 0, 1:
      Links_Label   : Gtk.Label.Gtk_Label;

      List_Select_Hbox       : Gtk.Box.Gtk_Hbox;
      --  Contains List_Select_* in public view.

      List_Edit_Hbox          : Gtk.Box.Gtk_Hbox;
      --  Contains add, delete widgets
      List_Edit_Add_Button    : Gtk.Button.Gtk_Button;
      List_Edit_Delete_Button : Gtk.Button.Gtk_Button;

      List_Display_Scroll : Table_Array_Scroll_Type;

      Config : SAL.Config_Files.Configuration_Access_Type;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Private_Stuff_Record, Private_Stuff_Access);

   ----------
   --  Local declarations

   procedure On_Button_Add    (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Edit   (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Delete (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Cancel (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Insert (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Update (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Test   (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Find_Changed (GEntry : access Gtk.GEntry.Gtk_Entry_Record'Class);
   procedure On_Find_Next (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_List_Column (Clist : access Gtk.Clist.Gtk_Clist_Record'Class; Column : in Glib.Gint);
   function  On_List_Double_Click
     (Clist : access Gtk.Clist.Gtk_Clist_Record'Class;
      Event : in     Gdk.Event.Gdk_Event)
     return Boolean;
   procedure On_List_Edit_Add_Clicked (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_List_Edit_Delete_Clicked (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_List_Select_Clicked (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function On_Window_Configure_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Configure)
     return Boolean;

   procedure On_Window_Destroy (Object : access Gtk.Object.Gtk_Object_Record'Class);

   procedure To_Add (Table_View : in Gtk_Table_View);
   --  Show controls for Add display, hide main.

   procedure To_Edit (Table_View : in Gtk_Table_View);
   --  Show controls for Edit display, hide main.

   procedure Update_Display (Table_View : access Gtk_Table_View_Record'class);

   ----------
   --  Bodies (alphabetical order)

   procedure Add_Link (Table_View : in Gtk_Table_View; Kind : in Table_Names)
   is
      Link_ID : constant Database.ID_Type := ID (Table_View.Sibling_Views (Kind));
   begin
      if Table_View.Primary_Kind < Kind then
         Link_Table (Table_View, Kind).Insert ((ID (Table_View), Link_ID));
      else
         Link_Table (Table_View, Kind).Insert ((Link_ID, ID (Table_View)));
      end if;
   end Add_Link;

   procedure Create_GUI
     (Table_View : access Gtk_Table_View_Record'Class;
      Config     : in     SAL.Config_Files.Configuration_Access_Type)
   is
      use type Books.List_Views.Gtk_List_View;

      Vbox : Gtk.Box.Gtk_Box; -- contains everthing
   begin
      Gtk.Window.Initialize (Table_View, Gtk.Enums.Window_Toplevel);

      Gtk.Object.Signal.Connect_Destroy (Table_View, On_Window_Destroy'Access);
      Gtk.Window.Signal.Connect_Configure_Event (Table_View, On_Window_Configure_Event'Access);

      Table_View.Private_Stuff := new Private_Stuff_Record;

      Table_View.Private_Stuff.Config := Config;

      Gtk.Box.Gtk_New_Vbox (Vbox);
      Add (Table_View, Vbox);

      --  Buttons
      Gtk.Box.Gtk_New_Hbox (Table_View.Private_Stuff.Button_Box);
      Gtk.Box.Pack_Start (Vbox, Table_View.Private_Stuff.Button_Box, Expand => False);

      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Add_Button, "_Add");
      Gtk.Box.Add (Table_View.Private_Stuff.Button_Box, Table_View.Private_Stuff.Add_Button);
      Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Add_Button, On_Button_Add'Access);

      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Edit_Button, "_Edit");
      Gtk.Box.Add (Table_View.Private_Stuff.Button_Box, Table_View.Private_Stuff.Edit_Button);
      Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Edit_Button, On_Button_Edit'Access);

      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Delete_Button, "_Delete");
      Gtk.Box.Add (Table_View.Private_Stuff.Button_Box, Table_View.Private_Stuff.Delete_Button);
      Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Delete_Button, On_Button_Delete'Access);

      Table_View.Private_Stuff.Enable_Test_Button := SAL.Config_Files.Boolean.Read (Config.all, "Test_Button", False);
      if Table_View.Private_Stuff.Enable_Test_Button then
         Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Test_Button, "_Test");
         Gtk.Box.Add (Table_View.Private_Stuff.Button_Box, Table_View.Private_Stuff.Test_Button);
         Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Test_Button, On_Button_Test'Access);
      end if;

      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Insert_Button, "_Insert");
      Gtk.Box.Add (Table_View.Private_Stuff.Button_Box, Table_View.Private_Stuff.Insert_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Insert_Button);
      Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Insert_Button, On_Button_Insert'Access);

      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Update_Button, "_Update");
      Gtk.Box.Add (Table_View.Private_Stuff.Button_Box, Table_View.Private_Stuff.Update_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Update_Button);
      Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Update_Button, On_Button_Update'Access);

      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Cancel_Button, "_Cancel");
      Gtk.Box.Add (Table_View.Private_Stuff.Button_Box, Table_View.Private_Stuff.Cancel_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Cancel_Button);
      Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Cancel_Button, On_Button_Cancel'Access);

      --  Search_Table
      Gtk.Table.Gtk_New (Table_View.Private_Stuff.Search_Table, Rows => 3, Columns => 3, Homogeneous => False);
      Gtk.Box.Pack_Start (Vbox, Table_View.Private_Stuff.Search_Table, Expand => False);

      --  Row 0
      Gtk.Label.Gtk_New (Table_View.Private_Stuff.Find_Label, "Find");
      Gtk.Label.Set_Justify (Table_View.Private_Stuff.Find_Label, Gtk.Enums.Justify_Right);
      Gtk.GEntry.Gtk_New (Table_View.Find_Text);
      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Find_Next, "_Next");

      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Private_Stuff.Find_Label, 0, 1, 0, 1);
      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Find_Text, 1, 2, 0, 1);
      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Private_Stuff.Find_Next, 2, 3, 0, 1);

      Gtk.GEntry.Signal.Connect_Changed (Table_View.Find_Text, On_Find_Changed'Access);
      Gtk.Button.Signal.Connect_Clicked (Table_View.Private_Stuff.Find_Next, On_Find_Next'Access);

      --  Row 1
      Gtk.Label.Gtk_New (Table_View.Private_Stuff.Index_Label, "Index");
      Gtk.Label.Set_Justify (Table_View.Private_Stuff.Index_Label, Gtk.Enums.Justify_Right);
      Gtk.Combo.Gtk_New (Table_View.Private_Stuff.Index_List);
      declare
         use Gtk.Widget.Widget_List;
         Index_Widget_List : Glist;
         List_Item         : Gtk.List_Item.Gtk_List_Item;
      begin
         Gtk.List_Item.Gtk_New (List_Item, Main_Index_Name (Table_View));
         Append (Index_Widget_List, Gtk.Widget.Gtk_Widget (List_Item));
         Gtk.List_Item.Gtk_New (List_Item, "ID");
         Append (Index_Widget_List, Gtk.Widget.Gtk_Widget (List_Item));
         Gtk.List.Append_Items
           (Gtk.List.Gtk_List (Gtk.Combo.Get_List (Table_View.Private_Stuff.Index_List)),
            Index_Widget_List);

         Gtk.Combo.Set_Value_In_List (Table_View.Private_Stuff.Index_List);
      end;
      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.Index_Last, "_Last");

      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Private_Stuff.Index_Label, 0, 1, 1, 2);
      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Private_Stuff.Index_List, 1, 2, 1, 2);
      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Private_Stuff.Index_Last, 2, 3, 1, 2);

      --  Row 2
      Gtk.Label.Gtk_New (Table_View.Private_Stuff.ID_Label, "ID");
      Gtk.Label.Set_Justify (Table_View.Private_Stuff.ID_Label, Gtk.Enums.Justify_Right);
      Gtk.Label.Gtk_New (Table_View.Private_Stuff.ID_Display);

      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Private_Stuff.ID_Label, 0, 1, 2, 3);
      Gtk.Table.Attach (Table_View.Private_Stuff.Search_Table, Table_View.Private_Stuff.ID_Display, 1, 2, 2, 3);

      --  Links_Table
      Gtk.Table.Gtk_New (Table_View.Private_Stuff.Links_Table, Rows => 2, Columns => 3, Homogeneous => False);
      Gtk.Box.Pack_Start (Vbox, Table_View.Private_Stuff.Links_Table, Expand => False);

      --  Row 0
      Gtk.Label.Gtk_New (Table_View.Private_Stuff.Links_Label, "Links");
      Gtk.Table.Attach (Table_View.Private_Stuff.Links_Table, Table_View.Private_Stuff.Links_Label, 0, 1, 0, 1);

      Gtk.Check_Button.Gtk_New (Table_View.Links_Buttons (Title), "Title");
      Gtk.Table.Attach
        (Table_View.Private_Stuff.Links_Table, Table_View.Links_Buttons (Title), 2, 3, 0, 1);

      --  Row 1
      Gtk.Check_Button.Gtk_New (Table_View.Links_Buttons (Author), "Author");
      Gtk.Table.Attach
        (Table_View.Private_Stuff.Links_Table, Table_View.Links_Buttons (Author), 0, 1, 1, 2);

      Gtk.Check_Button.Gtk_New (Table_View.Links_Buttons (Collection), "Collection");
      Gtk.Table.Attach
        (Table_View.Private_Stuff.Links_Table, Table_View.Links_Buttons (Collection), 1, 2, 1, 2);

      Gtk.Check_Button.Gtk_New (Table_View.Links_Buttons (Series), "Series");
      Gtk.Table.Attach
        (Table_View.Private_Stuff.Links_Table, Table_View.Links_Buttons (Series), 2, 3, 1, 2);

      Gtk.Table.Hide (Table_View.Private_Stuff.Links_Table);

      --  Data_Table set by child type.
      Gtk.Table.Gtk_New (Table_View.Data_Table, Rows => 1, Columns => 1, Homogeneous => False);
      Gtk.Box.Pack_Start (Vbox, Table_View.Data_Table, Expand => False);
      --  contents set by child type.

      --  List_Select radio buttons
      Gtk.Box.Gtk_New_Hbox (Table_View.Private_Stuff.List_Select_Hbox);

      for I in Table_Names loop
         if I = Table_Names'First then
            --  Start the radio button group
            Gtk.Radio_Button.Gtk_New (Table_View.List_Select (I), Label => Image (I));
         else
            --  Add to the group
            Gtk.Radio_Button.Gtk_New
              (Table_View.List_Select (I), Table_View.List_Select (Table_Names'First), Label => Image (I));
         end if;

         Gtk.Button.Signal.Connect_Clicked (Table_View.List_Select (I), On_List_Select_Clicked'Access);
         Gtk.Box.Pack_Start (Table_View.Private_Stuff.List_Select_Hbox, Table_View.List_Select (I), Expand => False);
      end loop;

      Gtk.Box.Pack_Start (Vbox, Table_View.Private_Stuff.List_Select_Hbox, Expand => False);

      --  List editing
      Gtk.Box.Gtk_New_Hbox (Table_View.Private_Stuff.List_Edit_Hbox);
      Gtk.Button.Gtk_New_With_Mnemonic (Table_View.Private_Stuff.List_Edit_Add_Button, "Add Lin_k");
      Gtk.Button.Signal.Connect_Clicked
        (Table_View.Private_Stuff.List_Edit_Add_Button, On_List_Edit_Add_Clicked'Access);

      Gtk.Button.Gtk_New (Table_View.Private_Stuff.List_Edit_Delete_Button, "Delete Link");
      Gtk.Button.Signal.Connect_Clicked
        (Table_View.Private_Stuff.List_Edit_Delete_Button, On_List_Edit_Delete_Clicked'Access);

      Gtk.Box.Pack_Start
        (Table_View.Private_Stuff.List_Edit_Hbox, Table_View.Private_Stuff.List_Edit_Add_Button, Expand => False);
      Gtk.Box.Pack_Start
        (Table_View.Private_Stuff.List_Edit_Hbox, Table_View.Private_Stuff.List_Edit_Delete_Button, Expand => False);
      Gtk.Box.Pack_Start (Vbox, Table_View.Private_Stuff.List_Edit_Hbox, Expand => False);

      --  List displays.
      for I in Table_Names loop
         Create_List_View (Table_View, I);

         if Table_View.List_Display (I) /= null then
            Gtk.Scrolled_Window.Gtk_New (Table_View.Private_Stuff.List_Display_Scroll (I));
            Gtk.Scrolled_Window.Set_Policy
              (Table_View.Private_Stuff.List_Display_Scroll (I),
               H_Scrollbar_Policy => Gtk.Enums.Policy_Automatic,
               V_Scrollbar_Policy => Gtk.Enums.Policy_Automatic);

            Gtk.Box.Pack_Start (Vbox, Table_View.Private_Stuff.List_Display_Scroll (I));

            List_Views.Set_Sort_Column (Table_View.List_Display (I), 0);
            List_Views.Set_Sort_Type (Table_View.List_Display (I), Gtk.Clist.Ascending);

            Gtk.Clist.Signal.Connect_Click_Column (Table_View.List_Display (I), On_List_Column'Access);
            Gtk.Clist.Signal.Connect_Button_Press_Event (Table_View.List_Display (I), On_List_Double_Click'Access);

            Gtk.Scrolled_Window.Add (Table_View.Private_Stuff.List_Display_Scroll (I), Table_View.List_Display (I));
         end if;
      end loop;

      --  Show everything except the list displays, let To_Main etc
      --  hide stuff.
      Show_All (Table_View);

      for I in Table_Names loop
         Gtk.Scrolled_Window.Hide (Table_View.Private_Stuff.List_Display_Scroll (I));
      end loop;

      To_Main (Table_View);

   end Create_GUI;

   function Link_Table
     (Table_View : access constant Gtk_Table_View_Record;
      Kind       : in Table_Names)
     return Books.Database.Link_Tables.Table_Access
   is
      use Books.Database;
   begin
      case Table_View.Primary_Kind is
      when Author =>
         case Kind is
         when Author =>
            raise SAL.Programmer_Error;

         when Collection =>
            return Link_Tables.Table_Access (Table_View.Links (Author, Collection));

         when Series =>
            return Link_Tables.Table_Access (Table_View.Links (Author, Series));

         when Books.Title =>
            return Link_Tables.Table_Access (Table_View.Links (Author, Title));
         end case;

      when Collection =>
         case Kind is
         when Author =>
            return Link_Tables.Table_Access (Table_View.Links (Author, Collection));

         when Collection =>
            raise SAL.Programmer_Error;

         when Series =>
            raise SAL.Programmer_Error;

         when Books.Title =>
            return Link_Tables.Table_Access (Table_View.Links (Collection, Title));
         end case;

      when Series =>
         case Kind is
         when Author =>
            return Link_Tables.Table_Access (Table_View.Links (Author, Series));

         when Collection =>
            raise SAL.Programmer_Error;

         when Series =>
            raise SAL.Programmer_Error;

         when Books.Title =>
            return Link_Tables.Table_Access (Table_View.Links (Series, Title));
         end case;

      when Title =>
         case Kind is
         when Author =>
            return Link_Tables.Table_Access (Table_View.Links (Author, Title));

         when Collection =>
            return Link_Tables.Table_Access (Table_View.Links (Collection, Title));

         when Series =>
            return Link_Tables.Table_Access (Table_View.Links (Series, Title));

         when Books.Title =>
            raise SAL.Programmer_Error;
         end case;
      end case;
   end Link_Table;

   procedure Set_Display (Table_View : access Gtk_Table_View_Record'Class; ID : in Books.Database.ID_Type)
   is
      use Books.Database;
   begin
      Gtk.GEntry.Set_Text (Table_View.Find_Text, "");
      Data_Tables.Fetch (Table_View.Primary_Table.all, ID);
      Update_Display (Table_View);
   end Set_Display;

   procedure Set_Visibility (Table_View : access Gtk_Table_View_Record'class)
   is begin
      for I in Table_Names loop
         Gtk.Scrolled_Window.Hide (Table_View.Private_Stuff.List_Display_Scroll (I));
      end loop;

      To_Main (Table_View);

   end Set_Visibility;

   function ID (Table_View : access Gtk_Table_View_Record'Class) return Books.Database.ID_Type
   is begin
      return Books.Database.Data_Tables.ID (Table_View.Primary_Table.all);
   end ID;

   function ID_Image (Table_View : access Gtk_Table_View_Record'Class) return String
   is begin
      return Books.Database.Data_Tables.ID_Image (Table_View.Primary_Table.all);
   end ID_Image;

   procedure On_Button_Add (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      To_Add (Table_View);
      Default_Add (Table_View);
   end On_Button_Add;

   procedure On_Button_Cancel (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      To_Main (Table_View);
   end On_Button_Cancel;

   procedure On_Button_Delete (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      Books.Database.Data_Tables.Delete (Table_View.Primary_Table.all);
      Update_Display (Table_View);
   end On_Button_Delete;

   procedure On_Button_Edit (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      To_Edit (Table_View);
   end On_Button_Edit;

   procedure On_Button_Insert (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      --  Add main data
      Insert_Database (Table_View);

      --  Add links
      for I in Table_Names loop
         if Gtk.Check_Button.Get_Active (Table_View.Links_Buttons (I)) then
            Add_Link (Table_View, I);
         end if;
      end loop;

      To_Main (Table_View);

      Update_Display (Table_View);
   end On_Button_Insert;

   procedure On_Button_Test (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      if Test_Hook /= null then
         Test_Hook.all (Table_View);
      end if;
   exception
   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Test_Hook : " &
           Ada.Exceptions.Exception_Name (E) & " " &
           Ada.Exceptions.Exception_Message (E));
   end On_Button_Test;

   procedure On_Button_Update (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      Update_Database (Table_View);
      To_Main (Table_View);
   end On_Button_Update;

   procedure On_Find_Changed (GEntry : access Gtk.GEntry.Gtk_Entry_Record'Class)
   is
      use Books.Database;
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.GEntry.Get_Toplevel (GEntry));
   begin
      --  FIXME: should use selected Index.
      Data_Tables.Find_By_Name (Table_View.Primary_Table.all, Gtk.GEntry.Get_Text (GEntry));
      Update_Display (Table_View);
   end On_Find_Changed;

   procedure On_Find_Next (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      use Books.Database;
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      Table_View.Primary_Table.Next;
      if Table_View.Primary_Table.Valid then
         Update_Display (Table_View);
      else
         --  restart search
         On_Find_Changed (Table_View.Find_Text);
      end if;
   end On_Find_Next;

   procedure On_List_Edit_Add_Clicked (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      Add_Link (Table_View, Table_View.Current_List);
      Update_Display (Table_View);

   end On_List_Edit_Add_Clicked;

   procedure On_List_Edit_Delete_Clicked (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      use Books.Database;
      use Books.Database.Link_Tables;
      use type Glib.Guint;
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));

      List_Display : List_Views.Gtk_List_View renames Table_View.List_Display (Table_View.Current_List);
      Selected     : constant Gtk.Enums.Gint_List.Glist :=  List_Views.Get_Selection (List_Display);

   begin
      --  List_View selection mode is Selection_Single, so we only have to
      --  deal with deleting one link.

      if Gtk.Enums.Gint_List.Length (Selected) = 0 then
         Gdk.Main.Beep;
         return;
      end if;

      declare
         Row               : constant Glib.Gint := Gtk.Enums.Gint_List.Get_Data (Gtk.Enums.Gint_List.First (Selected));
         Sibling_ID_String : constant String    := List_Views.Get_Text (List_Display, Row, Column => 0);
         Sibling_ID        : constant ID_Type   := ID_Type'Value (Sibling_ID_String);

         Primary_Kind : Table_Names renames Table_View.Primary_Kind;
         Sibling_Kind : Table_Names renames Table_View.Current_List;

         Link_Table : constant Link_Tables.Table_Access := Table_View.Link_Table (Sibling_Kind);
      begin
         if Primary_Kind < Sibling_Kind then
            Link_Table.Delete ((ID (Table_View), Sibling_ID));
         else
            Link_Table.Delete ((Sibling_ID, ID (Table_View)));
         end if;

         Update_Display (Table_View);
      end;
   end On_List_Edit_Delete_Clicked;

   procedure On_List_Column (Clist : access Gtk.Clist.Gtk_Clist_Record'Class; Column : in Glib.Gint)
   is begin
      Clist.Set_Sort_Column (Column);
      Clist.Sort;
   end On_List_Column;

   function On_List_Double_Click
     (Clist : access Gtk.Clist.Gtk_Clist_Record'Class;
      Event : in     Gdk.Event.Gdk_Event)
     return Boolean
   is
      use Gdk.Event;
      use type Glib.Guint;
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Clist.Get_Toplevel);
   begin
      case Get_Event_Type (Event) is
      when Gdk_2button_Press =>
         declare
            List_View : List_Views.Gtk_List_View renames Table_View.List_Display (Table_View.Current_List);

            Selected_Rows : constant Gtk.Enums.Gint_List.Glist := List_Views.Get_Selection (List_View);
         begin
            if Gtk.Enums.Gint_List.Length (Selected_Rows) >= 1 then
               declare
                  ID_String : constant String := List_View.Get_Text
                    (Row    => Gtk.Enums.Gint_List.Get_Data (Selected_Rows),
                     Column => 0);
               begin
                  if ID_String'Length = 0 then
                     raise SAL.Programmer_Error with "column 0 has no text";
                  end if;

                  Set_Display (Table_View.Sibling_Views (Table_View.Current_List), Database.ID_Type'Value (ID_String));
               end;
               return True;
            else
               return False;
            end if;
         end;

      when others =>
         return False;
      end case;
   end On_List_Double_Click;

   procedure On_List_Select_Clicked (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      use type Gtk.Radio_Button.Gtk_Radio_Button;
      Radio_Button : constant Gtk.Radio_Button.Gtk_Radio_Button := Gtk.Radio_Button.Gtk_Radio_Button (Button);
      Table_View   : constant Gtk_Table_View                    := Gtk_Table_View (Gtk.Button.Get_Toplevel (Button));
   begin
      Gtk.Scrolled_Window.Hide (Table_View.Private_Stuff.List_Display_Scroll (Table_View.Current_List));

      for I in Table_View.List_Select'Range loop
         if Table_View.List_Select (I) = Radio_Button then
            Table_View.Current_List := I;
            Gtk.Scrolled_Window.Show (Table_View.Private_Stuff.List_Display_Scroll (I));
         end if;
      end loop;
      Update_Display (Table_View);
   end On_List_Select_Clicked;

   function On_Window_Configure_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Configure)
     return Boolean
   is
      pragma Unreferenced (Event);

      Table_View : constant Gtk_Table_View := Gtk_Table_View (Widget);
      Title      : constant String         := Get_Title (Table_View);
   begin
      Gtk.Window.Config.Save_Geometry (Table_View, Table_View.Private_Stuff.Config.all, Title);
      return False; -- propagate
   end On_Window_Configure_Event;

   procedure On_Window_Destroy (Object : access Gtk.Object.Gtk_Object_Record'Class)
   is
      Table_View : constant Gtk_Table_View := Gtk_Table_View (Object);
   begin
      Free (Table_View.Private_Stuff);
   end On_Window_Destroy;

   procedure Set_Sibling_Views
     (Table_View    : access Gtk_Table_View_Record'Class;
      Sibling_Views : in     Table_Array_Table_View_Type)
   is begin
      Table_View.Sibling_Views := Sibling_Views;
   end Set_Sibling_Views;

   procedure To_Add (Table_View : in Gtk_Table_View)
   is begin
      Gtk.Button.Hide (Table_View.Private_Stuff.Add_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Edit_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Delete_Button);
      if Table_View.Private_Stuff.Enable_Test_Button then
         Gtk.Button.Hide (Table_View.Private_Stuff.Test_Button);
      end if;
      Gtk.Table.Hide (Table_View.Private_Stuff.Search_Table);

      Gtk.Button.Show (Table_View.Private_Stuff.Insert_Button);
      Gtk.Button.Show (Table_View.Private_Stuff.Cancel_Button);
      Gtk.Table.Show (Table_View.Private_Stuff.Links_Table);

      Gtk.Box.Hide (Table_View.Private_Stuff.List_Select_Hbox);
      Gtk.Box.Hide (Table_View.Private_Stuff.List_Edit_Hbox);
      Gtk.Scrolled_Window.Hide (Table_View.Private_Stuff.List_Display_Scroll (Table_View.Current_List));
   end To_Add;

   procedure To_Edit (Table_View : in Gtk_Table_View)
   is begin
      Gtk.Button.Hide (Table_View.Private_Stuff.Add_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Edit_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Delete_Button);
      if Table_View.Private_Stuff.Enable_Test_Button then
         Gtk.Button.Hide (Table_View.Private_Stuff.Test_Button);
      end if;
      Gtk.Table.Hide (Table_View.Private_Stuff.Search_Table);

      Gtk.Button.Show (Table_View.Private_Stuff.Update_Button);
      Gtk.Button.Show (Table_View.Private_Stuff.Cancel_Button);

      Gtk.Box.Hide (Table_View.Private_Stuff.List_Select_Hbox);
      Gtk.Box.Hide (Table_View.Private_Stuff.List_Edit_Hbox);
      Gtk.Scrolled_Window.Hide (Table_View.Private_Stuff.List_Display_Scroll (Table_View.Current_List));

   end To_Edit;

   procedure To_Main (Table_View : access Gtk_Table_View_Record'class)
   is begin
      Gtk.Button.Show (Table_View.Private_Stuff.Add_Button);
      Gtk.Button.Show (Table_View.Private_Stuff.Edit_Button);
      Gtk.Button.Show (Table_View.Private_Stuff.Delete_Button);
      if Table_View.Private_Stuff.Enable_Test_Button then
         Gtk.Button.Show (Table_View.Private_Stuff.Test_Button);
      end if;
      Gtk.Table.Show (Table_View.Private_Stuff.Search_Table);

      Gtk.Button.Hide (Table_View.Private_Stuff.Update_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Insert_Button);
      Gtk.Button.Hide (Table_View.Private_Stuff.Cancel_Button);
      Gtk.Table.Hide (Table_View.Private_Stuff.Links_Table);

      Gtk.Box.Show (Table_View.Private_Stuff.List_Select_Hbox);
      Gtk.Box.Show (Table_View.Private_Stuff.List_Edit_Hbox);
      Gtk.Scrolled_Window.Show (Table_View.Private_Stuff.List_Display_Scroll (Table_View.Current_List));

      Gtk.GEntry.Grab_Focus (Table_View.Find_Text);
   end To_Main;

   procedure Update_Display (Table_View : access Gtk_Table_View_Record'class)
   is
      use type Books.Database.ID_Type;
      use Database;
      Width   : Glib.Gint;
      pragma Unreferenced (Width);

      Link_Table    : Link_Tables.Table_Access renames Table_View.Link_Table (Table_View.Current_List);
      Sibling_Table : constant Data_Tables.Table_Access := Data_Tables.Table_Access
        (Table_View.Siblings (Table_View.Current_List));
      List_View     : List_Views.Gtk_List_View renames Table_View.List_Display (Table_View.Current_List);
   begin
      --  Caller is responsible for calling Find to get a valid record.

      if not Table_View.Primary_Table.Valid then
         Gtk.Label.Set_Text (Table_View.Private_Stuff.ID_Display, "");
         Clear_Primary_Display (Table_View);
         List_View.Clear;
         return;
      end if;

      Gtk.Label.Set_Text (Table_View.Private_Stuff.ID_Display, ID_Image (Table_View));
      Update_Primary_Display (Table_View);
      Link_Table.Find (Table_View.Primary_Kind, ID (Table_View));

      if not Link_Table.Valid then
         List_View.Clear;
         return;
      end if;

      List_View.Freeze;
      List_View.Clear;

      loop
         declare
            Sibling_ID : constant Books.Database.ID_Type := Link_Table.ID (Table_View.Current_List);
         begin
            Sibling_Table.Fetch (Sibling_ID);

            List_View.Insert_List_Row (Sibling_Table, Sibling_ID);
         end;

         Link_Table.Next;
         exit when not Link_Table.Valid;
      end loop;

      List_View.Sort;
      Width := List_View.Columns_Autosize;
      List_View.Thaw;

   end Update_Display;

   function Find_Entry (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type
   is
      use Gdk.Test_Events;
   begin
      --  Window_Position is just wrong for tables, so hard code an
      --  approximate answer.
      return Window_Position (Table_View) + (56, 30);
   end Find_Entry;

   function Add_Link_Button (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type
   is
      use Gdk.Test_Events;
   begin
      --  Widget_Position (list_edit_add_button) returns the position
      --  relative to the top level, not relative to its containing
      --  hbox, for some reason! So get the position of the HBox, instead.
      --
      --  add a small offset so a mouse click will actually hit the button
      return Window_Position (Table_View) +
        Widget_Position_Toplevel (Table_View.Private_Stuff.List_Edit_Hbox) +
        (5, 5);
   end Add_Link_Button;

   function First_Link (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type
   is
      use Gdk.Test_Events;
   begin
      --  List_Display_Scroll includes the column titles; can't get
      --  that hieght, so kludge an offset for it. Also include a
      --  small offset so mouse double click works.
      return Window_Position (Table_View) +
        Widget_Position_Toplevel (Table_View.Private_Stuff.List_Display_Scroll (Table_View.Current_List)) +
        (5, 40);
   end First_Link;

end Books.Table_Views;
