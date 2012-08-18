--  Abstract :
--
--  Base database table view widget for Books application.
--
--  Copyright (C) 2002, 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Books.Database.Data_Tables;
with Books.Database.Link_Tables;
with Books.List_Views;
with Gdk.Test_Events;
with Gtk.Check_Button;
with Gtk.Clist;
with Gtk.GEntry;
with Gtk.Radio_Button;
with Gtk.Table;
with Gtk.Window;
with SAL.Config_Files;
package Books.Table_Views is

   type Gtk_Table_View_Record is abstract new Gtk.Window.Gtk_Window_Record with private;
   type Gtk_Table_View is access all Gtk_Table_View_Record'Class;

   type Table_Array_Table_View_Type is array (Table_Names) of Gtk_Table_View;

   type Create_Parameters_Type is record
      --  used by child Gtk_New procedures
      DB       : Books.Database.Database_Access;
      Siblings : Table_Arrays;
      Links    : Link_Arrays;
      Config   : SAL.Config_Files.Configuration_Access_Type;
   end record;

   ----------
   --  New class-wide operations.

   procedure Create_GUI
     (Table_View : access Gtk_Table_View_Record'Class;
      Config     : in     SAL.Config_Files.Configuration_Access_Type);
   --  Create common GUI components. Child type must set data table
   --  components, and hide uneeded links, list selectors, lists.

   procedure Set_Display (Table_View : access Gtk_Table_View_Record'Class; ID : in Books.Database.ID_Type);
   --  Display ID

   procedure Set_Visibility (Table_View : access Gtk_Table_View_Record'class);
   --  Set visibility of all children, assuming Show_All has been done
   --  (for some brain-dead reason).

   procedure Set_Sibling_Views
     (Table_View    : access Gtk_Table_View_Record'Class;
      Sibling_Views : in     Table_Array_Table_View_Type);

   function ID (Table_View : access Gtk_Table_View_Record'Class) return Books.Database.ID_Type;
   --  Return ID of main record in view.

   function ID_Image (Table_View : access Gtk_Table_View_Record'Class) return String;

   ----------
   --  New dispatching operations.

   procedure Default_Add (Table_View : access Gtk_Table_View_Record) is abstract;
   --  Set default contents of Add display, set focus.

   function Link_Table
     (Table_View : access constant Gtk_Table_View_Record;
      Kind       : in Table_Names)
     return Books.Database.Link_Tables.Table_Access;
   --  Dispatching to allow Object.Method syntax

   function Main_Index_Name (Table_View : access Gtk_Table_View_Record) return String is abstract;
   --  Return the name of the main index (for index select drop box).

   procedure Update_Database (Table_View : access Gtk_Table_View_Record) is abstract;
   --  Update current database record with values from display.

   procedure Insert_Database (Table_View : access Gtk_Table_View_Record) is abstract;
   --  Insert a new database record with values from display.

   procedure Create_List_View (Table_View : access Gtk_Table_View_Record; List : in Table_Names) is abstract;
   procedure Update_Primary_Display (Table_View : access Gtk_Table_View_Record) is abstract;
   procedure Clear_Primary_Display (Table_View : access Gtk_Table_View_Record) is abstract;

   --  For unit tests
   type Test_Hook_Type is access procedure (Table_View : in Gtk_Table_View);

private

   type Private_Stuff_Record;
   type Private_Stuff_Access is access Private_Stuff_Record;

   type Table_Array_Radio_Type is array (Table_Names) of Gtk.Radio_Button.Gtk_Radio_Button;
   type Table_Array_List_Type  is array (Table_Names) of Books.List_Views.Gtk_List_View;
   type Table_Array_Check_Button_Type is array (Table_Names) of Gtk.Check_Button.Gtk_Check_Button;

   type Gtk_Table_View_Record is abstract new Gtk.Window.Gtk_Window_Record with record
      Private_Stuff : Private_Stuff_Access;
      Find_Text     : Gtk.GEntry.Gtk_Entry; --  Child needs text from this
      Links_Buttons : Table_Array_Check_Button_Type;
      Data_Table    : Gtk.Table.Gtk_Table;  --  Contents unique to child type.

      List_Select  : Table_Array_Radio_Type; --  Children need to set and check these.
      List_Display : Table_Array_List_Type;
      Current_List : Table_Names := Table_Names'First;

      Sibling_Views : Table_Array_Table_View_Type;

      --  Non-GUI stuff

      --  All table views share the same tables, for cross-referencing
      Primary_Kind  : Table_Names;
      Primary_Table : Books.Database.Data_Tables.Table_Access;
      Siblings      : Table_Arrays;
      Links         : Link_Arrays;
   end record;

   --  For children to call
   procedure To_Main (Table_View : access Gtk_Table_View_Record'class);
   --  Show controls for main display, hide edit, add.

   --  For unit tests
   Test_Hook : Test_Hook_Type;

   function Find_Entry (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type;
   function Add_Link_Button (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type;
   function First_Link (Table_View : access Gtk_Table_View_Record'Class) return Gdk.Test_Events.Point_Type;
   --  Return absolute coordinates of left top of GUI item.

end Books.Table_Views;
