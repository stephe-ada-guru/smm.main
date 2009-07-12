--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with Books.Table_Views.Author.Test;
with Books.Table_Views.Test;
with Books.Table_Views.Title.Test;
with GNAT.OS_Lib;
with Gdk.Test_Events;
package body Test_Books.GUI_Utils is

   procedure Empty_Database
   is
      Make_Args : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List ("--silent empty_database_test");
      Success : Boolean;
   begin
      GNAT.OS_Lib.Spawn ("make", Make_Args.all, Success);
      GNAT.OS_Lib.Free (Make_Args);

      AUnit.Assertions.Assert (Success, "make empty_database failed");

   end Empty_Database;

   procedure Create_Show_Main_Window
   is begin
      Books.Main_Window.Gtk_New (Main_Window, Ada.Strings.Unbounded.To_String (Config_File));
      Books.Main_Window.Show (Main_Window);
   end Create_Show_Main_Window;

   procedure Set_Up_Case
     (Config_File   : access String;
      Debug_Level   : in     Integer;
      Event_Handler : in     Gdk.Event.Event_Handler_Func := null)
   is begin
      Gdk.Test_Events.Debug_Level := Debug_Level;

      case Debug_Level is
      when 0 =>
         null;

      when 1 =>
         Gdk.Test_Events.Default_Delay := 1.0;

      when 2 =>
         --  Let user run window; mouse click locations are put to stdout.
         null;

      when others =>
         null;
      end case;

      GUI_Utils.Config_File := Ada.Strings.Unbounded.To_Unbounded_String (Config_File.all);

      Background.Background_Task.Create_Window;

      Background.Background_Task.Run (Event_Handler);
      --  Note that Books.Event_Handler is not used if Debug_Level >= 2.

   end Set_Up_Case;

   procedure Create_Some_Data
   is
      Make_Args : GNAT.OS_Lib.Argument_List_Access := GNAT.OS_Lib.Argument_String_To_List ("--silent test_data");
      Success : Boolean;
   begin
      GNAT.OS_Lib.Spawn ("make", Make_Args.all, Success);
      GNAT.OS_Lib.Free (Make_Args);

      AUnit.Assertions.Assert (Success, "make test_data failed");

   end Create_Some_Data;

   procedure Check_Clist (Label : in String; Expected : in Test_Books.String_Lists.String_Table_Access_Type)
   is begin
      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);

      Gdk.Test_Events.Alt_Key_Stroke ('t'); -- test

      Test_Books.String_Lists.Check (Label, Books.Table_Views.Test.Clist_Contents, Expected);
   end Check_Clist;

   procedure Add_Author
     (First  : in String;
      Middle : in String;
      Last   : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Author_View));
      Mouse_Click;
      Ctrl_Key_Stroke ('a');
      Key_Stroke (Last);
      Alt_Key_Stroke ('a', Key_Delay => 0.1); -- Add, let window get painted
      Shift_Tab;
      Key_Stroke (Middle);
      Shift_Tab;
      Key_Stroke (First);
      Alt_Key_Stroke ('i', Key_Delay => 0.1); -- Insert
   end Add_Author;

   procedure Find_Author (Last : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Author_View));
      Mouse_Click;
      Ctrl_Key_Stroke ('a'); -- select all current; beeps if empty
      Key_Stroke (Last);
   end Find_Author;

   procedure Check_Author (Label : in String; Expected : in Test_Books.String_Lists.String_List_Type)
   is
      use Gdk.Test_Events;
   begin
      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Author.Test.Dump_Author'Access);

      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Author_View));
      Mouse_Click;
      delay 0.1;
      Alt_Key_Stroke ('t'); -- test

      Test_Books.String_Lists.Check (Label, Books.Table_Views.Author.Test.Author_Contents, Expected);
   end Check_Author;

   procedure Find_Collection (Title : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Collection_View));
      Mouse_Click;
      Ctrl_Key_Stroke ('a'); -- select all current; beeps if empty
      Key_Stroke (Title);
   end Find_Collection;

   procedure Add_Title
     (Title   : in String;
      Year    : in String;
      Comment : in String;
      Rating  : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Title_View));
      Mouse_Click;
      Ctrl_Key_Stroke ('a'); -- select all current
      Key_Stroke (Title);
      Alt_Key_Stroke ('a', Key_Delay => 0.1); -- Add
      Key_Stroke (Tab); --  Year
      Key_Stroke (Year);
      Key_Stroke (Tab); --  Comment
      Key_Stroke (Comment);
      Key_Stroke (Tab); --  Rating
      Key_Stroke (Rating);
      Alt_Key_Stroke ('i', Key_Delay => 0.1); -- Insert
   end Add_Title;

   procedure Find_Title (Title : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Title_View));
      Mouse_Click;
      Ctrl_Key_Stroke ('a'); -- select all current
      Key_Stroke (Title);
   end Find_Title;

   procedure Check_Title (Label : in String; Expected : in Test_Books.String_Lists.String_List_Type)
   is
      use Gdk.Test_Events;
   begin
      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Title.Test.Dump_Title'Access);

      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Title_View));
      Mouse_Click;
      delay 0.1;
      Alt_Key_Stroke ('t'); -- test

      Test_Books.String_Lists.Check (Label, Books.Table_Views.Title.Test.Title_Contents, Expected);
   end Check_Title;

   procedure Check_Title_Full (Label : in String; Expected : in Test_Books.String_Lists.String_List_Type)
   is
      use Gdk.Test_Events;
   begin
      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Title.Test.Dump_Title_Full'Access);

      Mouse_Move (Books.Table_Views.Test.Find_Entry (Main_Window.Title_View));
      Mouse_Click;
      delay 0.1;
      Alt_Key_Stroke ('t'); -- test

      Test_Books.String_Lists.Check (Label, Books.Table_Views.Title.Test.Title_Contents_Full, Expected);
   end Check_Title_Full;

end Test_Books.GUI_Utils;
