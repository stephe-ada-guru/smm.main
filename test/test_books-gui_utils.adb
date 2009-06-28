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
with Books.Event_Handler;
with GNAT.OS_Lib;
with Gtk.Widget.Config;
with SAL.Config_Files;
package body Test_Books.GUI_Utils is

   procedure Set_Window_Origins (Config_File : in Ada.Strings.Unbounded.Unbounded_String)
   is
      use SAL.Config_Files;
      Config   : Configuration_Type;
      Geometry : Gtk.Widget.Gtk_Allocation;
   begin
      Open (Config, Ada.Strings.Unbounded.To_String (Config_File));

      Geometry    := Gtk.Widget.Config.Read (Config, "Main.Geometry");
      Main_Origin := (Geometry.X, Geometry.Y);

      Geometry      := Gtk.Widget.Config.Read (Config, "Author.Geometry");
      Author_Origin := (Geometry.X, Geometry.Y);

      Geometry     := Gtk.Widget.Config.Read (Config, "Title.Geometry");
      Title_Origin := (Geometry.X, Geometry.Y);

      Close (Config);
   end Set_Window_Origins;

   procedure Empty_Database
   is
      Make_Args : GNAT.OS_Lib.Argument_List_Access := GNAT.OS_Lib.Argument_String_To_List ("empty_database_test");
      Success : Boolean;
   begin
      GNAT.OS_Lib.Spawn ("make", Make_Args.all, Success);
      GNAT.OS_Lib.Free (Make_Args);

      AUnit.Assertions.Assert (Success, "make empty_database failed");

   end Empty_Database;

   procedure Create_Main_Window (Window : out Gtk.Window.Gtk_Window)
   is begin
      Books.Main_Window.Gtk_New (Main_Window, Ada.Strings.Unbounded.To_String (Config_File));

      Window := Gtk.Window.Gtk_Window (Main_Window);

   end Create_Main_Window;

   procedure Set_Up_Case
     (Config_File : access String;
      Debug_Level : in     Integer)
   is begin
      Background.Debug_Level := Debug_Level;

      case Debug_Level is
      when 0 =>
         null;

      when 1 =>
         Background.Test_Delay_Time    := 1.0;
         Gdk.Test_Events.Default_Delay := Background.Test_Delay_Time;

      when 2 =>
         --  Let user run window; mouse click locations are put to stdout.
         null;

      when others =>
         null;
      end case;

      GUI_Utils.Config_File := Ada.Strings.Unbounded.To_Unbounded_String (Config_File.all);

      Set_Window_Origins (GUI_Utils.Config_File);

      Background.Background_Task.Create_Window (Gtk.Window.Gtk_Window (Main_Window));

      Background.Background_Task.Run (Books.Event_Handler.Event_Handler'Access);

   end Set_Up_Case;

   procedure Create_Some_Data
   is
      Make_Args : GNAT.OS_Lib.Argument_List_Access := GNAT.OS_Lib.Argument_String_To_List ("test_data");
      Success : Boolean;
   begin
      GNAT.OS_Lib.Spawn ("make", Make_Args.all, Success);
      GNAT.OS_Lib.Free (Make_Args);

      AUnit.Assertions.Assert (Success, "make test_data failed");

   end Create_Some_Data;

   procedure Add_Author
     (First  : in String;
      Middle : in String;
      Last   : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Author_Origin + Find_Entry);
      Mouse_Double_Click;
      Key_Stroke (Last);
      Alt_Key_Stroke ('a', Key_Delay => 0.1); -- Add, let window get painted
      Shift_Tab;
      Key_Stroke (Middle);
      Shift_Tab;
      Key_Stroke (First);
      Alt_Key_Stroke ('i', Key_Delay => 0.1); -- Insert
   end Add_Author;

   procedure Add_Title
     (Title   : in String;
      Year    : in String;
      Comment : in String;
      Rating  : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Title_Origin + Find_Entry);
      Mouse_Double_Click (Mouse_Delay => 0.1); -- let current text get selected
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

end Test_Books.GUI_Utils;
