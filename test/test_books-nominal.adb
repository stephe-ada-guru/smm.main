--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with Books.Event_Handler;
with Books.Main_Window;
with Books.Table_Views.Test;
with Gdk.Test_Events;
with Gtk.Gen_Background_Window;
with Gtk.Window;
with Test_Books.GUI_Utils;
with Test_Books.String_Lists;
package body Test_Books.Nominal is

   Main_Window : Books.Main_Window.Gtk_Window;
   Config_File : Ada.Strings.Unbounded.Unbounded_String;

   procedure Create_Main_Window (Window : out Gtk.Window.Gtk_Window)
   is begin
      Books.Main_Window.Gtk_New (Main_Window, Ada.Strings.Unbounded.To_String (Config_File));

      Window := Gtk.Window.Gtk_Window (Main_Window);

   end Create_Main_Window;

   package Background is new Gtk.Gen_Background_Window (Create_Main_Window);

   ----------
   --  Test procedures

   procedure Add_Author (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      GUI_Utils.Add_Author
        (Last   => "Clarke",
         Middle => "C.",
         First  => "Arthur");
   end Add_Author;

   procedure Add_Title (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
      use Test_Books.String_Lists;
      use Test_Books.GUI_Utils;
   begin
      Add_Title
        (Title   => "2001",
         Year    => "1970",
         Comment => "Obelisk",
         Rating  => "9");

      Mouse_Move (Title_Origin + Title_Add_Link_Button + (30, 0)); --  add link entry
      Mouse_Click;
      Key_Stroke ("1"); --  Clarke
      Alt_Key_Stroke ('k'); -- add link

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check_List (Books.Table_Views.Test.Clist_Contents, +(+"00001", +"Arthur", +"C.", +"Clarke"));
   end Add_Title;

   procedure Search_Author (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
      use Test_Books.String_Lists;
      use Test_Books.GUI_Utils;
   begin
      Mouse_Move (Author_Origin + Find_Entry);
      Mouse_Double_Click;
      Key_Stroke ("clarke");

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check_List (Books.Table_Views.Test.Clist_Contents, +(+"00001", +"2001", +"1970"));
   end Search_Author;

   procedure Search_Title (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
      use Test_Books.String_Lists;
      use Test_Books.GUI_Utils;
   begin
      Mouse_Move (Title_Origin + Find_Entry);
      Mouse_Double_Click;
      Key_Stroke ("2001");

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check_List (Books.Table_Views.Test.Clist_Contents, +(+"00001", +"Arthur", +"C.", +"Clarke"));
   end Search_Title;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Nominal");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug_Level < 2 then
         Register_Routine (T, Add_Author'Access, "Add_Author");
         Register_Routine (T, Add_Title'Access, "Add_Title");
         Register_Routine (T, Search_Author'Access, "Search_Author");
         Register_Routine (T, Search_Title'Access, "Search_Title");
      end if;
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      use Ada.Strings.Unbounded;
   begin
      GUI_Utils.Empty_Database;

      Background.Debug_Level := T.Debug_Level;

      case T.Debug_Level is
      when 0 =>
         null;

      when 1 =>
         Background.Test_Delay_Time    := 1.0;
         Gdk.Test_Events.Default_Delay := Background.Test_Delay_Time;

      when 2 =>
         --  Let user run window; it shows mouse click locations.
         null;

      when others =>
         null;
      end case;

      Config_File := To_Unbounded_String (T.Config_File.all);

      GUI_Utils.Set_Window_Origins (Config_File);

      Background.Background_Task.Init;
      Background.Background_Task.Create_Window (Gtk.Window.Gtk_Window (Main_Window));

      Background.Background_Task.Run (Books.Event_Handler.Event_Handler'Access);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      if Background.Debug_Level < 2 then
         Background.Close;
      end if;
      Background.Background_Task.Wait_Shutdown;
   end Tear_Down_Case;

end Test_Books.Nominal;
