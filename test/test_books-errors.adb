--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Books.Event_Handler;
with Books.Main_Window;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with Gdk.Test_Events;
with Gdk.Test_Events;
with Gtk.Gen_Background_Window;
with Gtk.Window;
with Test_Books.GUI_Utils;
package body Test_Books.Errors is

   Main_Window : Books.Main_Window.Gtk_Window;
   Config_File : Ada.Strings.Unbounded.Unbounded_String;

   procedure Create_Main_Window (Window : out Gtk.Window.Gtk_Window)
   is begin
      Books.Main_Window.Gtk_New (Main_Window, Ada.Strings.Unbounded.To_String (Config_File));

      Window := Gtk.Window.Gtk_Window (Main_Window);

   end Create_Main_Window;

   package Background is new Gtk.Gen_Background_Window (Create_Main_Window);

   procedure Check_Exception (Label : in String)
   is
      use Ada.Exceptions;
      use Books.Event_Handler;
   begin
      if Unhandled_Exception then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Label & " unhandled exception: " &
              Exception_Name (Unhandled_Occurrence) & " " &
              Exception_Message (Unhandled_Occurrence));

         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Unhandled_Occurrence));

         AUnit.Assertions.Assert (False, "unhandled exception");
      end if;
   end Check_Exception;

   ----------
   --  Test procedures

   procedure Duplicate_Author (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
   begin
      GUI_Utils.Add_Author
        (Last   => "Clarke",
         Middle => "C.",
         First  => "Arthur");

      GUI_Utils.Add_Author
        (Last   => "Clarke",
         Middle => "C.",
         First  => "Arthur");

      Key_Stroke (Enter); --  Acknowledge exception message

      Check_Exception ("Duplicate_Author");

      Alt_Key_Stroke ('c'); -- cancel

   end Duplicate_Author;

   procedure Duplicate_Title (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
   begin
      GUI_Utils.Add_Title
        (Title   => "2001",
         Year    => "1970",
         Comment => "",
         Rating  => "");

      GUI_Utils.Add_Title
        (Title   => "2001",
         Year    => "1970",
         Comment => "",
         Rating  => "");

      Key_Stroke (Enter); --  Acknowledge exception message

      Check_Exception ("Duplicate_Title");

      Alt_Key_Stroke ('c'); -- cancel

   end Duplicate_Title;

   procedure Long_Title (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
   begin
      GUI_Utils.Add_Title
        (Title   => "A really, really, really, really, long, long, long, t",
         Year    => "1970",
         Comment => "",
         Rating  => "");

      Key_Stroke (Enter); --  Acknowledge exception message

      Check_Exception ("Long_Title");

      Alt_Key_Stroke ('c'); -- cancel

   end Long_Title;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Errors");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Duplicate_Author'Access, "Duplicate_Author");
      Register_Routine (T, Duplicate_Title'Access, "Duplicate_Title");
      Register_Routine (T, Long_Title'Access, "Long_Title");
   end Register_Tests;

   procedure Set_Up_Case (T : in out Test_Case)
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

      Config_File := To_Unbounded_String (T.Root_Directory.all & "/test.config");

      GUI_Utils.Set_Window_Origins (Config_File);

      Background.Background_Task.Init;
      Background.Background_Task.Create_Window (Gtk.Window.Gtk_Window (Main_Window));

      Background.Background_Task.Run (Books.Event_Handler.Event_Handler'Access);
   end Set_Up_Case;

   procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      if Background.Debug_Level < 2 then
         Background.Close;
      end if;

      Background.Background_Task.Wait_Shutdown;
   end Tear_Down_Case;

end Test_Books.Errors;
