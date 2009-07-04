--  Abstract :
--
--  See spec
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
--

with AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Ada.Exceptions;
with Ada.Text_IO;
with Books.Event_Handler;
with GNAT.Traceback.Symbolic;
with Gdk.Test_Events;
with SAL.AUnit;
with Test_Books.GUI_Utils;
package body Test_Books.Errors is

   procedure Reset_Exception
   is begin
      Books.Event_Handler.Handled_Exception   := False;
      Books.Event_Handler.Unhandled_Exception := False;
   end Reset_Exception;

   procedure Check_Exception (Label : in String)
   is begin
      if Books.Event_Handler.Unhandled_Exception then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Label & " unhandled exception: " &
              Ada.Exceptions.Exception_Name (Books.Event_Handler.Unhandled_Occurrence) & " " &
              Ada.Exceptions.Exception_Message (Books.Event_Handler.Unhandled_Occurrence));

         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Books.Event_Handler.Unhandled_Occurrence));

         AUnit.Assertions.Assert (False, "unhandled exception");
      end if;

      if not Books.Event_Handler.Handled_Exception then
         AUnit.Assertions.Assert (False, Label & ": no handled exception");
      end if;
   end Check_Exception;

   procedure Check_No_Exception (Label : in String)
   is
      use SAL.AUnit;
   begin
      Check (Label & "unhandled exception", Books.Event_Handler.Unhandled_Exception, False);
      Check (Label & "handled exception", Books.Event_Handler.Handled_Exception, False);
   end Check_No_Exception;

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

      Alt_Key_Stroke ('c'); -- cancel

      Check_Exception ("Duplicate_Author");

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

      Key_Stroke (Enter, Key_Delay => 0.1); --  Acknowledge exception message, let focus shift back

      Alt_Key_Stroke ('c'); -- cancel

      Check_Exception ("Duplicate_Title");

   end Duplicate_Title;

   procedure Long_Title (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
   begin
      GUI_Utils.Add_Title
        (Title   => "A really, really, really, really, long, long, long, title",
         Year    => "1970",
         Comment => "",
         Rating  => "");

      --  Since there's no way to fix this, there is no exception message; the title is silently truncated.
      Check_No_Exception ("Long_Title");

   end Long_Title;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Books.Errors");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Duplicate_Author'Access, "Duplicate_Author");
      Register_Routine (T, Duplicate_Title'Access, "Duplicate_Title");
      Register_Routine (T, Long_Title'Access, "Long_Title");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      use Ada.Strings.Unbounded;
   begin
      GUI_Utils.Empty_Database;

      GUI_Utils.Set_Up_Case (T.Config_File, T.Debug_Level);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      if GUI_Utils.Background.Debug_Level < 2 then
         GUI_Utils.Background.Close (GUI_Utils.Main_Window);
      end if;

      GUI_Utils.Background.Background_Task.Wait_Shutdown;
   end Tear_Down_Case;

   overriding procedure Set_up (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Reset_Exception;
   end Set_up;

end Test_Books.Errors;
