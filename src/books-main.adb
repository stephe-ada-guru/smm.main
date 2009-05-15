-- Abstract :
--
-- Main program for Books.
--
-- Copyright (C) 2002, 2004 Stephen Leake.  All Rights Reserved.
--
-- This program is free software; you can redistribute it and/or
-- modify it under terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2, or (at
-- your option) any later version. This program is distributed in the
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even
-- the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE. See the GNU General Public License for more details. You
-- should have received a copy of the GNU General Public License
-- distributed with this program; see file COPYING. If not, write to
-- the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
-- MA 02111-1307, USA.
--

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Books.Event_Handler;
with Books.Main_Window;
with GNAT.Traceback.Symbolic;
with Gdk.Event;
with Gtk.Main;
with SAL;
with System.Storage_Elements;
procedure Books.Main
is
   Main_Window : Books.Main_Window.Gtk_Window;

begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   if Ada.Command_Line.Argument_Count = 0 then
      Books.Main_Window.Gtk_New (Main_Window);
   else
      Books.Main_Window.Gtk_New
        (Main_Window,
         Config_File  => Ada.Command_Line.Argument (1));
   end if;

   Gdk.Event.Event_Handler_Set (Books.Event_Handler.Event_Handler'Access, System.Storage_Elements.To_Address (0));

   Books.Main_Window.Show (Main_Window);
   Gtk.Main.Main;
exception
when E : SAL.Config_File_Error =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));

when E : others =>
   Ada.Text_IO.Put_Line
     ("Unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Books.Main;
