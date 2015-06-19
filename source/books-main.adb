--  Abstract :
--
--  Main program for Books.
--
--  Copyright (C) 2002, 2004, 2009, 2012, 2015 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO;
with Books.Main_Window;
with Gtk.Main;
with GNAT.Traceback.Symbolic;
procedure Books.Main
is
   Main_Window : Books.Main_Window.Gtk_Window;

   Config_Filename : constant String :=
     (if Argument_Count = 0 then
        Ada.Environment_Variables.Value ("HOME") & "/.books/books.config"
     else
        Argument (1));
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;

   Books.Main_Window.Gtk_New (Main_Window, Config_Filename);

   --  Easier to fix bugs if get stack trace
   --  Gdk.Event.Event_Handler_Set (Books.Event_Handler.Event_Handler'Access, System.Storage_Elements.To_Address (0));

   Books.Main_Window.Show (Main_Window);
   Gtk.Main.Main;
exception
when E : others =>
   Ada.Text_IO.Put_Line
     ("Unhandled exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Books.Main;
