--  Abstract :
--
--  See spec.
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

with Ada.Exceptions;
with Ada.Text_IO;
with Books.Database;
with Gtk.Main;
with Gtk.Message_Box;
package body Books.Event_Handler is

   procedure Event_Handler
     (Event : in Gdk.Event.Gdk_Event;
      Data : in System.Address)
   is
      pragma Unreferenced (Data);
      use Ada.Exceptions;
   begin
      Gtk.Main.Do_Event (Event);
   exception
   when E : Books.Database.Entry_Error =>
      Gtk.Message_Box.Information_Box
        (Title   => "Entry error",
         Message => Exception_Message (E));

   when E : others =>
      Unhandled_Exception := True;
      Save_Occurrence (Unhandled_Occurrence, E);
      begin
         Gtk.Message_Box.Information_Box
           (Title   => "Unhandled exception " & Exception_Name (E),
            Message => Exception_Message (E));
      exception
      when others =>
         --  Gtk is shutting down?
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "Unhandled exception " & Exception_Name (E) & ": " & Exception_Message (E));
      end;
   end Event_Handler;

end Books.Event_Handler;
