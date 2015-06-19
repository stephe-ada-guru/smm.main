--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2000, 2003, 2004, 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Gtk.Handlers;
package body Gtk.Widget.Signal is

   package Return_Boolean is new Gtk.Handlers.Return_Callback (Gtk_Widget_Record, Boolean);

   procedure Connect_Button_Press_Event
     (Widget  : access Gtk_Widget_Record'Class;
      Handler : in     Boolean_Event_Handler)
   is begin
      Return_Boolean.Connect
        (Widget,
         "button_press_event",
         Return_Boolean.Event_Marshaller.To_Marshaller
           (Return_Boolean.Event_Marshaller.Handler (Handler)));
   end Connect_Button_Press_Event;

   procedure Connect_Configure_Event
     (Widget  : access Gtk_Widget_Record'Class;
      Handler : in     Boolean_Event_Handler)
   is begin
      Return_Boolean.Connect
        (Widget,
         "configure_event",
         Return_Boolean.Event_Marshaller.To_Marshaller
           (Return_Boolean.Event_Marshaller.Handler (Handler)));
   end Connect_Configure_Event;

   procedure Connect_Delete_Event
     (Widget  : access Gtk_Widget_Record'Class;
      Handler : in     Boolean_Event_Handler)
   is begin
      Return_Boolean.Connect
        (Widget,
         "delete_event",
         Return_Boolean.Event_Marshaller.To_Marshaller
           (Return_Boolean.Event_Marshaller.Handler (Handler)));
   end Connect_Delete_Event;

   procedure Connect_Focus_In_Event
     (Widget  : access Gtk_Widget_Record'Class;
      Handler : in     Boolean_Event_Handler)
   is begin
      Return_Boolean.Connect
        (Widget,
         "focus_in_event",
         Return_Boolean.Event_Marshaller.To_Marshaller
           (Return_Boolean.Event_Marshaller.Handler (Handler)));
   end Connect_Focus_In_Event;

   procedure Connect_Focus_Out_Event
     (Widget  : access Gtk_Widget_Record'Class;
      Handler : in     Boolean_Event_Handler)
   is begin
      Return_Boolean.Connect
        (Widget,
         "focus_out_event",
         Return_Boolean.Event_Marshaller.To_Marshaller
           (Return_Boolean.Event_Marshaller.Handler (Handler)));
   end Connect_Focus_Out_Event;

   procedure Connect_Window_State_Event
     (Widget  : access Gtk_Widget_Record'Class;
      Handler : in     Boolean_Event_Handler)
   is begin
      Return_Boolean.Connect
        (Widget,
         "window-state-event",
         Return_Boolean.Event_Marshaller.To_Marshaller
           (Return_Boolean.Event_Marshaller.Handler (Handler)));
   end Connect_Window_State_Event;

end Gtk.Widget.Signal;

