--  Abstract :
--
--  see spec
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Gtk.Handlers;
package body Gtk.Object.Signal is

   package Plain is new Gtk.Handlers.Callback (Gtk_Object_Record);
   --  No user data, no return value.

   procedure Connect_Destroy
     (Object  : access Gtk_Object_Record'Class;
      Handler : in     Event_Handler)
   is begin
      Plain.Connect
        (Object,
         "destroy",
         Plain.Marshallers.Void_Marshaller.To_Marshaller
           (Plain.Marshallers.Void_Marshaller.Handler (Handler)));
   end Connect_Destroy;

end Gtk.Object.Signal;
