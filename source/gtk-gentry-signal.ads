--  Abstract :
--
--  Safe Connect subprograms for Gtk.GEntry signals.
--
--  Copyright (C) 2003, 2004, 2009 Stephen Leake.  All Rights Reserved.
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
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);
with Gdk.Event;
package Gtk.GEntry.Signal is

   type Event_Handler is access procedure (GEntry : access Gtk_Entry_Record'Class);

   type Boolean_Event_Handler is access function
     (Widget : access Gtk_Entry_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean;

   procedure Connect_Activate
     (GEntry  : access Gtk_Entry_Record'Class;
      Handler : in     Event_Handler);

   procedure Connect_Changed
     (GEntry  : access Gtk_Entry_Record'Class;
      Handler : in     Event_Handler);

   procedure Connect_Focus_In_Event
     (GEntry  : access Gtk_Entry_Record'Class;
      Handler : in     Boolean_Event_Handler);

   procedure Connect_Focus_Out_Event
     (GEntry  : access Gtk_Entry_Record'Class;
      Handler : in     Boolean_Event_Handler);

end Gtk.GEntry.Signal;
