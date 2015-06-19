--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
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
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with Gdk.Drawable;
with Gtk.Widget.Config;
package body Gtk.Window.Config is

   procedure Set_Geometry
     (Window  : access Gtk.Window.Gtk_Window_Record'Class;
      Config  : in     SAL.Config_Files.Configuration_Type;
      Key     : in     String;
      Default : in     Gtk.Widget.Gtk_Allocation)
   is
      Geometry : constant Gtk.Widget.Gtk_Allocation := Gtk.Widget.Config.Read (Config, Key & ".Geometry", Default);
   begin
      Set_UPosition (Window, X => Geometry.X, Y => Geometry.Y);
      Set_USize (Window, Width => Geometry.Width, Height => Geometry.Height);
   end Set_Geometry;

   procedure Save_Geometry
     (Window : access Gtk.Window.Gtk_Window_Record'Class;
      Config : in out SAL.Config_Files.Configuration_Type;
      Key    : in     String)
   is
      Geometry : Gtk.Widget.Gtk_Allocation;
   begin
      Gdk.Window.Get_Root_Origin (Get_Window (Window), Geometry.X, Geometry.Y);
      Gdk.Drawable.Get_Size (Get_Window (Window), Geometry.Width, Geometry.Height);
      Gtk.Widget.Config.Write (Config, Key & ".Geometry", Geometry);
   end Save_Geometry;

end Gtk.Window.Config;
