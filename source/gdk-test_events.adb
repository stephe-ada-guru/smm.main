--  Abstract :
--
--  See spec
--
--  The body of this package is operating system and window system dependent.
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

with Ada.Command_Line;
with Ada.Text_IO;
with Gdk.Window;
package body Gdk.Test_Events is

   Small_Delay : constant Duration := 0.01;
   --  short enough for the second click to count as a double click

   procedure Process_Command_Line_Args (Next_Arg : in out Natural)
   is
      use Ada.Command_Line;
      function Remaining_Args return Natural
      is begin
         return Argument_Count - Next_Arg + 1;
      end Remaining_Args;
   begin
      if Remaining_Args = 0 then
         --  Use default Debug_Level
         null;
      else
         Debug_Level := Natural'Value (Argument (Next_Arg));
         Next_Arg    := Next_Arg + 1;
      end if;

      if Remaining_Args = 0 then
         --  Use default Default_Delay
         null;
      else
         Default_Delay := Duration'Value (Argument (Next_Arg));
         Next_Arg      := Next_Arg + 1;
      end if;
   end Process_Command_Line_Args;

   procedure Test_Delay
   is begin
      delay Default_Delay;
   end Test_Delay;

   procedure Debug_Put (Level : in Integer; Message : in String)
   is begin
      if Debug_Level >= Level then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Debug_Put;

   procedure Key_Event_Special
     (Key    : in Key_Type;
      Key_Up : in Boolean)
      is separate;
   --  Generate key event, followed by Small_Delay. If multiple
   --  events are required, use Small_Delay as delay between them.

   procedure Key_Stroke_ASCII (Key : in Character) is separate;
   --  Generate key event, followed by Small_Delay. If multiple
   --  events are required, use Small_Delay as delay between them.

   procedure Key_Event
     (Key       : in Key_Type;
      Key_Up    : in Boolean;
      Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event_Special (Key, Key_Up);
      delay Key_Delay;
   end Key_Event;

   procedure Key_Stroke
     (Key       : in Key_Type;
      Key_Delay : in Duration := Default_Delay)
   is begin
      begin
         Key_Event_Special (Key, Key_Up => False);
         delay Small_Delay;
      exception
      when others =>
         --  Action taken on key event had a problem; ensure we finish
         --  the key stroke, but still propagate exception so test
         --  reports it.
         Key_Event_Special (Key, Key_Up => True);
         raise;
      end;
      Key_Event_Special (Key, Key_Up => True);
      delay Key_Delay;
   end Key_Stroke;

   procedure Alt_Key_Stroke
     (Key : in Character;
      Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event_Special (Alt, Key_Up => False);
      Key_Stroke_ASCII (Key);
      delay Key_Delay;
      Key_Event_Special (Alt, Key_Up => True);
   end Alt_Key_Stroke;

   procedure Ctrl_Key_Stroke
     (Key : in Character;
      Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event_Special (Ctrl, Key_Up => False);
      Key_Stroke_ASCII (Key);
      delay Key_Delay;
      Key_Event_Special (Ctrl, Key_Up => True);
   end Ctrl_Key_Stroke;

   procedure Key_Stroke
     (Keys      : in String;
      Key_Delay : in Duration := Default_Delay)
   is begin
      for I in Keys'Range loop
         if Keys (I) in ' ' .. '~' then
            Key_Stroke_ASCII (Keys (I));
            delay Small_Delay;
         end if;
      end loop;
      delay Key_Delay;
   end Key_Stroke;

   procedure Shift_Tab (Key_Delay : in Duration := Default_Delay)
   is begin
      Key_Event_Special (Shift, Key_Up => False);
      delay Small_Delay;
      Key_Event_Special (Tab, Key_Up => False);
      delay Small_Delay;
      Key_Event_Special (Tab, Key_Up => True);
      delay Small_Delay;
      Key_Event_Special (Shift, Key_Up => True);
      delay Key_Delay;
   end Shift_Tab;

   -----------
   --  Mouse events

   procedure Mouse_Move_Event (Point : in Point_Type) is separate;
   --  Move mouse to absolute screen position Point.

   procedure Mouse_Button_Event
     (Button    : in Glib.Gint;
      Button_Up : in Boolean)
     is separate;

   function "+" (Left, Right : in Point_Type) return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : in Point_Type) return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   function "*"
     (Left : in Point_Type;
      Right : in Glib.Gint)
      return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X * Right, Left.Y * Right);
   end "*";

   function "*"
     (Left : in Glib.Gint;
      Right : in Point_Type)
      return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left * Right.X, Left * Right.Y);
   end "*";

   function "/"
     (Left : in Point_Type;
      Right : in Glib.Gint)
      return Point_Type
   is
      use type Glib.Gint;
   begin
      return (Left.X / Right, Left.Y / Right);
   end "/";

   function Image (Item : in Point_Type) return String is
   begin
      return "(" & Glib.Gint'Image (Item.X) &
         ", " & Glib.Gint'Image (Item.Y) & ")";
   end Image;

   procedure Mouse_Button
     (Button    : in Glib.Gint;
      Button_Up : in Boolean)
     renames Mouse_Button_Event;

   procedure Mouse_Move (Point : in Point_Type) renames Mouse_Move_Event;

   function Window_Frame_Position (Window : access Gtk.Window.Gtk_Window_Record'Class) return Point_Type
   is
      Origin : Point_Type;
   begin
      Gdk.Window.Get_Root_Origin
        (Gtk.Window.Get_Window (Window),
         X       => Glib.Gint (Origin.X),
         Y       => Glib.Gint (Origin.Y));

      return Origin;
   end Window_Frame_Position;

   function Window_Position (Window : access Gtk.Window.Gtk_Window_Record'Class) return Point_Type
   is
      Origin  : Point_Type;
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Gdk.Window.Get_Origin
        (Gtk.Window.Get_Window (Window),
         X       => Glib.Gint (Origin.X),
         Y       => Glib.Gint (Origin.Y),
         Success => Success);

      return Origin;
   end Window_Position;

   function Widget_Position_Toplevel (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Point_Type
   is
      use Gtk.Widget;
      Toplevel : constant Gtk_Widget := Get_Toplevel (Widget);
      Parent   : Gtk_Widget          := Get_Parent (Widget);
      Result   : Point_Type          := (Gtk.Widget.Get_Allocation_X (Widget), Gtk.Widget.Get_Allocation_Y (Widget));
      Relative : Point_Type;
   begin
      --  Get_Allocation is just wrong for Tables; it returns
      --  seemingly reasonable but just wrong results.
      --
      --  For the contents of HBoxes, it returns the allocation
      --  relative to the parent of the HBox.
      --
      --  The only alternative is Get_Position (gdk_window). But the
      --  gdk window of _every_ widget is the single top level window,
      --  so that doesn't give us relative positions!
      --
      --  So we use Get_Allocation, and let the caller handle tables
      --  specially.
      loop
         exit when Parent = Toplevel;
         Relative := (Gtk.Widget.Get_Allocation_X (Parent), Gtk.Widget.Get_Allocation_Y (Parent));
         Result := Result + Relative;
         Parent := Get_Parent (Parent);
      end loop;
      return Result;
   end Widget_Position_Toplevel;

   function Widget_Position_Parent (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Point_Type
   is begin
      return (Gtk.Widget.Get_Allocation_X (Widget), Gtk.Widget.Get_Allocation_Y (Widget));
   end Widget_Position_Parent;

   procedure Mouse_Click
     (Button      : in Glib.Gint := 1;
      Mouse_Delay : in Duration  := Default_Delay)
   is begin
      Mouse_Button_Event (Button, Button_Up => False);
      delay Mouse_Delay;
      Mouse_Button_Event (Button, Button_Up => True);
      delay Mouse_Delay;
   end Mouse_Click;

   procedure Mouse_Double_Click
     (Button      : in Glib.Gint := 1;
      Mouse_Delay : in Duration  := Default_Delay)
   is begin
      Mouse_Click (Button, Small_Delay);
      Mouse_Click (Button, Mouse_Delay);
   end Mouse_Double_Click;

   procedure Close (Window : access Gtk.Window.Gtk_Window_Record'Class)
   is
      use type Glib.Gint;
      Width : constant Glib.Gint := Glib.Gint (Gtk.Window.Get_Allocation_Width (Window));
   begin
      Debug_Put (1, "Test_Driver: Close");

      --  Click on close button (x in upper right).
      Mouse_Move (Window_Frame_Position (Window) + (Width - 10, 10));

      Mouse_Click;

   end Close;

end Gdk.Test_Events;
