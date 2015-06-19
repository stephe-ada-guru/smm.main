--  Abstract :
--
--  see spec
--
--  Copyright (C) 2000, 2003, 2009 Stephen Leake.  All Rights Reserved.
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

with Gtk.Box;
with Gtk.Button.Signal;
with Gtk.Dialog.Signal;
with Gtk.Enums;
with Gdk.Event;
with Gtk.Label;
with Gtk.Main;
with Gtk.Widget;
package body Gtk.Message_Box is

   package Ok_Cancel is

      type Gtk_Message_Box_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
         Message       : Gtk.Label.Gtk_Label;
         Ok_Button     : Gtk.Button.Gtk_Button;
         Cancel_Button : Gtk.Button.Gtk_Button;
         Result        : Result_Type;
      end record;

      type Gtk_Message_Box is access all Gtk_Message_Box_Record'Class;

      function On_Delete_Event
        (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
         Event  : in     Gdk.Event.Gdk_Event)
        return Boolean;
      procedure On_Ok_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class);
      procedure On_Cancel_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class);

   end Ok_Cancel;

   package body Ok_Cancel is

      function On_Delete_Event
        (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
         Event  : in     Gdk.Event.Gdk_Event)
        return Boolean
      is
         pragma Unreferenced (Event);
      begin
         Gtk_Message_Box (Object).Result := Ok;
         Gtk.Main.Main_Quit;
         return False; --  Ok to quit
      end On_Delete_Event;

      procedure On_Ok_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class)
      is
         Message_Box : constant Gtk_Message_Box := Gtk_Message_Box (Gtk.Button.Get_Toplevel (Object));
      begin
         Message_Box.Result := Ok;
         Gtk.Main.Main_Quit;
      end On_Ok_Clicked;

      procedure On_Cancel_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class)
      is
         Message_Box : constant Gtk_Message_Box := Gtk_Message_Box (Gtk.Button.Get_Toplevel (Object));
      begin
         Message_Box.Result := Cancel;
         Gtk.Main.Main_Quit;
      end On_Cancel_Clicked;

   end Ok_Cancel;

   function Ok_Cancel_Box
     (Title   : in String;
      Message : in String)
      return Result_Type
   is
      use Ok_Cancel;
      Message_Box : Gtk_Message_Box;
      Result      : Result_Type := Ok;
      Vbox        : Gtk.Box.Gtk_Box;
      Hbox        : Gtk.Box.Gtk_Box;
   begin
      Message_Box := new Gtk_Message_Box_Record;

      Gtk.Dialog.Initialize (Message_Box);
      Set_Title (Message_Box, Title);
      Set_Policy (Message_Box, True, True, False);
      Set_Position (Message_Box, Gtk.Enums.Win_Pos_Center);
      Set_Modal (Message_Box, True);

      Vbox := Get_Vbox (Message_Box);
      Gtk.Box.Set_Homogeneous (Vbox, False);

      Gtk.Label.Gtk_New (Message_Box.Message, Message);
      Gtk.Label.Show (Message_Box.Message);
      Gtk.Box.Pack_Start (Vbox, Message_Box.Message, Expand => False);

      Gtk.Box.Gtk_New_Hbox (Hbox);
      Gtk.Box.Show (Hbox);
      Gtk.Box.Pack_Start (Vbox, Hbox);

      Gtk.Button.Gtk_New (Message_Box.Ok_Button, "Ok");
      Gtk.Button.Set_Flags (Message_Box.Ok_Button, Gtk.Widget.Can_Default);
      Gtk.Button.Show (Message_Box.Ok_Button);
      Gtk.Box.Pack_Start (Hbox, Message_Box.Ok_Button, Expand => False);

      Gtk.Button.Grab_Default (Message_Box.Ok_Button); --  Must be in a GtkWindow to grab default
      Gtk.Button.Signal.Connect_Clicked (Message_Box.Ok_Button, On_Ok_Clicked'Access);

      Gtk.Button.Gtk_New (Message_Box.Cancel_Button, "Cancel");
      Gtk.Button.Show (Message_Box.Cancel_Button);
      Gtk.Box.Pack_End (Hbox, Message_Box.Cancel_Button, Expand => False);
      Gtk.Button.Signal.Connect_Clicked (Message_Box.Cancel_Button, On_Cancel_Clicked'Access);

      Gtk.Dialog.Signal.Connect_Delete_Event (Message_Box, On_Delete_Event'Access);

      Show (Message_Box);
      Gtk.Main.Main; --  Exits when a button is clicked
      Result := Message_Box.Result;
      Destroy (Message_Box);
      return Result;
   end Ok_Cancel_Box;

   -----------

   package Information is

      type Gtk_Message_Box_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
         Message   : Gtk.Label.Gtk_Label;
         Ok_Button : Gtk.Button.Gtk_Button;
      end record;

      type Gtk_Message_Box is access all Gtk_Message_Box_Record'Class;

      function On_Delete
        (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
         Event  : in     Gdk.Event.Gdk_Event)
        return Boolean;

      procedure On_Ok_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class);

   end Information;

   package body Information is

      function On_Delete
        (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
         Event  : in     Gdk.Event.Gdk_Event)
        return Boolean
      is
         pragma Unreferenced (Object);
         pragma Unreferenced (Event);
      begin
         Gtk.Main.Main_Quit;
         return False; --  Ok to quit
      end On_Delete;

      procedure On_Ok_Clicked (Object : access Gtk.Button.Gtk_Button_Record'Class)
      is
         pragma Unreferenced (Object);
      begin
         Gtk.Main.Main_Quit;
      end On_Ok_Clicked;

   end Information;

   procedure Information_Box
     (Title   : in String;
      Message : in String)
   is
      use Information;
      Message_Box : Gtk_Message_Box;
      Vbox        : Gtk.Box.Gtk_Box;
   begin
      Message_Box := new Gtk_Message_Box_Record;

      Gtk.Dialog.Initialize (Message_Box);
      Set_Title (Message_Box, Title);
      Set_Policy (Message_Box, True, True, False);
      Set_Position (Message_Box, Gtk.Enums.Win_Pos_Center);
      Set_Modal (Message_Box, True);

      Vbox := Get_Vbox (Message_Box);
      Gtk.Box.Set_Homogeneous (Vbox, False);

      Gtk.Label.Gtk_New (Message_Box.Message, Message);
      Gtk.Label.Show (Message_Box.Message);
      Gtk.Box.Pack_Start (Vbox, Message_Box.Message, Expand => False);

      Gtk.Button.Gtk_New (Message_Box.Ok_Button, "Ok");
      Gtk.Button.Set_Flags (Message_Box.Ok_Button, Gtk.Widget.Can_Default);
      Gtk.Button.Show (Message_Box.Ok_Button);
      Gtk.Box.Pack_Start (Vbox, Message_Box.Ok_Button, Expand => False);

      Gtk.Button.Grab_Default (Message_Box.Ok_Button); --  Must be in a GtkWindow to grab default
      Gtk.Button.Signal.Connect_Clicked (Message_Box.Ok_Button, On_Ok_Clicked'Access);

      Gtk.Dialog.Signal.Connect_Delete_Event (Message_Box, On_Delete'Access);

      Show (Message_Box);
      Gtk.Main.Main; --  Exits when a button is clicked
      Destroy (Message_Box);
   end Information_Box;

end Gtk.Message_Box;
