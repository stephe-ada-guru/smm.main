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

with AUnit.Assertions;
with Gdk.Test_Events;
with GNAT.OS_Lib;
with Gtk.Widget.Config;
with SAL.Config_Files;
package body Test_Books.GUI_Utils is

   procedure Set_Window_Origins (Config_File : in Ada.Strings.Unbounded.Unbounded_String)
   is
      use SAL.Config_Files;
      Config   : Configuration_Type;
      Geometry : Gtk.Widget.Gtk_Allocation;
   begin
      Open (Config, Ada.Strings.Unbounded.To_String (Config_File));

      Geometry    := Gtk.Widget.Config.Read (Config, "Main.Geometry");
      Main_Origin := (Geometry.X, Geometry.Y);

      Geometry      := Gtk.Widget.Config.Read (Config, "Author.Geometry");
      Author_Origin := (Geometry.X, Geometry.Y);

      Geometry     := Gtk.Widget.Config.Read (Config, "Title.Geometry");
      Title_Origin := (Geometry.X, Geometry.Y);

      Close (Config);
   end Set_Window_Origins;

   procedure Empty_Database
   is
      Make_Args : GNAT.OS_Lib.Argument_List_Access := GNAT.OS_Lib.Argument_String_To_List ("empty_database_test");
      Success : Boolean;
   begin
      GNAT.OS_Lib.Spawn ("make", Make_Args.all, Success);
      GNAT.OS_Lib.Free (Make_Args);

      AUnit.Assertions.Assert (Success, "make empty_database failed");

   end Empty_Database;

   procedure Add_Author
     (First  : in String;
      Middle : in String;
      Last   : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Author_Origin + Find_Entry);
      Mouse_Double_Click;
      Key_Stroke (Last);
      Alt_Key_Stroke ('a'); -- Add
      Shift_Tab;
      Key_Stroke (Middle);
      Shift_Tab;
      Key_Stroke (First);
      Alt_Key_Stroke ('i'); -- Insert
   end Add_Author;

   procedure Add_Title
     (Title   : in String;
      Year    : in String;
      Comment : in String;
      Rating  : in String)
   is
      use Gdk.Test_Events;
   begin
      Mouse_Move (Title_Origin + Find_Entry);
      Mouse_Double_Click;
      Key_Stroke (Title);
      Alt_Key_Stroke ('a'); -- Add
      Key_Stroke (Tab); --  Year
      Key_Stroke (Year);
      Key_Stroke (Tab); --  Comment
      Key_Stroke (Comment);
      Key_Stroke (Tab); --  Rating
      Key_Stroke (Rating);
      Alt_Key_Stroke ('i'); -- Insert
   end Add_Title;

end Test_Books.GUI_Utils;
