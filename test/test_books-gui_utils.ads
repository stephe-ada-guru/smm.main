--  Abstract :
--
--  Utilities for GUI tests for Books
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Strings.Unbounded;
with Books.Main_Window;
with Gdk.Event;
with Gtk.Gen_Background_Window;
package Test_Books.GUI_Utils is

   procedure Empty_Database;
   --  Run 'make empty_database_test'.

   Main_Window : Books.Main_Window.Gtk_Window;
   Config_File : Ada.Strings.Unbounded.Unbounded_String;

   procedure Create_Show_Main_Window;

   package Background is new Gtk.Gen_Background_Window (Create_Show_Main_Window);

   procedure Set_Up_Case
     (Config_File : access String;
      Debug_Level : in     Integer;
      Event_Handler : in     Gdk.Event.Event_Handler_Func := null);
   --  Set up Main_Window for typical GUI test

   procedure Create_Some_Data;
   --  standard set of data for tests

   procedure Add_Author
     (First  : in String;
      Middle : in String;
      Last   : in String);
   --  Use Mouse & Keyboard events to add author. Assumes Author
   --  window is showing main display. Last is typed into the Find
   --  window, as the user normally will do.

   procedure Find_Author (Last : in String);
   --  Use Mouse & Keyboard events to search for author, assuming Last
   --  is unique. Assumes Author window is showing main display. Last
   --  is typed into the Find window.

   procedure Add_Title
     (Title   : in String;
      Year    : in String;
      Comment : in String;
      Rating  : in String);
   --  Use Mouse & Keyboard events to add title. Assumes Title
   --  window is showing main display. Title is typed into the Find
   --  window, as the user normally will do.

   procedure Find_Title (Title : in String);
   --  Use Mouse & Keyboard events to search for title, assuming Title
   --  is unique. Assumes Title window is showing main display. Title
   --  is typed into the Find window.

end Test_Books.GUI_Utils;
