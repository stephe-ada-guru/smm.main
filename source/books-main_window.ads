--  Abstract :
--
--  Main window for Books application
--
--  Copyright (C) 2002, 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Books.Database;
with Books.Table_Views.Author;
with Books.Table_Views.Collection;
with Books.Table_Views.Series;
with Books.Table_Views.Title;
with Gtk.Window;
with SAL.Config_Files;
package Books.Main_Window is

   type Gtk_Window_Record is new Gtk.Window.Gtk_Window_Record with record
      --  All components public for unit tests
      Author_View     : Books.Table_Views.Author.Gtk_Author_View;
      Title_View      : Books.Table_Views.Title.Gtk_Title_View;
      Collection_View : Books.Table_Views.Collection.Gtk_Collection_View;
      Series_View     : Books.Table_Views.Series.Gtk_Series_View;

      DB     : Books.Database.Database_Access;
      Config : SAL.Config_Files.Configuration_Access_Type;
   end record;
   type Gtk_Window is access all Gtk_Window_Record'Class;

   procedure Gtk_New
     (Window      :    out Gtk_Window;
      Config_File : in     String     := "books.config");

end Books.Main_Window;
