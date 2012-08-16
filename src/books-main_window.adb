--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2009, 2012 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with Ada.Text_IO;
with Books.Database.Data_Tables.Author;
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
with Books.Database.Link_Tables;
with Books.Database;
with Gdk.Event;
with Gdk.Window;
with Gtk.Enums;
with Gtk.Main;
with Gtk.Object.Signal;
with Gtk.Widget.Signal;
with Gtk.Window.Config;
with Gtk.Window.Signal;
with SAL.Config_Files;
package body Books.Main_Window is

   procedure Create_Tables
     (DB       : in     Books.Database.Database_Access;
      Siblings :    out Table_Arrays;
      Links    :    out Link_Arrays)
   is
      use Books.Database.Link_Tables;
      use type Books.Database.Data_Tables.Table_Access;
   begin
      Siblings (Author)          := new Books.Database.Data_Tables.Author.Table (DB);
      Siblings (Title)           := new Books.Database.Data_Tables.Title.Table (DB);
      Siblings (Collection)      := new Books.Database.Data_Tables.Collection.Table (DB);
      Siblings (Series)          := new Books.Database.Data_Tables.Series.Table (DB);
      Links (Author, Collection) := new Books.Database.Link_Tables.Table (new Link_Names'(Author, Collection), DB);
      Links (Author, Series)     := new Books.Database.Link_Tables.Table (new Link_Names'(Author, Series), DB);
      Links (Author, Title)      := new Books.Database.Link_Tables.Table (new Link_Names'(Author, Title), DB);
      Links (Collection, Title)  := new Books.Database.Link_Tables.Table (new Link_Names'(Collection, Title), DB);
      Links (Series, Title)      := new Books.Database.Link_Tables.Table (new Link_Names'(Series, Title), DB);
   end Create_Tables;

   procedure Free_Tables
     (Siblings : in out Table_Arrays;
      Links    : in out Link_Arrays)
   is
      use type Books.Database.Data_Tables.Table_Access;
   begin
      if Siblings (Author) /= null then
         for I in Siblings'Range loop
            Books.Database.Free (Books.Database.Table_Access (Siblings (I)));
         end loop;
         for I in Links'Range (1) loop
            for J in Links'Range (2) loop
               Books.Database.Free (Books.Database.Table_Access (Links (I, J)));
            end loop;
         end loop;
      end if;
   end Free_Tables;

   function On_Window_Configure_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event_Configure)
     return Boolean
   is
      pragma Unreferenced (Event);
      Window : constant Gtk_Window := Gtk_Window (Widget);
   begin
      Gtk.Window.Config.Save_Geometry (Window, Window.Parameters.Config.all, "Main");
      return False; --  propagate signal
   end On_Window_Configure_Event;

   procedure On_Window_Destroy (Object : access Gtk.Object.Gtk_Object_Record'Class)
   is
      Window : constant Gtk_Window := Gtk_Window (Object);

      use type Books.Database.Database_Access;
      use type SAL.Config_Files.Configuration_Access_Type;
      use type Books.Table_Views.Author.Gtk_Author_View;

   begin
      if Window.Author_View /= null then

         Books.Table_Views.Author.Destroy (Window.Author_View);
         Books.Table_Views.Title.Destroy (Window.Title_View);
         Books.Table_Views.Collection.Destroy (Window.Collection_View);
         Books.Table_Views.Series.Destroy (Window.Series_View);

         Window.Author_View := null;
      end if;

      Free_Tables (Window.Parameters.Siblings, Window.Parameters.Links);

      if Window.Parameters.DB /= null then
         Books.Database.Free (Window.Parameters.DB);
      end if;

      if Window.Parameters.Config /= null then
         SAL.Config_Files.Close (Window.Parameters.Config.all);
         SAL.Config_Files.Free (Window.Parameters.Config);
      end if;

      Gtk.Main.Main_Quit;
   end On_Window_Destroy;

   function On_Window_State_Event
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : in     Gdk.Event.Gdk_Event)
     return Boolean
   is
      pragma Unreferenced (Event);
      Window    : constant Gtk_Window := Gtk_Window (Widget);
      New_State : constant Gdk.Event.Gdk_Window_State := Gdk.Window.Get_State (Gtk.Widget.Get_Window (Widget));
   begin
      case New_State is
      when Gdk.Event.Window_State_Iconified =>
         Books.Table_Views.Author.Hide (Window.Author_View);
         Books.Table_Views.Title.Hide (Window.Title_View);
         Books.Table_Views.Collection.Hide (Window.Collection_View);
         Books.Table_Views.Series.Hide (Window.Series_View);

      when others =>
         Books.Table_Views.Author.Show (Window.Author_View);
         Books.Table_Views.Title.Show (Window.Title_View);
         Books.Table_Views.Collection.Show (Window.Collection_View);
         Books.Table_Views.Series.Show (Window.Series_View);

      end case;
      return False; -- propagate.
   end On_Window_State_Event;

   ----------
   --  Public subprograms

   procedure Initialize
     (Window      : access Gtk_Window_Record'Class;
      Config_File : in     String             := "books.config")
   is
      use type Books.Database.Database_Access;
      use type SAL.Config_Files.Configuration_Access_Type;
      use Gtk.Enums;
   begin
      --  Connect to the database first, so the various table
      --  interfaces can get the initial data.
      if Window.Parameters.Config = null then
         Window.Parameters.Config := new SAL.Config_Files.Configuration_Type;

         --  We save window positions in the config file, so it is not
         --  read-only.
         SAL.Config_Files.Open (Window.Parameters.Config.all, Config_File, Read_Only => False);
         Ada.Text_IO.Put_Line
           ("using config file " & SAL.Config_Files.Writeable_File_Name (Window.Parameters.Config.all));
      end if;

      if Window.Parameters.DB = null then
         Window.Parameters.DB := new Books.Database.Database (Window.Parameters.Config);
      end if;

      Gtk.Window.Initialize (Window, Window_Toplevel);
      Set_Title (Window, "Books");
      Gtk.Window.Config.Set_Geometry
        (Window, Window.Parameters.Config.all, "Main", Default => (10, 10, 50, 50));

      Gtk.Window.Signal.Connect_Configure_Event (Window, On_Window_Configure_Event'Access);
      Gtk.Object.Signal.Connect_Destroy (Window, On_Window_Destroy'Access);
      Gtk.Widget.Signal.Connect_Window_State_Event (Window, On_Window_State_Event'Access);

      Create_Tables (Window.Parameters.DB, Window.Parameters.Siblings, Window.Parameters.Links);

      Books.Table_Views.Author.Gtk_New (Window.Author_View, Window.Parameters);
      Books.Table_Views.Author.Set_Title (Window.Author_View, "Author");
      Books.Table_Views.Author.Show (Window.Author_View);
      Gtk.Window.Config.Set_Geometry
        (Window.Author_View, Window.Parameters.Config.all, "Author", Default => (10, 10, 150, 250));

      Books.Table_Views.Title.Gtk_New (Window.Title_View, Window.Parameters);
      Books.Table_Views.Title.Set_Title (Window.Title_View, "Title");
      Books.Table_Views.Title.Show (Window.Title_View);
      Gtk.Window.Config.Set_Geometry
        (Window.Title_View, Window.Parameters.Config.all, "Title", Default => (10, 10, 150, 250));

      Books.Table_Views.Collection.Gtk_New (Window.Collection_View, Window.Parameters);
      Books.Table_Views.Collection.Set_Title (Window.Collection_View, "Collection");
      Books.Table_Views.Collection.Show (Window.Collection_View);
      Gtk.Window.Config.Set_Geometry
        (Window.Collection_View, Window.Parameters.Config.all, "Collection", Default => (10, 10, 150, 250));

      Books.Table_Views.Series.Gtk_New (Window.Series_View, Window.Parameters);
      Books.Table_Views.Series.Set_Title (Window.Series_View, "Series");
      Books.Table_Views.Series.Show (Window.Series_View);
      Gtk.Window.Config.Set_Geometry
        (Window.Series_View, Window.Parameters.Config.all, "Series", Default => (10, 10, 150, 250));

      declare
         Sibling_Views : constant Books.Table_Views.Table_Array_Table_View_Type :=
           (Author     => Books.Table_Views.Gtk_Table_View (Window.Author_View),
            Collection => Books.Table_Views.Gtk_Table_View (Window.Collection_View),
            Series     => Books.Table_Views.Gtk_Table_View (Window.Series_View),
            Title      => Books.Table_Views.Gtk_Table_View (Window.Title_View));
      begin
         Books.Table_Views.Set_Sibling_Views (Window.Author_View, Sibling_Views);
         Books.Table_Views.Set_Sibling_Views (Window.Collection_View, Sibling_Views);
         Books.Table_Views.Set_Sibling_Views (Window.Series_View, Sibling_Views);
         Books.Table_Views.Set_Sibling_Views (Window.Title_View, Sibling_Views);
      end;

   end Initialize;

   procedure Gtk_New
     (Window      :    out Gtk_Window;
      Config_File : in     String     := "books.config")
   is begin
      Window := new Gtk_Window_Record;
      Initialize (Window, Config_File);
   end Gtk_New;

end Books.Main_Window;
