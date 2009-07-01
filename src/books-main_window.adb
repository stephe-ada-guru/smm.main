--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
with Books.Database.Data_Tables.Author;
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
with Books.Database.Link_Tables.AuthorTitle;
with Books.Database.Link_Tables.CollectionTitle;
with Books.Database.Link_Tables.SeriesTitle;
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

   procedure Initialize_DB (Tables : in out Table_Views.Tables_Type; DB : in Books.Database.Database_Access)
   is
      use type Books.Database.Data_Tables.Table_Access;
   begin
      Tables.Sibling (Author)     := new Books.Database.Data_Tables.Author.Table (DB);
      Tables.Sibling (Title)      := new Books.Database.Data_Tables.Title.Table (DB);
      Tables.Sibling (Collection) := new Books.Database.Data_Tables.Collection.Table (DB);
      Tables.Sibling (Series)     := new Books.Database.Data_Tables.Series.Table (DB);
      Tables.AuthorTitle          := new Books.Database.Link_Tables.AuthorTitle.Table (DB);
      Tables.CollectionTitle      := new Books.Database.Link_Tables.CollectionTitle.Table (DB);
      Tables.SeriesTitle          := new Books.Database.Link_Tables.SeriesTitle.Table (DB);

      Books.Database.Data_Tables.Author.Initialize
        (Books.Database.Data_Tables.Author.Table (Tables.Sibling (Author).all));
      Books.Database.Data_Tables.Title.Initialize
        (Books.Database.Data_Tables.Title.Table (Tables.Sibling (Title).all));
      Books.Database.Data_Tables.Collection.Initialize
        (Books.Database.Data_Tables.Collection.Table (Tables.Sibling (Collection).all));
      Books.Database.Data_Tables.Series.Initialize
        (Books.Database.Data_Tables.Series.Table (Tables.Sibling (Series).all));
      Books.Database.Link_Tables.AuthorTitle.Initialize (Tables.AuthorTitle.all);
      Books.Database.Link_Tables.CollectionTitle.Initialize (Tables.CollectionTitle.all);
      Books.Database.Link_Tables.SeriesTitle.Initialize (Tables.SeriesTitle.all);

   end Initialize_DB;

   procedure Finalize_DB (Tables : in out Books.Table_Views.Tables_Type)
   is
      use type Books.Database.Data_Tables.Table_Access;
   begin
      if Tables.Sibling (Author) /= null then
         Books.Database.Free (Books.Database.Table_Access (Tables.Sibling (Author)));
         Books.Database.Free (Books.Database.Table_Access (Tables.Sibling (Title)));
         Books.Database.Free (Books.Database.Table_Access (Tables.Sibling (Collection)));
         Books.Database.Free (Books.Database.Table_Access (Tables.Sibling (Series)));
         Books.Database.Free (Books.Database.Table_Access (Tables.AuthorTitle));
         Books.Database.Free (Books.Database.Table_Access (Tables.CollectionTitle));
         Books.Database.Free (Books.Database.Table_Access (Tables.SeriesTitle));
      end if;
   end Finalize_DB;

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

      Finalize_DB (Window.Parameters.Tables);

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

      Initialize_DB (Window.Parameters.Tables, Window.Parameters.DB);

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
