--  Abstract :
--
--  main procedure to import CSV data into Books database
--
--  Copyright (C) 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Books.Database.Data_Tables.Author.Import;
with Books.Database.Data_Tables.Collection.Import;
with Books.Database.Data_Tables.Gen_Import;
with Books.Database.Data_Tables.Series.Import;
with Books.Database.Data_Tables.Title.Import;
with Books.Database.Link_Tables.Import;
with SAL.Config_Files;
procedure Books.Import.Main
is
   procedure Import_Author is new Books.Database.Data_Tables.Gen_Import
     (Table_Name       => "Author",
      Column_Count     => 4, --  ID, first, middle, last
      Read_Insert_Find => Books.Database.Data_Tables.Author.Import.Read_Insert_Find);

   procedure Import_Collection is new Books.Database.Data_Tables.Gen_Import
     (Table_Name       => "Collection",
      Column_Count     => 4, --  ID, Title, Year, editor_id
      Read_Insert_Find => Books.Database.Data_Tables.Collection.Import.Read_Insert_Find);

   procedure Import_Series is new Books.Database.Data_Tables.Gen_Import
     (Table_Name       => "Series",
      Column_Count     => 3, --  ID, Title, Author_id
      Read_Insert_Find => Books.Database.Data_Tables.Series.Import.Read_Insert_Find);

   procedure Import_Title is new Books.Database.Data_Tables.Gen_Import
     (Table_Name       => "Title",
      Column_Count     => 5, --  ID, Title, Year, Comment, Rating
      Read_Insert_Find => Books.Database.Data_Tables.Title.Import.Read_Insert_Find);

begin
   if Argument_Count /= 2 then
      Put_Line ("usage: books-import-main.exe <config_file> <root_csv_file_path>");
      Put_Line ("config: ");
      Put_Line ("Database_File, default $HOME/.books/books.db");
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      use Books.Database;

      Config : constant SAL.Config_Files.Configuration_Access_Type := SAL.Config_Files.Open (Argument (1));

      Root_File_Name : constant String := Ada.Command_Line.Argument (2);

      DB : Database_Access := new Books.Database.Database (Config);
   begin
      Author_Table     := new Data_Tables.Author.Table (DB);
      Title_Table      := new Data_Tables.Title.Table (DB);
      Collection_Table := new Data_Tables.Collection.Table (DB);
      Series_Table     := new Data_Tables.Series.Table (DB);

      Links (Author, Collection) := new Link_Tables.Table (new Link_Tables.Link_Names'(Author, Collection), DB);
      Links (Author, Series)     := new Link_Tables.Table (new Link_Tables.Link_Names'(Author, Series), DB);
      Links (Author, Title)      := new Link_Tables.Table (new Link_Tables.Link_Names'(Author, Title), DB);

      Links (Collection, Title) := new Link_Tables.Table (new Link_Tables.Link_Names'(Collection, Title), DB);
      Links (Series, Title)     := new Link_Tables.Table (new Link_Tables.Link_Names'(Series, Title), DB);

      Import_Author (Root_File_Name);
      Import_Collection (Root_File_Name);
      Import_Series (Root_File_Name);
      Import_Title (Root_File_Name);

      Link_Tables.Import (Root_File_Name, Links (Author, Title).all);
      Link_Tables.Import (Root_File_Name, Links (Collection, Title).all);
      Link_Tables.Import (Root_File_Name, Links (Series, Title).all);
   exception
   when E : others =>
      Set_Exit_Status (Ada.Command_Line.Failure);

      Put_Line
        ("Exception " &
           Ada.Exceptions.Exception_Name (E) & " " &
           Ada.Exceptions.Exception_Message (E));
      Free (DB);
   end;
end Books.Import.Main;
