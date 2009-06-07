--  Abstract :
--
--  main procedure to import CSV data into Books database
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Import_Books.Author_Table;
with Import_Books.Collection_Table;
with Import_Books.Import_Author;
with Import_Books.Import_AuthorTitle;
with Import_Books.Import_Collection;
with Import_Books.Import_CollectionTitle;
with Import_Books.Import_Series;
with Import_Books.Import_SeriesTitle;
with Import_Books.Import_Title;
with Import_Books.Series_Table;
with Import_Books.Title_Table;
procedure Import_Books.Main
is begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count /= 2 then
         Ada.Text_IO.Put_Line ("usage: import_books <database_username> <root_file_name>");
         Set_Exit_Status (Failure);
         return;
      end if;
   end;

   declare
      Root_File_Name : constant String := Ada.Command_Line.Argument (2);
   begin
      Connect (Database_Username => Ada.Command_Line.Argument (1));

      Author_Table.Initialize;
      Collection_Table.Initialize;
      Series_Table.Initialize;
      Title_Table.Initialize;

      Import_Books.Import_Author (Root_File_Name);
      Import_Books.Import_Collection (Root_File_Name);
      Import_Books.Import_Series (Root_File_Name);
      Import_Books.Import_Title (Root_File_Name);

      Import_Books.Import_AuthorTitle (Root_File_Name);
      Import_Books.Import_CollectionTitle (Root_File_Name);
      Import_Books.Import_SeriesTitle (Root_File_Name);
   end;
exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   Ada.Text_IO.Put_Line
     ("Exception " &
        Ada.Exceptions.Exception_Name (E) & " " &
        Ada.Exceptions.Exception_Message (E));
   Disconnect;
end Import_Books.Main;
