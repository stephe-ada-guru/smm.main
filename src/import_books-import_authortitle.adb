--  Abstract :
--
--  See spec
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

with Ada.Text_IO;
with Import_Books.Author_Table;
with Import_Books.Title_Table;
procedure Import_Books.Import_AuthorTitle (Root_File_Name : in String)
is

   use GNU.DB.SQLCLI;
   use MySQL_ID_Binding;
   use SAL.CSV;

   MySQL_Author_ID           : aliased MySQL_ID_Type;
   MySQL_Author_ID_Indicator : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;

   MySQL_Title_ID           : aliased MySQL_ID_Type;
   MySQL_Title_ID_Indicator : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;

   MySQL_Statement : GNU.DB.SQLCLI.SQLHANDLE;

   File_Name : constant String := Root_File_Name & "_authortitle.csv";

   File : File_Type;

   Author : Author_Table.Name_Type;
   Title  : Title_Table.Title_Type;
begin
   Ada.Text_IO.Put_Line ("Importing AuthorTitle table from " & File_Name);

   Open (File, File_Name, Max_Row_Size => 3 * (Name_Field_Length + 3) + Title_Field_Length + 5);

   if Columns (File) /= 5 then
      raise SAL.Initialization_Error with "expected 5 columns; found" & Integer'Image (Columns (File));
   end if;

   SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Statement);
   SQLPrepare (MySQL_Statement, String'("INSERT INTO AuthorTitle (Author, Title) VALUES (?, ?)"));
   SQLBindParameter (MySQL_Statement, 1, MySQL_Author_ID'Access, MySQL_Author_ID_Indicator'Access);
   SQLBindParameter (MySQL_Statement, 2, MySQL_Title_ID'Access, MySQL_Title_ID_Indicator'Access);

   loop
      exit when End_Of_File (File);

      Author_Table.Read (File, 1, Author);
      Title_Table.Read (File, 4, Title);

      Next_Row (File);

      Warm_Fuzzy;

      MySQL_Author_ID := Author_Table.Lookup (Author);
      MySQL_Title_ID  := Title_Table.Lookup (Title);

      SQLExecute (MySQL_Statement);
   end loop;

   --  Don't commit until all csv lines are processed; that lets us
   --  run the same csv file again if there are any errors.
   SQLCommit (MySQL_Connection);

   Ada.Text_IO.New_Line;
end Import_Books.Import_AuthorTitle;
