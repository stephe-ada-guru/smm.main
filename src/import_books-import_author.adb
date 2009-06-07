--  Abstract :
--
--  see spec
--
--  Copyright (C) 2002, 2009 Stephen Leake.  All Rights Reserved.
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
procedure Import_Books.Import_Author (Root_File_Name : in String)
is
   use GNU.DB.SQLCLI;
   use SAL.CSV;

   Name : Author_Table.Name_Type;
   Count : Integer := 0;

   File_Name : constant String := Root_File_Name & "_author.csv";

   File : File_Type;

   MySQL_Statement : SQLHANDLE;

begin
   Ada.Text_IO.Put_Line ("Importing Author table from " & File_Name);

   Open (File, File_Name, Max_Row_Size => 3 * (Name_Field_Length + 3));

   if Columns (File) /= 3 then
      raise SAL.Initialization_Error with "expected 3 columns; found" & Integer'Image (Columns (File));
   end if;

   SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Statement);
   SQLPrepare (MySQL_Statement, String'("INSERT INTO Author (First, Middle, Last) VALUES (?, ?, ?)"));
   SQLBindParameter (MySQL_Statement, 1, Name.First, Name.First_Length'Access);
   SQLBindParameter (MySQL_Statement, 2, Name.Middle, Name.Middle_Length'Access);
   SQLBindParameter (MySQL_Statement, 3, Name.Last, Name.Last_Length'Access);

   loop
      Author_Table.Read (File, 1, Name);

      Warm_Fuzzy;

      SQLExecute (MySQL_Statement);

      Count := Count + 1;

      exit when End_Of_File (File);

      Next_Row (File);

   end loop;

   --  Don't commit until all csv lines are processed; that lets us
   --  run the same csv file again if there are any errors.
   SQLCommit (MySQL_Connection);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Integer'Image (Count) & " titles");

end Import_Books.Import_Author;
