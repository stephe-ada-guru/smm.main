--  Abstract :
--
--  see spec
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Import_Books.Title_Table;
procedure Import_Books.Import_Title (Root_File_Name : in String)
is
   use GNU.DB.SQLCLI;
   use Int_16_Binding;
   use Unsigned_8_Binding;
   use SAL.CSV;

   Data : Title_Table.Data_Type;

   File_Name : constant String := Root_File_Name & "_title.csv";

   File : File_Type;

   MySQL_Statement : SQLHANDLE;

begin
   Ada.Text_IO.Put_Line ("Importing Title table from " & File_Name);

   Open (File, File_Name, Max_Row_Size => 2 * (Title_Field_Length + 3) + 20);

   if Columns (File) /= 4 then
      raise SAL.Initialization_Error with "expected 4 columns; found" & Integer'Image (Columns (File));
   end if;

   SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Statement);
   SQLPrepare (MySQL_Statement, String'("INSERT INTO Title (Title, Year, Comment, Rating) VALUES (?, ?, ?, ?)"));
   SQLBindParameter (MySQL_Statement, 1, Data.Title, Data.Title_Length'Access);
   SQLBindParameter (MySQL_Statement, 2, Data.Year'Access, Data.Year_Indicator'Access);
   SQLBindParameter (MySQL_Statement, 3, Data.Comment, Data.Comment_Length'Access);
   SQLBindParameter (MySQL_Statement, 4, Data.Rating'Access, Data.Rating_Indicator'Access);

   Warm_Fuzzy_Count := 0;
   loop
      Title_Table.Read (File, 1, Data);

      begin
         SQLExecute (MySQL_Statement);
      exception
      when E : Database_Error =>
         if Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (E), "Data too Long") /= 0 then
            raise Database_Error with "'" & Data.Title (1 .. Integer (Data.Title_Length)) & "'" & " too long";
         else
            raise;
         end if;
      end;

      Warm_Fuzzy;

      exit when End_Of_File (File);

      Next_Row (File);
   end loop;

   SQLCommit (MySQL_Connection);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Integer'Image (Warm_Fuzzy_Count) & " titles");
end Import_Books.Import_Title;
