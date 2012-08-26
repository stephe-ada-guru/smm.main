--  Abstract :
--
--  see spec
--
--  Copyright (C) 2002, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with Books.Import; use Books.Import;
with SAL.CSV;      use SAL.CSV;
procedure Books.Database.Data_Tables.Author.Import (Root_File_Name : in String)
is
   File_Name : constant String := Root_File_Name & "author.csv";

   File : SAL.CSV.File_Type;
begin
   Put_Line ("Importing Author table from " & File_Name);

   Open (File, File_Name, Max_Row_Size => 1024);

   --  columns: ID, First, Middle, Last
   if Columns (File) /= 4 then
      raise SAL.Initialization_Error with "expected 4 columns; found" & Integer'Image (Columns (File));
   end if;

   Warm_Fuzzy_Count := 0;

   loop
      declare
         use type GNATCOLL.SQL.Exec.SQL_Parameter;
         Old_ID      : constant Integer        := Read (File, 1);
         First_Name  : aliased constant String := Unquote (Read (File, 2));
         Middle_Name : aliased constant String := Unquote (Read (File, 3));
         Last_Name   : aliased constant String := Unquote (Read (File, 4));
      begin
         begin
            Author_Table.Insert (First_Name, Middle_Name, Last_Name);
         exception
         when Entry_Error =>
            --  Presumably a duplicate name; find it, map a second old id to it
            New_Line;
            --  GNATCOLL.Fetch outputs a nice error message for the failed insert
         end;

         --  FIXME: use Exec.Last_ID?
         if Middle_Name'Length > 0 then
            Find
              (Author_Table.all,
               "SELECT ID, First, Middle, Last FROM Author WHERE First = ? and Middle = ? and Last = ?",
               (+First_Name'Unchecked_Access, +Middle_Name'Unchecked_Access, +Last_Name'Unchecked_Access));
         else
            Find
              (Author_Table.all,
               "SELECT ID, First, Middle, Last FROM Author WHERE First = ? and Last = ?",
               (+First_Name'Unchecked_Access, +Last_Name'Unchecked_Access));
         end if;

         if Author_Table.Valid then
            --  At this point, either the newly inserted, or another matching, record is current
            Author_ID_Map.Add ((Old_ID, Author_Table.ID));
         else
            --  Some other error occurred; one of the above statements
            --  should have raised an exception that was not handled.
            --  But just in case:
            raise SAL.Programmer_Error with Integer'Image (Old_ID) & ", '" &
              First_Name & "', '" & Middle_Name & "', '" & Last_Name &
              "' not found in Authors";
         end if;
      end;

      Warm_Fuzzy;

      exit when End_Of_File (File);

      Next_Row (File);

   end loop;

   New_Line;
   Put_Line (Integer'Image (Warm_Fuzzy_Count) & " authors");

end Books.Database.Data_Tables.Author.Import;
