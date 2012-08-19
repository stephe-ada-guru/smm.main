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

with Ada.Text_IO;
with Books.Import; use Books.Import;
with SAL.CSV;      use SAL.CSV;
procedure Books.Database.Data_Tables.Author.Import (Root_File_Name : in String)
is
   File_Name : constant String := Root_File_Name & "_author.csv";

   File : File_Type;
begin
   Ada.Text_IO.Put_Line ("Importing Author table from " & File_Name);

   Open (File, File_Name, Max_Row_Size => 1024);

   --  columns: ID, First, Middle, Last
   if Columns (File) /= 4 then
      raise SAL.Initialization_Error with "expected 4 columns; found" & Integer'Image (Columns (File));
   end if;

   Warm_Fuzzy_Count := 0;

   loop
      declare
         Old_ID : constant Integer := Read (File, 1);
      begin
         Author_Table.Insert
           (First_Name  => Read (File, 2),
            Middle_Name => Read (File, 3),
            Last_Name   => Read (File, 4));

         Author_ID_Map.Add ((Old_ID, Author_Table.ID));
      end;

      Warm_Fuzzy;

      exit when End_Of_File (File);

      Next_Row (File);

   end loop;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Integer'Image (Warm_Fuzzy_Count) & " authors");

end Books.Database.Data_Tables.Author.Import;
