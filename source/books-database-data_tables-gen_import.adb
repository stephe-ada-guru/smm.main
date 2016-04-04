--  Abstract :
--
--  See spec
--
--  Copyright (C) 2012, 2016 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Books.Import;
procedure Books.Database.Data_Tables.Gen_Import (Root_File_Name : in String)
is
   use SAL.CSV;

   File_Name : constant String := Root_File_Name & Ada.Characters.Handling.To_Lower (Table_Name) & ".csv";

   File : SAL.CSV.File_Type;
begin
   Put_Line ("Importing " & Table_Name & " table from " & File_Name);

   Open (File, File_Name, Max_Row_Size => 1024, Delimiter => Delimiter);

   if Columns (File) /= Column_Count then
      raise SAL.Initialization_Error with "expected" & Integer'Image (Column_Count) &
        " columns; found" & Integer'Image (Columns (File));
   end if;

   Books.Import.Warm_Fuzzy_Count := 0;

   loop
      Read_Insert_Find (File);

      Books.Import.Warm_Fuzzy;

      exit when End_Of_File (File);

      Next_Row (File);

   end loop;

   New_Line;
   Put_Line (Integer'Image (Books.Import.Warm_Fuzzy_Count) & " " & Table_Name);

end Books.Database.Data_Tables.Gen_Import;
