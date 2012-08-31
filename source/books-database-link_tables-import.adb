--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
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
with SAL.CSV;
procedure Books.Database.Link_Tables.Import
  (Root_File_Name : in String;
   T              : in out Table)
is
   use SAL.CSV;

   First_Column_Name : constant String := Table_Names'Image (T.Link_Names (1));
   Last_Column_Name  : constant String := Table_Names'Image (T.Link_Names (2));
   Table_Name        : constant String := First_Column_Name & Last_Column_Name;

   File_Name : constant String := Root_File_Name &
     Ada.Characters.Handling.To_Lower (First_Column_Name) & "_" &
     Ada.Characters.Handling.To_Lower (Last_Column_Name) & ".csv";

   File : SAL.CSV.File_Type;

   function Checked_Find (Link_Index : in Link_Tables.Link_Index; Old_IDs : in Link_Array_ID_Type) return Integer
   is
      use GNATCOLL.SQL.Exec;
   begin
      return Books.Import.ID_Maps (T.Link_Names (Link_Index)).Find (Old_IDs (Link_Index)).New_ID;
   exception
   when SAL.Not_Found =>
      Put_Line
        (Table_Name & Integer'Image (Old_IDs (1)) & Integer'Image (Old_IDs (2)) &
           ": " & Field_Index'Image (Link_Index) & " not found in " & Table_Names'Image (T.Link_Names (Link_Index)));
         raise;
   end Checked_Find;

begin
   Put_Line ("Importing " & Table_Name & " table from " & File_Name);

   Open (File, File_Name, Max_Row_Size => 1024);

   if Columns (File) /= 2 then
      raise SAL.Initialization_Error with "expected 2 columns; found" & Integer'Image (Columns (File));
   end if;

   Books.Import.Warm_Fuzzy_Count := 0;

   loop
      begin
         declare
            use Books.Import;

            Old_First_ID : constant Integer := Read (File, 1);
            Old_Last_ID  : constant Integer := Read (File, 2);
            New_First_ID : constant Integer := Checked_Find (1, (Old_First_ID, Old_Last_ID));
            New_Last_ID  : constant Integer := Checked_Find (2, (Old_First_ID, Old_Last_ID));
         begin
            --  There's no restriction on duplicate entries, so this should never raise an exception
            T.Insert ((New_First_ID, New_Last_ID));
         end;

         Books.Import.Warm_Fuzzy;
      exception
      when SAL.Not_Found =>
         --  From checked_find
         null;
      end;

      exit when End_Of_File (File);

      Next_Row (File);

   end loop;

   New_Line;
   Put_Line (Integer'Image (Books.Import.Warm_Fuzzy_Count) & " " & Table_Name);

end Books.Database.Link_Tables.Import;
