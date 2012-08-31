--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

package body Books.Database.Link_Tables is

   function To_Params (Data : in Link_Array_ID_Type) return GNATCOLL.SQL.Exec.SQL_Parameters
   is
      use GNATCOLL.SQL.Exec;
   begin
      return
        (1 => +Data (Link_Array_ID_Type'First),
         2 => +Data (Link_Array_ID_Type'Last));
   end To_Params;

   function Find_Link_Index (T : in Table'Class; Name : in Table_Names) return Link_Index
   is begin
      if T.Link_Names (0) = Name then
         return 0;
      elsif T.Link_Names (1) = Name then
         return 1;
      else
         raise SAL.Programmer_Error with Table_Names'Image (Name) & " is not a link in link table " &
           Table_Names'Image (T.Link_Names (0)) & Table_Names'Image (T.Link_Names (1));
      end if;
   end Find_Link_Index;

   procedure Delete (T : in out Table; Data : in Link_Array_ID_Type)
   is
      First_Column_Name : constant String := Table_Names'Image (T.Link_Names (0));
      Last_Column_Name  : constant String := Table_Names'Image (T.Link_Names (1));
      Table_Name        : constant String := First_Column_Name & Last_Column_Name;
   begin
      Checked_Execute
        (T,
         "DELETE FROM " &
           Table_Name &
           " WHERE " &
           First_Column_Name & " = ? AND " &
           Last_Column_Name & " = ?",
         To_Params (Data));
   end Delete;

   procedure Find (T : in out Table; Name : in Table_Names; Item : in ID_Type)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;
      First_Column_Name : constant String := Table_Names'Image (T.Link_Names (0));
      Last_Column_Name  : constant String := Table_Names'Image (T.Link_Names (1));
      Table_Name        : constant String := First_Column_Name & Last_Column_Name;
   begin
      Find
        (T,
         "SELECT " & First_Column_Name & ", " & Last_Column_Name & " FROM " & Table_Name &
           " WHERE " & Table_Names'Image (T.Link_Names (Find_Link_Index (T, Name))) & " = ?",
         Params => (1 => +Item));
   end Find;

   function ID (T : in Table; Name : in Table_Names) return ID_Type is
   begin
      return ID_Type'Value (Field (T, Find_Link_Index (T, Name)));
   end ID;

   procedure Insert (T : in out Table; Data : in Link_Array_ID_Type)
   is
      First_Column_Name : constant String := Table_Names'Image (T.Link_Names (0));
      Last_Column_Name  : constant String := Table_Names'Image (T.Link_Names (1));
      Table_Name        : constant String := First_Column_Name & Last_Column_Name;
   begin
      Checked_Execute
        (T,
         "INSERT INTO " & Table_Name & "(" & First_Column_Name & ", " & Last_Column_Name & ") VALUES (?, ?)",
         To_Params (Data));
   end Insert;

end Books.Database.Link_Tables;
