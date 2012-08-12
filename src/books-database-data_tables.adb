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

package body Books.Database.Data_Tables is

   use type GNATCOLL.SQL.Exec.SQL_Parameter; -- for "+"

   --  Subprogram bodies (alphabetical order)

   procedure Delete (T : in out Table'Class)
   is begin
      Checked_Execute (T, T.Delete_By_ID_Statement.all, Params => (1 => +ID (T)));
      Next (T);
   end Delete;

   procedure Fetch (T : in out Table'Class; ID : in ID_Type)
   is begin
      Find (T, T.Find_By_ID_Statement, Params => (1 => +ID));
   end Fetch;

   overriding procedure Finalize (T : in out Table)
   is begin
      Books.Database.Finalize (Books.Database.Table (T));
   end Finalize;

   function ID (T : in Table'Class) return ID_Type
   is begin
      return ID_Type'Value (Field (T, ID_Index));
   end ID;

   procedure Set_Find_By_ID (T : in out Table'Class)
   is begin
      T.Find_Statement := T.Find_By_ID_Statement;
   end Set_Find_By_ID;

   procedure Set_Find_By_Name (T : in out Table'Class)
   is begin
      T.Find_Statement := T.Find_By_Name_Statement;
   end Set_Find_By_Name;

   procedure Find (T : in out Table'Class; Item : in String)
   is begin
      --  FIXME: when is this allocation freed?
      Find (T, T.Find_By_Name_Statement, Params => (1 => +(new String'(Item & '%'))));
   end Find;

end Books.Database.Data_Tables;
