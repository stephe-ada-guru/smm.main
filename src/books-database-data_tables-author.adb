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

package body Books.Database.Data_Tables.Author is

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding procedure Initialize (T : in out Table)
   is begin
      --  from Database.Data_Tables
      T.Find_By_ID_Statement := new String'("SELECT ID, First, Middle, Last FROM Author WHERE ID = ?");

      T.Find_By_Name_Statement := new String'
        ("SELECT ID, First, Middle, Last FROM Author WHERE Last LIKE ? ORDER BY Last, First, Middle");

      Checked_Execute (T, T.Find_By_Name_Statement.all); --  So Next is valid.

   end Initialize;

   procedure Insert
     (T           : in out Table;
      First_Name  : in     String;
      Middle_Name : in     String;
      Last_Name   : in     String)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;

      First  : aliased String := First_Name;
      Middle : aliased String := Middle_Name;
      Last   : aliased String := Last_Name;

      --  We use parameters so we don't have to quote the names
      Statement : constant String := "INSERT INTO Author (First, Middle, Last) VALUES (?, ?, ?)";
   begin
      --  FIXME: when do these allocations get freed? Try Unchecked_Access
      Checked_Execute
        (T, Statement, Params => (+First'Unchecked_Access, +Middle'Unchecked_Access, +Last'Unchecked_Access));
      Find_By_Name (T, Last_Name);
   end Insert;

   procedure Update
     (T           : in out Table;
      First_Name  : in     String;
      Middle_Name : in     String;
      Last_Name   : in     String)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;

      --  We use parameters so we don't have to quote the names
      Statement : constant String := "UPDATE Author SET First = ?, Middle = ?, Last = ? WHERE ID = " &
        Field (T, ID_Index);
   begin
      --  FIXME: when do these allocations get freed?
      Checked_Execute
        (T, Statement, Params => (+new String'(First_Name), +new String'(Middle_Name), +new String'(Last_Name)));
   end Update;

end Books.Database.Data_Tables.Author;
