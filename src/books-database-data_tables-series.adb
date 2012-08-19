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

package body Books.Database.Data_Tables.Series is

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding procedure Initialize (T : in out Table)
   is
   begin
      T.Find_By_ID_Statement := new String'("SELECT ID, Title FROM Series WHERE ID = ?");

      T.Find_By_Name_Statement := new String'("SELECT ID, Title FROM Series WHERE Title LIKE ? ORDER BY Title");

      Checked_Execute (T, T.Find_By_Name_Statement.all); --  So Next is valid.
   end Initialize;

   procedure Insert
     (T     : in out Table;
      Title : in     String)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;

      Statement : constant String := "INSERT INTO Series (Title) VALUES (?)";
   begin
      Checked_Execute (T, Statement, Params => (1 => +new String'(Title)));
      Find (T, Title);
   end Insert;

   procedure Update
     (T     : in out Table;
      Title : in     String)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;

      Statement : constant String := "UPDATE Series SET Title = ? WHERE ID = ?";
   begin
      Checked_Execute (T, Statement, Params => (+new String'(Title), +ID (T)));
   end Update;

end Books.Database.Data_Tables.Series;
