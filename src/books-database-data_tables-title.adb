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

pragma License (Gpl);

package body Books.Database.Data_Tables.Title is

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding procedure Initialize (T : in out Table)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;
   begin
      T.All_By_ID_Statement := new String'("SELECT ID, Title, Year, Comment, Rating FROM Title ORDER BY ID");

      T.Find_By_ID_Statement := new String'("SELECT ID, Title, Year, Comment, Rating FROM Title WHERE ID = ?");

      T.Find_By_Name_Statement := new String'
        ("SELECT ID, Title, Year, Comment, Rating FROM Title WHERE Title LIKE ? ORDER BY Title");

      T.Delete_By_ID_Statement := new String'("DELETE FROM Title WHERE ID = ?");

      T.Find_Statement := T.Find_By_Name_Statement;

      Checked_Execute (T, T.Find_Statement.all, Params => (1 => +ID (T))); --  So Next is valid.

   end Initialize;

   procedure Insert
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Integer;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Integer;
      Rating_Valid : in     Boolean)
   is
      use GNATCOLL.SQL.Exec;

      Statement : constant String := "INSERT INTO Title (Title, Year, Comment, Rating) VALUES (?, ?, ?, ?)";
   begin
      Checked_Execute
        (T,
         Statement,
         Params =>
           (1 => +new String'(Title),
            2 => (if Year_Valid then +Year else Null_Parameter),
            3 => (if Comment'Length > 0 then +new String'(Comment) else Null_Parameter),
            4 => (if Rating_Valid then +Rating else Null_Parameter)));

      Find (T, Title);
   end Insert;

   procedure Update
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Integer;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Integer;
      Rating_Valid : in     Boolean)
   is
      use GNATCOLL.SQL.Exec;

      Statement : constant String := "UPDATE Title SET Title = ?, Year = ?, Comment = ?, Rating = ? WHERE ID = ?";
   begin
      Checked_Execute
        (T,
         Statement,
         Params =>
           (1 => +new String'(Title),
            2 => (if Year_Valid then +Year else Null_Parameter),
            3 => (if Comment'Length > 0 then +new String'(Comment) else Null_Parameter),
            4 => (if Rating_Valid then +Rating else Null_Parameter)));
   end Update;

end Books.Database.Data_Tables.Title;
