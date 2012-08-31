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

package body Books.Database.Data_Tables.Collection is

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding procedure Initialize (T : in out Table)
   is begin
      --  from Database.Data_Tables
      T.Find_By_ID_Statement := new String'("SELECT ID, Title, Year FROM Collection WHERE ID = ?");

      T.Find_By_Name_Statement := new String'
        ("SELECT ID, Title, Year FROM Collection WHERE Title LIKE ? ORDER BY Title");

   end Initialize;

   procedure Insert
     (T          : in out Table;
      Title      : in     String;
      Year       : in     Natural;
      Year_Valid : in     Boolean)
   is
      use GNATCOLL.SQL.Exec;
   begin
      if Year_Valid then
         Checked_Execute
           (T,
            "INSERT INTO Collection (Title, Year) VALUES (?, ?)",
            Params =>
              (1 => +new String'(Title),
               2 => +Year));
      else
         Checked_Execute
           (T,
            "INSERT INTO Collection (Title) VALUES (?)",
            Params =>
              (1 => +new String'(Title)));
      end if;

   end Insert;

   procedure Update
     (T          : in out Table;
      Title      : in     String;
      Year       : in     Natural;
      Year_Valid : in     Boolean)
   is
      use GNATCOLL.SQL.Exec;

      Statement : constant String := "UPDATE Collection SET Title = ?, Year = ? WHERE ID = ?";
   begin
      Checked_Execute
        (T,
         Statement,
         Params =>
           (1 => +new String'(Title),
            2 => (if Year_Valid then +Year else Null_Parameter),
            3 => +ID (T)));
   end Update;

end Books.Database.Data_Tables.Collection;
