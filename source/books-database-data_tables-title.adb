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

with Ada.Strings.Unbounded;
package body Books.Database.Data_Tables.Title is

   ----------
   --  Subprogram bodies (alphabetical order)

   overriding procedure Initialize (T : in out Table)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;
   begin
      T.Find_By_ID_Statement := new String'("SELECT ID, Title, Year, Comment, Rating FROM Title WHERE ID = ?");

      T.Find_By_Name_Statement := new String'
        ("SELECT ID, Title, Year, Comment, Rating FROM Title WHERE Title LIKE ? ORDER BY Title");

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
      use Ada.Strings.Unbounded;
      use GNATCOLL.SQL.Exec;

      Title_1   : aliased constant String := Title;
      Comment_1 : aliased constant String := Comment;

      Statement  : Unbounded_String := To_Unbounded_String ("INSERT INTO Title (Title, Comment");
      Values     : Unbounded_String := To_Unbounded_String (" VALUES (?,?");
      Params     : SQL_Parameters (1 .. 4);
      Last_Param : Positive         := 1;

   begin
      Params (Last_Param) := +Title_1'Unchecked_Access;
      Last_Param          := Last_Param + 1;
      Params (Last_Param) := +Comment_1'Unchecked_Access;

      if Year_Valid then
         Statement           := Statement & ", Year";
         Last_Param          := Last_Param + 1;
         Params (Last_Param) := +Year;

         Values := Values & ",?";
      end if;

      if Rating_Valid then
         Statement           := Statement & ", Rating";
         Last_Param          := Last_Param + 1;
         Params (Last_Param) := +Rating;

         Values := Values & ",?";
      end if;

      Checked_Execute
        (T,
         To_String (Statement) & ")" & To_String (Values) & ")",
         Params (1 .. Last_Param));

      Statement  := To_Unbounded_String ("SELECT ID, Title, Year, Comment, Rating from Title WHERE Title = ?");
      Last_Param := 1;

      if Year_Valid then
         Statement := Statement & " and Year = ?";

         Last_Param          := Last_Param + 1;
         Params (Last_Param) := +Year;
      end if;

      Find (T, To_String (Statement), Params (1 .. Last_Param));

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

      Title_1   : aliased constant String := Title;
      Comment_1 : aliased constant String := Comment;

      Statement : constant String := "UPDATE Title SET Title = ?, Year = ?, Comment = ?, Rating = ? WHERE ID = ?";
   begin
      Checked_Execute
        (T,
         Statement,
         Params =>
           (1 => +Title_1'Unchecked_Access,
            2 => Param (Year_Valid, Year),
            3 => +Comment_1'Unchecked_Access,
            4 => Param (Rating_Valid, Rating),
            5 => +T.ID));

      T.Fetch (T.ID);
   end Update;

end Books.Database.Data_Tables.Title;
