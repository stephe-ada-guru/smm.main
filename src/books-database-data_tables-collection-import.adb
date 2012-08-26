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
with Books.Database.Link_Tables;
with Books.Import; use Books.Import;
with SAL.CSV;      use SAL.CSV;
package body Books.Database.Data_Tables.Collection.Import is

   procedure Read_Insert_Find (File : in out SAL.CSV.File_Type)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;
      Old_ID          : constant Integer        := Read (File, 1);
      Title           : aliased constant String := Unquote (Read (File, 2));
      Year            : Integer;
      Year_Valid      : Boolean;
      Editor_ID       : Integer;
      Editor_ID_Valid : Boolean;
   begin
      Read (File, 3, Editor_ID, Editor_ID_Valid);
      Read (File, 4, Year, Year_Valid);

      begin
         Collection_Table.Insert (Title, Year, Year_Valid);
      exception
      when Entry_Error =>
         --  Presumably a duplicate name; find it, map a second old id to it

         --  GNATCOLL.Fetch outputs a nice error message for the failed insert
         null;
      end;

      --  We don't use Last_ID, because we might be looking for the
      --  previous ID with the same name.

      if Year_Valid then
         Find
           (Collection_Table.all,
            "SELECT ID, Title, Year FROM Collection WHERE Title = ? and Year = ?",
            (+Title'Unchecked_Access, +Year));
      else
         Find
           (Collection_Table.all,
            "SELECT ID, Title, Year FROM Collection WHERE Title = ?",
            (1 => +Title'Unchecked_Access));
      end if;

      if Collection_Table.Valid then
         --  At this point, either the newly inserted, or another matching, record is current
         Collection_ID_Map.Add ((Old_ID, Collection_Table.ID));
      else
         --  Some other error occurred; one of the above statements
         --  should have raised an exception that was not handled.
         --  But just in case:
         raise SAL.Programmer_Error with Integer'Image (Old_ID) & ", '" &
           Title & "' not found in Collections";
      end if;

      if Editor_ID_Valid then
         --  WORKAROUND: GNAT GPL 2012 says Link_Tables is unused if we use Links().Insert
         Books.Database.Link_Tables.Insert
           (Links (Author, Books.Collection).all, (Author_ID_Map.Find (Editor_ID).New_ID, Collection_Table.ID));
      else
         Ada.Text_IO.Put_Line (Title & " has no editor");
      end if;
   end Read_Insert_Find;

end Books.Database.Data_Tables.Collection.Import;
