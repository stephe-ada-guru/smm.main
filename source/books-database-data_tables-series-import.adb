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
package body Books.Database.Data_Tables.Series.Import is

   procedure Read_Insert_Find (File : in out SAL.CSV.File_Type)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;
      Old_ID          : constant Integer        := Read (File, 1);
      Title           : aliased constant String := Unquote (Read (File, 2));
      Author_ID       : Integer;
      Author_ID_Valid : Boolean;
   begin
      Read (File, 3, Author_ID, Author_ID_Valid);

      begin
         Series_Table.Insert (Title);
      exception
      when Entry_Error =>
         --  see ../books-database-data_tables-author-import for comments
         null;
      end;

      --  Unchecked_Access ok here, because value not needed after subprogram return.
      Find (Series_Table.all, "SELECT ID, Title FROM Series WHERE Title = ?", (1 => +Title'Unchecked_Access));

      if Series_Table.Valid then
         --  At this point, either the newly inserted, or another matching, record is current
         ID_Maps (Books.Series).Add ((Old_ID, Series_Table.ID));
      else
         --  Some other error occurred; one of the above statements
         --  should have raised an exception that was not handled.
         --  But just in case:
         raise SAL.Programmer_Error with Integer'Image (Old_ID) & ", '" & Title & "' not found in Seriess";
      end if;

      if Author_ID_Valid then
         --  WORKAROUND: GNAT GPL 2012 says Link_Tables is unused if we use Links().Insert
         Books.Database.Link_Tables.Insert
           (Links (Author, Books.Series).all, (ID_Maps (Author).Find (Author_ID).New_ID, Series_Table.ID));
      else
         Ada.Text_IO.Put_Line (Title & " has no author");
      end if;
   end Read_Insert_Find;

end Books.Database.Data_Tables.Series.Import;
