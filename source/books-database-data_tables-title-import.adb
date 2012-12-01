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

with Ada.Strings.Unbounded;
with Books.Import; use Books.Import;
with SAL.CSV;      use SAL.CSV;
package body Books.Database.Data_Tables.Title.Import is

   procedure Read_Insert_Find (File : in out SAL.CSV.File_Type)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;

      Old_ID       : constant Integer        := Read (File, 1);
      Title        : aliased constant String := Unquote (Read (File, 2));
      Year         : Integer;
      Year_Valid   : Boolean;
      Comment      : aliased constant String := Unquote (Read (File, 4));
      Rating       : Integer;
      Rating_Valid : Boolean;

   begin
      Read (File, 3, Year, Year_Valid);
      Read (File, 5, Rating, Rating_Valid);

      begin
         Title_Table.Insert (Title, Year, Year_Valid, Comment, Rating, Rating_Valid);
      exception
      when Entry_Error =>
         --  see ../books-database-data_tables-author-import for comments
         null;
      end;

      declare
         use Ada.Strings.Unbounded;
         use GNATCOLL.SQL.Exec;

         Statement : Unbounded_String := To_Unbounded_String
           ("SELECT ID, Title, Year, Comment, Rating from Title WHERE Title = ?");

         Params     : SQL_Parameters (1 .. 2);
         Last_Param : Positive := 1;
      begin

         --  Unchecked_Access ok here, because value not needed after subprogram return.
         Params (Last_Param) := +Title'Unchecked_Access;

         if Year_Valid then
            Statement := Statement & " and Year = ?";
            Last_Param          := Last_Param + 1;
            Params (Last_Param) := +Year;
         end if;

         Find (Title_Table.all, To_String (Statement), Params (1 .. Last_Param));
      end;

      if Title_Table.Valid then
         --  At this point, either the newly inserted, or another matching, record is current
         ID_Maps (Books.Title).Add ((Old_ID, Title_Table.ID));
      else
         --  Some other error occurred; one of the above statements
         --  should have raised an exception that was not handled.
         --  But just in case:
         raise SAL.Programmer_Error with Integer'Image (Old_ID) & ", '" & Title & "'" & Integer'Image (Year) &
           " not found in Titles";
      end if;
   end Read_Insert_Find;

end Books.Database.Data_Tables.Title.Import;
