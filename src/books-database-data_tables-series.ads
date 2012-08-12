--  Abstract :
--
--  Operations on the Series table
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
--

package Books.Database.Data_Tables.Series is

   use type GNATCOLL.SQL.Exec.Field_Index;

   type Table (DB : access Database'Class) is new Data_Tables.Table (DB => DB) with null record;
   type Table_Access is access all Table;

   overriding procedure Initialize (T : in out Table);

   Title_Index : constant GNATCOLL.SQL.Exec.Field_Index := ID_Index + 1;

   procedure Insert
     (T     : in out Table;
      Title : in     String);
   --  Insert a new record, fetch it using Find.

   procedure Update
     (T     : in out Table;
      Title : in     String);
   --  Update the data in the current record.

end Books.Database.Data_Tables.Series;
