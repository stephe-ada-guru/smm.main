--  Abstract :
--
--  Generic link database table
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

generic
   type Source_Labels_Type is (<>);
   --  Must have two values, whose images are the names of the columns
   --  of the link table, and whose concatenation is the name of the
   --  link table.
package Books.Database.Gen_Link_Tables is

   type Table is new Books.Database.Table with private;
   type Table_Access is access all Table;

   type Source_Array_ID_Type is array (Source_Labels_Type) of aliased ID_Type;

   ----------
   --  Override parent operations.

   overriding procedure Initialize (T : in out Table);

   ----------
   --  New operations

   function ID (T : in Table; Source : in Source_Labels_Type) return ID_Type;
   --  Retrieve data from current record

   procedure Delete (T : in out Table; Data : in Source_Array_ID_Type);
   --  Delete record containing Data.

   procedure Find (T : in out Table; Source : in Source_Labels_Type; Item : in ID_Type);
   --  Find records with ID (Source) = Item.

   procedure Insert (T : in out Table; Data : in Source_Array_ID_Type);
   --  Insert a link record containing Data.

private

   type Source_Array_Statement_Type is array (Source_Labels_Type) of aliased access constant String;

   type Table is new Books.Database.Table with record
      Find_By_Source_Statement : Source_Array_Statement_Type;
   end record;

end Books.Database.Gen_Link_Tables;
