--  Abstract :
--
--  Root of link tables package tree.
--
--  Copyright (C) 2002, 2012 Stephen Leake.  All Rights Reserved.
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

package Books.Database.Link_Tables is

   subtype Link_Index is GNATCOLL.SQL.Exec.Field_Index range 1 .. 2;
   type Link_Names is array (Link_Index) of Table_Names;

   type Table
     (Link_Names : access constant Link_Tables.Link_Names;
      DB         : access Database'Class)
   is new Books.Database.Table with private;
   type Table_Access is access all Table;
   --  Names gives the names of the columns of the link table; concatenation is
   --  the name of the link table.

   type Link_Array_ID_Type is array (Link_Index) of aliased ID_Type;

   overriding procedure Initialize (T : in out Table);

   function ID (T : in Table; Name : in Table_Names) return ID_Type;
   --  Retrieve data from current record

   procedure Delete (T : in out Table; Data : in Link_Array_ID_Type);
   --  Delete record containing Data.

   procedure Find (T : in out Table; Name : in Table_Names; Item : in ID_Type);
   --  Name must be one of Link_Names; Programmer_Error is raised if not.
   --  Find records with ID (Name) = Item.

   procedure Insert (T : in out Table; Data : in Link_Array_ID_Type);
   --  Insert a link record containing Data.

private

   type Link_Array_Statement_Type is array (Link_Index) of aliased access constant String;

   type Table
     (Link_Names : access constant Link_Tables.Link_Names;
      DB         : access Database'Class)
   is new Books.Database.Table (DB => DB) with record
      Find_By_Link_Statement : Link_Array_Statement_Type;
   end record;

end Books.Database.Link_Tables;
