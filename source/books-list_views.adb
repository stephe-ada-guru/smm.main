--  Abstract :
--
--  Base database list view widget for Books application.
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
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

with Books.Database.Data_Tables;
package body Books.List_Views is

   function Field
     (Table : in Books.Database.Data_Tables.Table_Access;
      Index : in GNATCOLL.SQL.Exec.Field_Index)
      return Interfaces.C.Strings.chars_ptr
   is begin
      if Table.Valid_Field (Index) then
         return Interfaces.C.Strings.New_String (Table.Field (Index));
      else
         return Interfaces.C.Strings.Null_Ptr;
      end if;
   end Field;

end Books.List_Views;
