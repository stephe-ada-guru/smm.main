--  Abstract :
--
--  Base package; import CSV data into Stephe's books database.
--
--  Copyright (C) 2009, 2012, 2016 Stephen Leake.  All Rights Reserved.
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

with Ada.Containers.Ordered_Maps;
with Books.Database.Data_Tables.Author;
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
with SAL.CSV;
package Books.Import is

   Warm_Fuzzy_Count : Integer := 0;
   --  global to foil optimizer

   procedure Warm_Fuzzy;

   procedure Read
     (File   : in     SAL.CSV.File_Type;
      Column : in     Integer;
      Value  :    out Integer;
      Valid  :    out Boolean);

   function Read
     (File      : in     SAL.CSV.File_Type;
      Column    : in     Integer)
     return Integer;
   --  Raises SAL.Initialization_Error if Column does not hold a valid
   --  integer.

   package ID_Map_Pkg is new Ada.Containers.Ordered_Maps
     (Key_Type     => Database.ID_Type, -- old id
      Element_Type => Database.ID_Type, -- new id
      "<"          => Standard."<",
      "="          => Standard."=");

   type Map_Arrays is array (Table_Names) of ID_Map_Pkg.Map;
   ID_Maps : Map_Arrays;

   Author_Table     : Books.Database.Data_Tables.Author.Table_Access;
   Collection_Table : Books.Database.Data_Tables.Collection.Table_Access;
   Series_Table     : Books.Database.Data_Tables.Series.Table_Access;
   Title_Table      : Books.Database.Data_Tables.Title.Table_Access;
   Links            : Link_Arrays;

end Books.Import;
