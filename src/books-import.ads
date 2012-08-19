--  Abstract :
--
--  Base package; import CSV data into Stephe's books database.
--
--  Copyright (C) 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Books.Database.Data_Tables.Author;
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
with SAL.Aux.Definite_Private_Items;
with SAL.CSV;
with SAL.Poly.Binary_Trees.Sorted;
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

   type ID_Map_Item is record
      Old_ID : Database.ID_Type;
      New_ID : Database.ID_Type;
   end record;

   function To_Key (Item : in ID_Map_Item) return Database.ID_Type is (Item.Old_ID);

   package ID_Map_Aux is new SAL.Aux.Definite_Private_Items (ID_Map_Item);
   package ID_Maps is new SAL.Poly.Binary_Trees.sorted
     (Item_Type         => ID_Map_Item,
      Item_Node_Type    => ID_Map_Item,
      To_Item_Node      => ID_Map_Aux.To_Item_Node,
      Free_Item         => ID_Map_Aux.Free_Item,
      Key_Type          => Database.ID_Type,
      To_Key            => To_Key,
      Is_Greater        => ">",
      Is_Equal          => "=",
      Node_Storage_Pool => Database.Database_Access'Storage_Pool);

   Author_ID_Map     : ID_Maps.Tree_Type;
   Collection_ID_Map : ID_Maps.Tree_Type;
   Series_ID_Map     : ID_Maps.Tree_Type;
   Title_ID_Map      : ID_Maps.Tree_Type;

   Author_Table     : Books.Database.Data_Tables.Author.Table_Access;
   Collection_Table : Books.Database.Data_Tables.Collection.Table_Access;
   Series_Table     : Books.Database.Data_Tables.Series.Table_Access;
   Title_Table      : Books.Database.Data_Tables.Title.Table_Access;
   Links            : Link_Arrays;

end Books.Import;
