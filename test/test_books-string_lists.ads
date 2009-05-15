--  Abstract :
--
--  List of list of strings, for testing GUI list displays.
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Glib;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Poly.Lists.Double;
with System.Storage_Pools;
package Test_Books.String_Lists is

   type String_List_Type is array (Glib.Gint range <>) of Ada.Strings.Unbounded.Unbounded_String;
   type String_List_Access_Type is access String_List_Type;

   package String_List_Aux is new SAL.Aux.Indefinite_Private_Items (String_List_Type, String_List_Access_Type);
   package String_Tables is new SAL.Poly.Lists.Double
     (Item_Type         => String_List_Type,
      Item_Node_Type    => String_List_Access_Type,
      To_Item_Node      => String_List_Aux.To_Item_Node,
      Free_Item         => String_List_Aux.Free_Item,
      Copy              => String_List_Aux.Copy,
      Node_Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class
        (Ada.Strings.Unbounded.String_Access'Storage_Pool));

   subtype String_Table_Type is String_Tables.List_Type;
   function "+" (Right : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Right : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;
   function "+" (Right : in String) return String_List_Type;
   function "+" (Right : in String_List_Type) return String_Table_Type;

   procedure Check_List (Computed, Expected : in String_Table_Type);
   --  Does AUnit.Assertions.Assert on each element of computed,
   --  expected. Also checks that all elements of Expected are
   --  checked. String_Lists must be 0 indexed, since Clists are.

end Test_Books.String_Lists;
