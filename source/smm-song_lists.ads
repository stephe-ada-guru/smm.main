--  Abstract :
--
--  Types and operations for lists of songs.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Containers.Doubly_Linked_Lists;
with SAL.Gen_Randomize_Doubly_Linked_Lists;
with SMM.Database;
package SMM.Song_Lists is

   package Song_Lists is new Ada.Containers.Doubly_Linked_Lists (Integer);
   --  We don't store a Database.Cursor in the list, because copying a
   --  GNATCOLL.Sql cursor only copies a reference to the actual cursor,
   --  so it can point to a different element than the one we want.
   --  So we store the element ID instead.

   procedure Randomize is new SAL.Gen_Randomize_Doubly_Linked_Lists (Song_Lists);

   procedure Least_Recent_Songs
     (DB                : in     SMM.Database.Database;
      Category          : in     String;
      Songs             :    out Song_Lists.List;
      Song_Count        : in     Ada.Containers.Count_Type;
      New_Song_Count    : in     Ada.Containers.Count_Type;
      Over_Select_Ratio : in     Float;
      Seed              : in     Integer := 0);
   --  Return randomized list of Song_Count least-recently downloaded
   --  songs in Category. If any Songs have .Play_Before attribute,
   --  enforce it.
   --
   --  If Seed is non-zero, it is used to initialize the randomizer
   --  (for repeatable test results). Otherwise initialized per Ada
   --  LRM.

   procedure Play_Before
     (DB    : in     SMM.Database.Database;
      Songs : in out Song_Lists.List);
   --  If any Songs have .Play_Before attribute, enforce it.

   ----------
   --  Time lists

   type Time_List_Element_Type is record
      Last_Downloaded : SMM.Database.Time_String;
      Songs           : Song_Lists.List;
   end record;

   function Is_Equal (Left : in Time_List_Element_Type; Right : in Time_List_Element_Type) return Boolean is
      (Left.Last_Downloaded = Right.Last_Downloaded);

   function Is_Less (Left : in Time_List_Element_Type; Right : in Time_List_Element_Type) return Boolean is
      (Left.Last_Downloaded < Right.Last_Downloaded);

   package Time_Lists is new Ada.Containers.Doubly_Linked_Lists (Time_List_Element_Type, Is_Equal);

   package Time_Lists_Sorting is new Time_Lists.Generic_Sorting (Is_Less);

   procedure Insert
     (DB   : in     SMM.Database.Database;
      Item : in     Integer;
      List : in out Time_Lists.List);
   --  Insert Item into List.

end SMM.Song_Lists;
