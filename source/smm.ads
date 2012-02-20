--  Abstract :
--
--  Root of Stephe's Music Manager packages
--
--  Copyright (C) 2008 - 2012 Stephen Leake.  All Rights Reserved.
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with SAL.Aux.Definite_Private_Items;
with SAL.Config_Files;
with SAL.Gen.Alg.Count;
with SAL.Poly.Lists.Double;
with SAL.Storage_Pools;
with SAL.Time_Conversions;
package SMM is

   Verbosity : Integer;
   Debug     : Boolean; -- if true, don't change disk, just show what would be done

   Playlist_Error : exception;

   function Normalize (Path : in String) return String;
   --  convert '\' to '/'

   function Relative_Name (Root : in String; Full_Name : in String) return String;

   function As_Directory (Path : in String) return String;
   --  normalize, append '/' if needed.

   --  database keys
   Category_Key        : constant String := "Category";
   File_Key            : constant String := "File";
   --  Last_Downloaded_Key in body only
   Songs_Key           : constant String := "Songs";
   Root_Key            : constant String := "Root";
   Playlist_Key        : constant String := "Playlists";

   function Read_Last_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type;

   procedure Write_Last_Downloaded
     (Db   : in out SAL.Config_Files.Configuration_Type;
      I    : in     SAL.Config_Files.Iterator_Type;
      Time : in     SAL.Time_Conversions.Time_Type);

   procedure Write_Last_Downloaded
     (Db       : in out SAL.Config_Files.Configuration_Type;
      Root_Key : in     String;
      Time     : in     SAL.Time_Conversions.Time_Type);

   package Song_Lists_Aux is new SAL.Aux.Definite_Private_Items (SAL.Config_Files.Iterator_Type);

   package Song_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => SAL.Config_Files.Iterator_Type,
      Item_Node_Type    => SAL.Config_Files.Iterator_Type,
      To_Item_Node      => Song_Lists_Aux.To_Item_Node,
      Free_Item         => Song_Lists_Aux.Free_Item,
      Copy              => Song_Lists_Aux.Copy_Item_Node,
      Node_Storage_Pool => SAL.Storage_Pools.Integer_Access_Type'Storage_Pool);

   package Song_Lists_Algorithms is new SAL.Gen.Alg
     (Item_Node_Type => SAL.Config_Files.Iterator_Type,
      Container_Type => Song_Lists.List_Type,
      Iterator_Type  => Song_Lists.Iterator_Type,
      Current        => Song_Lists.Current,
      First          => Song_Lists.First,
      Last           => Song_Lists.Last,
      None           => Song_Lists.None,
      Is_Null        => Song_Lists.Is_Null,
      Next_Procedure => Song_Lists.Next,
      Next_Function  => Song_Lists.Next);

   function Count is new Song_Lists_Algorithms.Count;

   procedure Least_Recent_Songs
     (Db         : in     SAL.Config_Files.Configuration_Type;
      Category   : in     String;
      Songs      :    out Song_Lists.List_Type;
      Song_Count : in     Integer;
      Seed       : in     Integer                             := 0);
   --  Return randomized list of Song_Count least-recently downloaded
   --  songs in Category. If any Songs have .Play_Before attribute,
   --  enforce it.

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Standard.String,
      Element_Type    => SAL.Config_Files.Iterator_Type,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=",
      "="             => SAL.Config_Files."=");

   procedure Edit_Playlist
     (Playlist_File_Name : in String;
      Last_File_Name     : in String);
   --  Delete lines from start of playlist file up to and including
   --  line in last file. Delete last file.

   procedure Read_Playlist
     (File_Name  : in     String;
      Files      :    out String_Lists.List);
   --  Read playlist File_name, build list of files (lowercase) in it.

   procedure Play_Before
     (Db    : in     SAL.Config_Files.Configuration_Type;
      Songs : in out Song_Lists.List_Type);
   --  If any Songs have .Play_Before attribute, enforce it.

end SMM;
