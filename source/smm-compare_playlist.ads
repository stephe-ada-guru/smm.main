--  Abstract :
--
--  Shared stuff for compare lists
--
--  Copyright (C) 2025 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.JSON;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SMM.Database;
package SMM.Compare_Playlist is

   type Song_Name is record
      Album_Artist : Ada.Strings.Unbounded.Unbounded_String;
      Album        : Ada.Strings.Unbounded.Unbounded_String;
      Title        : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   function "=" (Left, Right : in Song_Name) return Boolean;

   Null_Song_Name : constant Song_Name := (others => Ada.Strings.Unbounded.Null_Unbounded_String);

   function Image (Item : in Song_Name) return String;

   function Identity (Item : in Song_Name) return Song_Name is (Item);

   function Image (Item : in SMM.Database.Cursor) return String;

   function DB_Find (Item : in Song_Name) return SMM.Database.Cursor;

   function Song_Name_Compare (Left, Right : in Song_Name) return SAL.Compare_Result;
   --  If a field is blank on one side, it is ignored on the other.

   package Song_Name_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Song_Name,
      Key_Type     => Song_Name,
      Key          => Identity,
      Key_Compare  => Song_Name_Compare);

   function Song_ID_Compare is new SAL.Gen_Compare_Integer (SMM.Database.Song_ID);

   function Get_Album_Artist (Cur : in SMM.DataBase.Cursor) return String;

   procedure Read_DB_Tree (DB : in out SMM.DataBase.Database; DB_Tree : in out Song_Name_Trees.Tree);

   procedure Compare_Trees
     (Left       : in Song_Name_Trees.Tree;
      Left_Name  : in String;
      Right      : in Song_Name_Trees.Tree;
      Right_Name : in String);
   --  Report missing/different on standard output.

end SMM.Compare_Playlist;
