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

with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SMM.Database;
package SMM.Compare_Playlist is

   function Identity (Item : in Song_Name) return Song_Name is (Item);

   function Image (Item : in SMM.Database.Cursor) return String;

   package Song_Name_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Song_Name,
      Key_Type     => Song_Name,
      Key          => Identity,
      Key_Compare  => Song_Name_Compare);

   function Song_ID_Compare is new SAL.Gen_Compare_Integer (SMM.Database.Song_ID);

   procedure Compare_To_DB
     (DB           : in SMM.Database.Database;
      DB_Missing   : in Song_Name_Trees.Tree;
      Category     : in String;
      Tree         : in Song_Name_Trees.Tree;
      Tree_Name    : in String;
      Tree_Missing : in Song_Name_Trees.Tree);
   --  Report missing/different on standard output.

end SMM.Compare_Playlist;
