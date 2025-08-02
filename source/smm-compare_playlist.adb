--  Abstract :
--
--  See spec.
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
package body SMM.Compare_Playlist is

   overriding
   function "=" (Left, Right : in Song_Names) return Boolean
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return
        Left.Album_Artist = Right.Album_Artist and
        Left.Album = Right.Album and
        Left.Title = Right.Title;
   end "=";

   Null_Song_Names : constant Song_Names := (others => Ada.Strings.Unbounded.Null_Unbounded_String);

   function Image (Item : in Song_Names) return String
   is begin
      return -Item.Album_Artist & ", " & (-Item.Album) & ", " & (-Item.Title);
   end Image;

   function Image (Item : in SMM.Database.Cursor) return String
   is begin
      return Item.Album_Artist & ", " & Item.Album & ", " & Item.Title;
   end Image;

   function DB_Find (Item : in Song_Names) return SMM.Database.Cursor
   is
      use SMM.Database;
      I : constant Cursor := Find_Like
        (DB,
         Param           =>
           (Album_Artist => Item.Album_Artist,
            Album        => Item.Album,
            Title        => Item.Title,
            others       => Ada.Strings.Unbounded.Null_Unbounded_String),
         Order_By        => (1 => Album_Artist));
   begin
      if not I.Has_Element then
         raise SAL.Not_Found with "'" & Image (Item) & "' not found in DB";
      end if;

      return I;
      --  We check for more than one item in I later (which is why we return
      --  a cursor).
   end DB_Find;

end SMM.Compare_Playlist;
