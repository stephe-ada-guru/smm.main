--  Abstract :
--
--  Stephe's Music Manager command line show id
--
--  Copyright (C) 2025 Stephen Leake All Rights Reserved.
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

with SMM.Database;
with Ada.Text_IO; use Ada.Text_IO;
procedure SMM.Show (ID : in SMM.Song_ID)
is
   use SMM.Database;
   DB : SMM.Database.Database;
   I  : Cursor;

   procedure Search_Result
   is
   begin
      Put_Line (I.ID'Image & ", " & I.File_Name);
      Put_Line (I.Album_Artist & " | " & I.Album & " | " & I.Title);
      Put_Line (I.Artist);
      Put_Line (I.Category);
      New_Line;
   end Search_Result;

begin
   DB.Open (DB_File_Name);
   I := Find_ID (DB, ID);
   if not I.Has_Element then
      Put_Line (ID'Image & " not found");
   else
      Search_Result;
   end if;
end SMM.Show;
