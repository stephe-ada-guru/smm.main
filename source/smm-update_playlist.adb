--  Abstract :
--
--  Update a playlist file
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

with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;
with SMM.Database;
with SMM.Song_Lists;
procedure SMM.Update_Playlist
  (Db                 : in SMM.Database.Database;
   Playlist_File_Name : in String;
   Category           : in String;
   Count              : in Ada.Containers.Count_Type;
   New_Song_Count     : in Ada.Containers.Count_Type;
   Over_Select_Ratio  : in Float;
   Replace            : in Boolean)
is
   Songs         : SMM.Song_Lists.Song_Lists.List;
   Playlist_File : Ada.Text_IO.File_Type;
begin
   declare
      use Ada.Directories, Ada.Text_IO;
   begin
      if Exists (Playlist_File_Name) then
         if Replace then
            Delete_File (Playlist_File_Name);
            Create (Playlist_File, Out_File, Playlist_File_Name);
         else
            Open (Playlist_File, Append_File, Playlist_File_Name);
         end if;
      else
         Create (Playlist_File, Out_File, Playlist_File_Name);
      end if;
   end;

   SMM.Song_Lists.Least_Recent_Songs
     (DB, Category, Songs,
      Song_Count        => Count,
      New_Song_Count    => New_Song_Count,
      Over_Select_Ratio => Over_Select_Ratio);

   for I of Songs loop
      declare
         Cur : constant SMM.Database.Cursor := DB.Find_ID (I);
      begin
         if Cur.Has_Element then
            Ada.Text_IO.Put_Line (Playlist_File, Cur.File_Name);
            Cur.Write_Last_Downloaded (DB, SMM.Database.UTC_Image (Ada.Calendar.Clock));
         else
            --  Must be a bad play before/after link. Not clear how to print a
            --  helpful message here; we don't know what song contains the bad
            --  link.
            if Verbosity >= 1 then
               Ada.Text_IO.Put_Line ("bad before/ater link:" & I'Image);
            end if;
         end if;
      end;
   end loop;
   Ada.Text_IO.Close (Playlist_File);
end SMM.Update_Playlist;
