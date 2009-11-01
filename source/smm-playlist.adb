--  Abstract :
--
--  create a playlist of least-recenty heard songs
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Directories;
with Ada.Real_Time;
with Ada.Text_IO;
with SAL.Time_Conversions;
procedure SMM.Playlist
  (Db             : in out SAL.Config_Files.Configuration_Type;
   Category       : in String;
   Destination    : in String;
   Replace        : in Boolean;
   Max_Song_Count : in Integer)
is
   use Song_Lists;

   Playlist_File : Ada.Text_IO.File_Type;

   Songs : List_Type;

   I           : Iterator_Type;
   Source_Root : constant String := SAL.Config_Files.Read (Db, Root_Key);
   Relative    : Boolean;

   Output_Time : constant String := SAL.Time_Conversions.Time_Type'Image
     (SAL.Time_Conversions.To_Time (Ada.Real_Time.Clock));
begin
   declare
      use Ada.Directories;
      use Ada.Text_IO;
      Mode : File_Mode;
   begin
      if Replace then
         Mode := Out_File;
      else
         Mode := Append_File;
      end if;

      if Exists (Destination) then
         Open (Playlist_File, Mode, Destination);
      else
         Create (Playlist_File, Out_File, Destination);
      end if;

      Relative := Containing_Directory (Destination) = Full_Name (Source_Root);
   end;

   Least_Recent_Songs (Db, Category, Song_Count => Max_Song_Count, Songs => Songs);

   I := First (Songs);
   loop
      exit when Is_Null (I);
      declare
         Source : constant String := SAL.Config_Files.Read (Db, Current (I), File_Key);
      begin
         if Relative then
            Ada.Text_IO.Put_Line (Playlist_File, Source);
         else
            Ada.Text_IO.Put_Line (Playlist_File, Source_Root & Source);
         end if;

         SAL.Config_Files.Write (Db, Current (I), Last_Downloaded_Key, Output_Time);

         Next (I);
      end;
   end loop;

   Ada.Text_IO.Close (Playlist_File);
exception
when others =>
   if Ada.Text_IO.Is_Open (Playlist_File) then
      Ada.Text_IO.Close (Playlist_File);
   end if;
   raise;
end SMM.Playlist;
