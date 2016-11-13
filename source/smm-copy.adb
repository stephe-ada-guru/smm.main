--  Abstract :
--
--  See below
--
--  Copyright (C) 2013, 2016 Stephen Leake.  All Rights Reserved.
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

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;
procedure SMM.Copy
  (Playlist    : in String;
   Destination : in String)
is
   use Ada.Directories;
   use Ada.Text_IO;

   Source_Playlist_Dir : constant String := Containing_Directory (Playlist);
   Target_Playlist     : constant String := Compose (Destination, Simple_Name (Playlist));
   Target_Name         : constant String := Base_Name (Playlist);
   Root_Target_Dir     : constant String := As_Directory (Compose (Destination, Target_Name));
   Root_Dir            : constant String := Current_Directory;

   Source_Playlist_File : File_Type;
   Target_Playlist_File : File_Type;
begin
   if not Exists (Root_Target_Dir) then
      Create_Directory (Root_Target_Dir);
   end if;
   if Exists (Target_Playlist) then
      Delete_File (Target_Playlist);
   end if;

   Open (Source_Playlist_File, In_File, Playlist);
   Create (Target_Playlist_File, Out_File, Target_Playlist);
   loop
      exit when End_Of_File (Source_Playlist_File);
      declare
         Source      : constant String := Get_Line (Source_Playlist_File);
         Source_Full : constant String := Full_Name (Source_Playlist_Dir & "/" & Source);
         --  Full_Name normalizes relative paths.
         Relative    : constant String := SMM.Relative_Name (Root_Dir, Normalize (Source_Full));
         Target      : constant String := Root_Target_Dir & Relative;
         Target_Dir  : constant String := Containing_Directory (Target);
      begin
         if not Exists (Target_Dir) then
            begin
               Create_Path (Target_Dir);
            exception
            when Ada.IO_Exceptions.Use_Error =>
               Put_Line ("can't create directory " & Target_Dir);
            end;
         end if;

         if Verbosity > 0 then
            Put_Line (Relative); -- warm fuzzy for user
         end if;

         Copy_File (Source_Full, Target);
         Put_Line (Target_Playlist_File, Normalize (Target_Name & "/" & Relative));
      end;
   end loop;
   Close (Source_Playlist_File);
   Close (Target_Playlist_File);
end SMM.Copy;
