--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009 Stephen Leake.  All Rights Reserved.
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
package body Playlists is

   procedure Read_Playlist (File_Name : in String; Files : out String_Lists.List)
   is
      File : File_Type;
   begin
      if Verbosity > 1 then
         Put_Line ("processing playlist " & File_Name);
      end if;

      Open (File, In_File, File_Name);

      --  special case; empty file
      if End_Of_Line (File) then
         if End_Of_File (File) then
            Close (File);

            Files := String_Lists.Empty_List;

            return;
         end if;
      end if;

      loop -- exit on End_Error
         declare
            Name : constant String := Get_Line (File);
         begin
            String_Lists.Append (Files, Ada.Characters.Handling.To_Lower (Name));
         end;
      end loop;
   exception
   when End_Error =>
      Close (File);
   end Read_Playlist;

end Playlists;
