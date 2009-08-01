--  Abstract :
--
--  Root of Playlists application
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
package Playlists is
   pragma Elaborate_Body; --  Ada.Text_IO
   Verbosity : Integer := 0;

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   procedure Read_Playlist (File_Name : in String; Files : out String_Lists.List);
   --  Read playlist File_name, build list of files in it.

end Playlists;
