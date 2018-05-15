--  Abstract :
--
--  Root of Stephe's Music Manager packages
--
--  Copyright (C) 2008 - 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
package SMM is

   File_Name_Encode_Set : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" &#+;:$,""{}|\^`'");
   --  Mimic Android OkHttp encoding, which does not encode [ ].

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Verbosity : Integer;

   function Find_Home return String;

   function Normalize (Path : in String) return String;
   --  convert '\' to '/'

   function Relative_Name (Root : in String; Full_Name : in String) return String;
   --  If Full_Name starts with Root, return relative part. Otherwise return Full_Name.

   function As_Directory (Path : in String) return String;
   --  normalize, append '/' if needed.

   function As_File (Path : in String) return String;
   --  delete trailing '/' if needed.

   --  Config file keys
   Root_Key : constant String := "Root";

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   procedure Edit_Playlist
     (Playlist_File_Name : in String;
      Last_File_Name     : in String);
   --  Delete lines from start of playlist file up to and including
   --  line in last file. Delete last file.

   procedure Read_Playlist
     (File_Name  : in     String;
      Files      :    out String_Lists.List);
   --  Read playlist File_name, build list of files (lowercase) in it.

end SMM;
