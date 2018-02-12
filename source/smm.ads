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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with SAL.Config_Files;
package SMM is

   Verbosity : Integer;
   Debug     : Boolean; -- if true, don't change disk, just show what would be done

   Playlist_Error : exception;

   function Find_Home return String;

   function Normalize (Path : in String) return String;
   --  convert '\' to '/'

   function Relative_Name (Root : in String; Full_Name : in String) return String;
   --  Ensure Root is As_Directory.

   function As_Directory (Path : in String) return String;
   --  normalize, append '/' if needed.

   function As_File (Path : in String) return String;
   --  delete trailing '/' if needed.

   --  database keys
   Category_Key        : constant String := "Category";
   File_Key            : constant String := "File";
   Last_Downloaded_Key : constant String := "Last_Downloaded";
   Prev_Downloaded_Key : constant String := "Prev_Downloaded";
   Play_After_Key      : constant String := "Play_After";
   Play_Before_Key     : constant String := "Play_Before";

   Songs_Key       : constant String := "Songs";
   Root_Key        : constant String := "Root";
   Playlist_Key    : constant String := "Playlists";

   function Find
     (Db       : in SAL.Config_Files.Configuration_Type;
      Filename : in String)
     return SAL.Config_Files.Iterator_Type;
   --  Filename is Artist/Album/title.mp3; file name relative to music root.

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Standard.String,
      Element_Type    => SAL.Config_Files.Iterator_Type,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=",
      "="             => SAL.Config_Files."=");

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
