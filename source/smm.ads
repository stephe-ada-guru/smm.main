--  Abstract :
--
--  Root of Stephe's Music Manager packages
--
--  Copyright (C) 2008 - 2016 Stephen Leake.  All Rights Reserved.
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with SAL.Config_Files;
with SAL.Time_Conversions;
package SMM is

   Verbosity : Integer;
   Debug     : Boolean; -- if true, don't change disk, just show what would be done

   Playlist_Error : exception;

   function Find_Home return String;

   function Normalize (Path : in String) return String;
   --  convert '\' to '/'

   function Relative_Name (Root : in String; Full_Name : in String) return String;

   function As_Directory (Path : in String) return String;
   --  normalize, append '/' if needed.

   function As_File (Path : in String) return String;
   --  delete trailing '/' if needed.

   function To_String (Time : in SAL.Time_Conversions.Time_Type) return String;
   --  User friendly date representation

   --  database keys
   Category_Key        : constant String := "Category";
   File_Key            : constant String := "File";
   --  Last_Downloaded_Key : use Read_Last_Downloaded
   --  Prev_Downloaded_Key : use Read_Prev_Downloaded
   Songs_Key           : constant String := "Songs";
   Root_Key            : constant String := "Root";
   Playlist_Key        : constant String := "Playlists";

   function Read_Last_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type;

   function Read_Prev_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type;

   procedure Write_Last_Downloaded
     (Db   : in out SAL.Config_Files.Configuration_Type;
      I    : in     SAL.Config_Files.Iterator_Type;
      Time : in     SAL.Time_Conversions.Time_Type);

   procedure Write_Last_Downloaded
     (Db       : in out SAL.Config_Files.Configuration_Type;
      Root_Key : in     String;
      Time     : in     SAL.Time_Conversions.Time_Type);

   package Song_Lists is new Ada.Containers.Doubly_Linked_Lists (SAL.Config_Files.Iterator_Type, SAL.Config_Files."=");

   procedure Least_Recent_Songs
     (Db             : in     SAL.Config_Files.Configuration_Type;
      Category       : in     String;
      Songs          :    out Song_Lists.List;
      Song_Count     : in     Ada.Containers.Count_Type;
      New_Song_Count : in     Ada.Containers.Count_Type;
      Seed           : in     Integer                             := 0);
   --  Return randomized list of Song_Count least-recently downloaded
   --  songs in Category. If any Songs have .Play_Before attribute,
   --  enforce it.
   --
   --  If Seed is non-zero, it is used to initialize the randomizer
   --  (for repeatable test results). Otherwise initialized per Ada
   --  LRM.

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

   procedure Play_Before
     (Db    : in     SAL.Config_Files.Configuration_Type;
      Songs : in out Song_Lists.List);
   --  If any Songs have .Play_Before attribute, enforce it.

end SMM;
