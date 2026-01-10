--  Abstract :
--
--  Root of Stephe's Music Manager packages
--
--  Copyright (C) 2008 - 2018, 2025, 2026 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with SAL.Gen_Trimmed_Image;
package SMM is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Verbosity  : Integer := 0;
   Max_Errors : Integer := 0;

   DB_File_Name : constant String := "/var/www/html/music_server_data/smm.db";

   subtype Time_String is String (1 .. 19);
   --  UTC time in 'YYYY-MM-DD HH:MM:SS' format

   Jan_1_1958 : constant Time_String := "1958-01-01 00:00:00";
   --  A time before any valid database time, used for a default time in
   --  various places.

   Default_Time_String : Time_String renames Jan_1_1958;

   subtype Song_ID is Integer;
   Invalid_Song_ID : constant Song_ID := -1;
   Invalid_Song_ID_JSON : constant GNATCOLL.JSON.JSON_Value;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Song_ID);

   package ID_Lists is new Ada.Containers.Doubly_Linked_Lists (Song_ID);

   function Max_ID (List : in ID_Lists.List) return Song_ID;

   function To_JSON (List : in ID_Lists.List) return GNATCOLL.JSON.JSON_Array;
   --  Return array containing contents of List.

   function To_List (List : in GNATCOLL.JSON.JSON_Array) return ID_Lists.List;

   function Normalize (Path : in String) return String;
   --  convert '\' to '/'

   function Relative_Name (Root : in String; Full_Name : in String) return String;
   --  If Full_Name starts with Root, return relative part. Otherwise return Full_Name.

   function As_Directory (Path : in String) return String;
   --  normalize, append '/' if needed.

   function As_File (Path : in String) return String;
   --  delete trailing '/' if needed.

   type Song_Name is record
      Album_Artist : Ada.Strings.Unbounded.Unbounded_String;
      Album        : Ada.Strings.Unbounded.Unbounded_String;
      Title        : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   function "=" (Left, Right : in Song_Name) return Boolean;
   function Song_Name_Compare (Left, Right : in Song_Name) return SAL.Compare_Result;
   --  Compare is case insensitive.

   Null_Song_Name : constant Song_Name := (others => Ada.Strings.Unbounded.Null_Unbounded_String);

   function Is_Null (Item : in Song_Name) return Boolean;
   --  True if Item is Null_Song_Name

   function Image (Item : in Song_Name) return String;

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

   Albumart_Extensions : constant array (Natural range <>) of Ada.Strings.Unbounded.Unbounded_String :=
     (+"jpg", +"png", +"webp");

   Meta_File_Patterns : constant array (Natural range <>) of Ada.Strings.Unbounded.Unbounded_String :=
     (+"*.jpg", +"*.png", +"*.webp");

private

   Invalid_Song_ID_JSON : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Read ("{""ID"":-1}");

end SMM;
