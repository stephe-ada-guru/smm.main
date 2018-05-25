--  Abstract :
--
--  Interface to ID3 tags in media files.
--
--  References:
--
--  [1] http://id3.org/d3v2.3.0            ID3 2.3.0 spec
--  [2] http://id3.org/id3v2.4.0-structure ID3 2.4.0 spec
--  [3] http://id3.org/id3v2.4.0-frames    ID3 2.4.0 frames
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings.Unbounded;
private with Ada.Streams.Stream_IO;
private with Interfaces;
package SMM.ID3 is

   type File is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (File : in out SMM.ID3.File);
   --  Close file.

   procedure Open (File : in out SMM.ID3.File; Name : in String);
   --  Open for read.

   procedure Close (File : in out SMM.ID3.File);

   subtype ID_String is String (1 .. 4);
   --  See [1] section 4, or [3], for frame definitions.

   --  Some common Frames
   Album      : constant ID_String := "TALB"; -- [1] Album/Movie/Show title
   Artist     : constant ID_String := "TPE1"; -- [1] Lead performer(s)/Soloist(s)
   Alt_Artist : constant ID_String := "TPE2"; -- [1] Band/orchestra/accompaniment
   Title      : constant ID_String := "TIT2"; -- [1] Title/songname/content description
   Track      : constant ID_String := "TRCK"; -- [1] Track number/position in a set

   function To_Track (Item : in String) return Integer;
   --  Handle <track>/<total>; 0 for empty string.

   type Frame is record
      ID   : ID_String;
      Data : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Frame_Lists is new Ada.Containers.Doubly_Linked_Lists (Frame);

   function Is_Present (ID : in ID_String; Frames : in Frame_Lists.List) return Boolean
     is (for some F of Frames => ID = F.ID);

   function Find (ID : in ID_String; Frames : in Frame_Lists.List) return Ada.Strings.Unbounded.Unbounded_String;

   function All_Frames (File : in SMM.ID3.File) return Frame_Lists.List;

   procedure Metadata
     (Abs_File_Name : in     String;
      ID3_Frames    :    out Frame_Lists.List;
      Artist_ID     :    out ID_String);

   procedure Create
     (Name    : in String;
      Content : in Frame_Lists.List);
   --  Create a file with Content. Mostly useful for unit tests.

private

   type File is new Ada.Finalization.Limited_Controlled with record
      Stream : Ada.Streams.Stream_IO.File_Type;
   end record;

   ----------
   --  Visible for unit tests

   type Size_Type is record
      Byte_3 : Interfaces.Unsigned_8; -- only lower 7 bits used.
      Byte_2 : Interfaces.Unsigned_8;
      Byte_1 : Interfaces.Unsigned_8;
      Byte_0 : Interfaces.Unsigned_8;
   end record;
   for Size_Type'Size use 4 * 8;

   function Size (Item : in Size_Type) return Ada.Streams.Stream_IO.Count;
   function To_Size (Item : in Ada.Streams.Stream_IO.Count) return Size_Type;

end SMM.ID3;
