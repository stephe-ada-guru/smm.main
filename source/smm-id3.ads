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
--  Copyright (C) 2018 - 2020, 2022 Stephen Leake All Rights Reserved.
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

with SMM.Metadata;
private with Ada.Streams.Stream_IO;
private with Interfaces;
package SMM.ID3 is

   Ignore_Flags : Boolean := False;
   --  By default, ID3 file and frame header flags are checked for
   --  settings we nominally don't support. Setting Ignore_Flags True
   --  ignores those checks, which sometimes lets us read the frames.

   function All_Frames (File : in SMM.Metadata.File) return SMM.Metadata.Frame_Lists.List;
   --  FIXME: make private

   procedure Metadata
     (Abs_File_Name : in     String;
      ID3_Frames    :    out SMM.Metadata.Frame_Lists.List;
      Artist_ID     :    out SMM.Metadata.ID_String);

   procedure Create
     (Name    : in String;
      Content : in SMM.Metadata.Frame_Lists.List);
   --  Create a file with Content. Mostly useful for unit tests.

private

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
