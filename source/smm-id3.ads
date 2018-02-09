--  Abstract :
--
--  Interface to ID3 tags in media files.
--
--  References:
--
--  [1] http://id3.org/d3v2.3.0 ID3 2.3.0 spec
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

with Ada.Finalization;
private with Ada.Streams.Stream_IO;
package SMM.ID3 is

   type File is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (File : in out SMM.ID3.File);
   --  Close file.

   procedure Open (File : in out SMM.ID3.File; Name : in String);
   --  Open for read.

   subtype Tag_String is String (1 .. 4);
   --  See [1] section 4 for tag definitions.

   --  Some common tags:
   Album  : constant Tag_String := "TALB"; -- [1] Album/Movie/Show title
   Artist : constant Tag_String := "TPE1"; -- [1] Lead performer(s)/Soloist(s)
   Title  : constant Tag_String := "TIT2"; -- [1] Title/songname/content description

   function Read (File : in out SMM.ID3.File; Tag : in Tag_String) return String;
   --  Raises SAL.Not_Found if Tag is not found in File.

private
   type File is new Ada.Finalization.Limited_Controlled with record
      Stream : Ada.Streams.Stream_IO.File_Type;
   end record;
end SMM.ID3;
