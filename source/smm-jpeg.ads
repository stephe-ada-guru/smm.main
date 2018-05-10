--  Abstract :
--
--  Interface to JPEG tags in media files.
--
--  References (actual standard is not open source):
--
--  [1] http://www.faqs.org/faqs/jpeg-faq/part1/
--  [2] http://ijg.org/ rdjpgcom.c
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
package SMM.JPEG is

   type File is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (File : in out SMM.JPEG.File);
   --  Close file.

   procedure Open (File : in out SMM.JPEG.File; Name : in String);
   --  Open for read.

   procedure Close (File : in out SMM.JPEG.File);

   type Size_Type is record
      --  In pixels
      X : Integer;
      Y : Integer;
   end record;

   function Image (Size : in Size_Type) return String;

   function Size (File : in SMM.JPEG.File) return Size_Type;

   function Size (File_Name : in String) return Size_Type;

private

   type File is new Ada.Finalization.Limited_Controlled with record
      Stream : Ada.Streams.Stream_IO.File_Type;
   end record;

end SMM.JPEG;
