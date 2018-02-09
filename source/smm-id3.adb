--  Abstract :
--
--  See spec.
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

with Ada.IO_Exceptions;
with Interfaces.C;
with Interfaces;
package body SMM.ID3 is

   overriding procedure Finalize (File : in out SMM.ID3.File)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream) then
         Close (File.Stream);
      end if;
   end Finalize;

   procedure Open (File : in out SMM.ID3.File; Name : in String)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream) then
         raise Ada.IO_Exceptions.Use_Error with "file is already open with '" &
           Ada.Streams.Stream_IO.Name (File.Stream) & "'";
      end if;

      Open (File.Stream, In_File, Name);
   end Open;

   type Size_Type is record
      Byte_3 : Interfaces.Unsigned_8;
      Byte_2 : Interfaces.Unsigned_8;
      Byte_1 : Interfaces.Unsigned_8;
      Byte_0 : Interfaces.Unsigned_8;
   end record;
   for Size_Type'Size use 4 * 8;

   function Size (Item : in Size_Type) return Ada.Streams.Stream_IO.Count
   is
      use Ada.Streams.Stream_IO;
   begin
      return
        Count (Item.Byte_3) * 256 ** 3 +
        Count (Item.Byte_2) * 256 ** 2 +
        Count (Item.Byte_1) * 256 +
        Count (Item.Byte_0);
   end Size;

   type File_Header is record
      ID          : Interfaces.C.char_array (1 .. 3);
      Version_Msb : Interfaces.Unsigned_8;
      Version_Lsb : Interfaces.Unsigned_8;
      Flags       : Interfaces.Unsigned_8;
      Size        : Size_Type;
   end record
   with Convention => C;
   File_Header_Byte_Size : constant := 10;
   pragma Pack (File_Header);
   for File_Header'Size use File_Header_Byte_Size * 8;

   type Frame_Header is record
      ID      : Tag_String;
      Size    : Size_Type;
      Flags_1 : Interfaces.Unsigned_8;
      Flags_0 : Interfaces.Unsigned_8;
   end record;
   Frame_Header_Byte_Size : constant := 10;
   for Frame_Header'Size use Frame_Header_Byte_Size * 8;

   function Read (File : in out SMM.ID3.File; Tag : in Tag_String) return String
   is
      use Ada.Streams.Stream_IO;
      use all type Interfaces.C.char_array;
      use all type Interfaces.Unsigned_8;
   begin
      if not Is_Open (File.Stream) then
         raise Ada.IO_Exceptions.Use_Error with "file not open";
      end if;

      Reset (File.Stream, In_File);

      declare
         Stream         : constant Stream_Access := Ada.Streams.Stream_IO.Stream (File.Stream);
         File_Head      : File_Header;
         File_Head_Size : Count;
         Frame_Head     : Frame_Header;
         Last           : Count;
      begin
         File_Header'Read (Stream, File_Head);

         if File_Head.ID /= "ID3" then
            raise Use_Error with "'" & Ada.Streams.Stream_IO.Name (File.Stream) & "' does not start with an ID3 record";
         end if;

         if File_Head.Version_Msb /= 3 then
            raise Use_Error with "'" & Ada.Streams.Stream_IO.Name (File.Stream) & "' ID3 version is" &
              Interfaces.Unsigned_8'Image (File_Head.Version_Msb) & "; expecting 3";
         end if;

         File_Head_Size := Size (File_Head.Size);

         Last := File_Header_Byte_Size;
         Set_Index (File.Stream, Last + 1);

         loop
            Frame_Header'Read (Stream, Frame_Head);

            if Last >= File_Head_Size or Size (Frame_Head.Size) = 0 then
               --  Real files seem to terminate the header with many 0 bytes, rather
               --  then at File_Header.Size.
               raise SAL.Not_Found with "tag '" & Tag & "' not found in '" &
                 Ada.Streams.Stream_IO.Name (File.Stream) & "'";
            end if;

            exit when Frame_Head.ID = Tag;
            Last := Last + Frame_Header_Byte_Size + Size (Frame_Head.Size);
            Set_Index (File.Stream, Last + 1);
         end loop;

         declare
            Result : String (1 .. Integer (Size (Frame_Head.Size)));
         begin
            String'Read (Stream, Result);
            if Result (1) = ASCII.NUL then
               return Result (2 .. Result'Last);
            else
               return Result;
            end if;
         end;
      end;
   end Read;

end SMM.ID3;
