--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018, 2023 Stephen Leake All Rights Reserved.
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

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Interfaces;
with SAL;
package body SMM.JPEG is

   SOF0  : constant Ada.Streams.Stream_Element := 16#C0#; -- Start Of Frame N
   SOF1  : constant Ada.Streams.Stream_Element := 16#C1#; -- N indicates which compression process
   SOF2  : constant Ada.Streams.Stream_Element := 16#C2#; -- Only SOF0-SOF2 are now in common use
   SOF3  : constant Ada.Streams.Stream_Element := 16#C3#;
   SOF5  : constant Ada.Streams.Stream_Element := 16#C5#; -- C4 and CC are NOT SOF markers
   SOF6  : constant Ada.Streams.Stream_Element := 16#C6#;
   SOF7  : constant Ada.Streams.Stream_Element := 16#C7#;
   SOF9  : constant Ada.Streams.Stream_Element := 16#C9#;
   SOF10 : constant Ada.Streams.Stream_Element := 16#CA#;
   SOF11 : constant Ada.Streams.Stream_Element := 16#CB#;
   SOF13 : constant Ada.Streams.Stream_Element := 16#CD#;
   SOF14 : constant Ada.Streams.Stream_Element := 16#CE#;
   SOF15 : constant Ada.Streams.Stream_Element := 16#CF#;

   SOI : constant Ada.Streams.Stream_Element := 16#D8#;
   SOS : constant Ada.Streams.Stream_Element := 16#DA#; --  start of image data
   EOI : constant Ada.Streams.Stream_Element := 16#D9#; --  end of image

   type Two_Bytes is record
      High : Interfaces.Unsigned_8;
      Low  : Interfaces.Unsigned_8;
   end record;
   for Two_Bytes'Size use 2 * 8;

   function To_Integer (Item : in Two_Bytes) return Integer
   is
   begin
      return Integer (Item.High) * 256 + Integer (Item.Low);
   end To_Integer;

   type SOF_Marker is record
      --  [2] process_SOFn
      Length     : Two_Bytes;
      Precision  : Interfaces.Unsigned_8;
      Height     : Two_Bytes;
      Width      : Two_Bytes;
      Components : Interfaces.Unsigned_8;
   end record;
   pragma Pack (SOF_Marker);
   for SOF_Marker'Size use 8 * 8;

   ----------
   --  Public subprograms

   overriding procedure Finalize (File : in out SMM.JPEG.File)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream) then
         Close (File.Stream);
      end if;
   end Finalize;

   procedure Open (File : in out SMM.JPEG.File; Name : in String)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream) then
         raise Ada.IO_Exceptions.Use_Error with "file is already open with '" &
           Ada.Streams.Stream_IO.Name (File.Stream) & "'";
      end if;

      Open (File.Stream, In_File, Name);
   end Open;

   procedure Close (File : in out SMM.JPEG.File)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream) then
         Close (File.Stream);
      end if;
   end Close;

   function Image (Size : in Size_Type) return String
   is begin
      return "(" & Integer'Image (Size.X) & "," & Integer'Image (Size.Y) & ")";
   end Image;

   function Size (File : in SMM.JPEG.File) return Size_Type
   is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      Stream : constant access Root_Stream_Type'Class := Ada.Streams.Stream_IO.Stream (File.Stream);
      Data   : Stream_Element_Array (1 .. 2);
      Last   : Stream_Element_Offset;
      Length : Two_Bytes;

      procedure Next_Marker
      is begin
         --  [2] next_marker
         Read (Stream.all, Data (1 .. 1), Last);
         if not (Last = 1 and Data (1) = 16#FF#) then
            raise SAL.Invalid_Format with "malformed JPEG file at" & Count'Image (Index (File.Stream));
         end if;

         loop
            --  More FF is padding
            Read (Stream.all, Data (1 .. 1), Last);
            exit when Last /= 1 or Data (1) /= 16#FF#;
         end loop;
      end Next_Marker;

      function Handle_SOF return Size_Type
      is
         Marker : SOF_Marker;
      begin
         SOF_Marker'Read (Stream, Marker);
         return (X => To_Integer (Marker.Width), Y => To_Integer (Marker.Height));
      end Handle_SOF;

   begin
      Read (Stream.all, Data, Last); -- [2] first_marker

      if not (Last = 2 and Data (1) = 16#FF# and Data (2) = SOI) then
         raise SAL.Invalid_Format with "file header not JPEG";
      end if;

      loop
         Next_Marker;

         if Data (1) = SOF0 or
           Data (1) = SOF1 or
           Data (1) = SOF2 or
           Data (1) = SOF3 or
           Data (1) = SOF5 or
           Data (1) = SOF6 or
           Data (1) = SOF7 or
           Data (1) = SOF9 or
           Data (1) = SOF10 or
           Data (1) = SOF11 or
           Data (1) = SOF13 or
           Data (1) = SOF14 or
           Data (1) = SOF15
         then
            return Handle_SOF;

         elsif Data (1) = SOS or
           Data (1) = EOI
         then
            raise SAL.Not_Found;

         end if;
         Two_Bytes'Read (Stream, Length);

         declare
            Junk : Stream_Element_Array (1 .. Stream_Element_Offset (To_Integer (Length) - 2));
            --  GNAT 12 requires this present; GNAT 13 requires it absent. Sigh.
            --  pragma Unreferenced (Junk);
         begin
            Stream.Read (Junk, Last);
         end;
      end loop;
   exception
   when E : SAL.Not_Implemented | SAL.Invalid_Format =>
      declare
         use Ada.Exceptions;
      begin
         Raise_Exception
           (Exception_Identity (E), Ada.Streams.Stream_IO.Name (File.Stream) & ": " & Exception_Message (E));
      end;
   end Size;

   function Size (File_Name : in String) return Size_Type
   is
      File : SMM.JPEG.File;
   begin
      Open (File, File_Name);
      return Size (File);
   end Size;

end SMM.JPEG;
