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
package body SMM.ID3 is

   function Size (Item : in Size_Type) return Ada.Streams.Stream_IO.Count
   is
      use Ada.Streams.Stream_IO;
   begin
      return
        Count (Item.Byte_3) * 128 ** 3 +
        Count (Item.Byte_2) * 128 ** 2 +
        Count (Item.Byte_1) * 128 +
        Count (Item.Byte_0);
   end Size;

   function To_Size (Item : in Ada.Streams.Stream_IO.Count) return Size_Type
   is
      use Ada.Streams.Stream_IO;
      use Interfaces;
      Remaining : Count := Item;
   begin
      return Result : Size_Type do
         Result.Byte_0 := Unsigned_8 (Item mod 128);
         Remaining := Remaining - Count (Result.Byte_0);
         Remaining := Remaining / 128;

         Result.Byte_1 := Unsigned_8 (Remaining mod 128);
         Remaining := Remaining - Count (Result.Byte_1);
         Remaining := Remaining / 128;

         Result.Byte_2 := Unsigned_8 (Remaining mod 128);
         Remaining := Remaining - Count (Result.Byte_2);
         Remaining := Remaining / 128;

         Result.Byte_3 := Unsigned_8 (Remaining);
      end return;
   end To_Size;

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

   function Size (Item : in Tag) return Ada.Streams.Stream_IO.Count
   is begin
      return Ada.Streams.Stream_IO.Count (Frame_Header_Byte_Size + Ada.Strings.Unbounded.Length (Item.Data));
   end Size;

   function Size (Item : in Tag_Lists.List) return Ada.Streams.Stream_IO.Count
   is
      use Ada.Streams.Stream_IO;
   begin
      return Result : Count := 0 do
         for I of Item loop
            Result := Result + Size (I);
         end loop;
      end return;
   end Size;

   function Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in Tag_String) return String
   is
      use all type Ada.Streams.Stream_IO.Count;
      use all type Interfaces.C.char_array;
      use all type Interfaces.Unsigned_8;

      File_Head  : File_Header;
      Frame_Head : Frame_Header;
   begin
      File_Header'Read (Stream, File_Head);

      if File_Head.ID /= "ID3" or File_Head.Version_Msb /= 3 then
         raise Ada.IO_Exceptions.Use_Error;
      end if;

      loop
         Frame_Header'Read (Stream, Frame_Head);

         if Size (Frame_Head.Size) = 0 then
            --  Real files seem to terminate the header with many 0 bytes, rather
            --  then at File_Header.Size.
            --
            --  Caller adds appropriate message.
            raise SAL.Not_Found;
         end if;

         exit when Frame_Head.ID = Tag;

         --  Skip the contents
         declare
            use Ada.Streams;
            Junk : Stream_Element_Array (1 .. Stream_Element_Offset (Size (Frame_Head.Size)));
            Last : Stream_Element_Offset;
         begin
            Stream.Read (Junk, Last);
         end;
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
   end Read;

   ----------
   --  Public subprograms

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

   function Read (File : in out SMM.ID3.File; Tag : in Tag_String) return String
   is
      use Ada.Streams.Stream_IO;
   begin
      if not Is_Open (File.Stream) then
         raise Ada.IO_Exceptions.Use_Error with "file not open";
      end if;

      Reset (File.Stream, In_File);

      return Read (Ada.Streams.Stream_IO.Stream (File.Stream), Tag);
   exception
   when Ada.IO_Exceptions.Use_Error =>
      raise Ada.IO_Exceptions.Use_Error with
        "'" & Ada.Streams.Stream_IO.Name (File.Stream) & "' does not start with an ID3 record";

   when SAL.Not_Found =>
      raise SAL.Not_Found with "tag '" & Tag & "' not found in '" &
        Ada.Streams.Stream_IO.Name (File.Stream) & "'";
   end Read;

   procedure Create
     (Name    : in String;
      Content : in Tag_Lists.List)
   is
      use Ada.Strings.Unbounded;
      use Ada.Streams.Stream_IO;
      File : aliased File_Type;
   begin
      Create (File, Out_File, Name);
      File_Header'Write (Stream (File), File_Header'("ID3", 3, 0, 0, To_Size (Size (Content))));
      for T of Content loop
         Frame_Header'Write
           (Stream (File),
            Frame_Header'(T.ID, To_Size (Ada.Streams.Stream_IO.Count (Length (T.Data))), 0, 0));
         String'Write (Stream (File), To_String (T.Data));
      end loop;
      Close (File);
   end Create;

end SMM.ID3;
