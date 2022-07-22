--  Abstract :
--
--  See spec.
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Text_IO;
with GNATCOLL.Iconv;
with SAL.Generic_Binary_Image;
package body SMM.ID3 is
   use SMM.Metadata;

   function Is_Alphanumeric (Item : in ID_String) return Boolean
   is
      use Ada.Characters.Handling;
   begin
      --  Apparently a very old format allows ascii nul at end of ID.
      for I in Item'First .. Item'Last - 1 loop
         if not Is_Alphanumeric (Item (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Alphanumeric;

   function Size (Item : in Size_Type) return Ada.Streams.Stream_IO.Count
   is
      use Ada.Streams.Stream_IO;
      --  Size_Type is a syncsafe integer; [2] 6.2
   begin
      return
        Count (Item.Byte_3) * 128 ** 3 +
        Count (Item.Byte_2) * 128 ** 2 +
        Count (Item.Byte_1) * 128 +
        Count (Item.Byte_0);
   end Size;

   function Old_Size (Item : in Size_Type) return Ada.Streams.Stream_IO.Count
   is
      use Ada.Streams.Stream_IO;
      --  Assume Size is actually a 32 bit integer (older versions of ID3 standard)
   begin
      return
        Count (Item.Byte_3) * 256 ** 3 +
        Count (Item.Byte_2) * 256 ** 2 +
        Count (Item.Byte_1) * 256 +
        Count (Item.Byte_0);
   end Old_Size;

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

   function Image is new SAL.Generic_Binary_Image (Nibbles => 2, Number_Type => Interfaces.Unsigned_8);

   type File_Header is record
      ID          : String (1 .. 3);
      Version_Msb : Interfaces.Unsigned_8;
      Version_Lsb : Interfaces.Unsigned_8;
      Flags       : Interfaces.Unsigned_8;
      Size        : Size_Type;
   end record;
   File_Header_Byte_Size : constant := 10;
   pragma Pack (File_Header);
   for File_Header'Size use File_Header_Byte_Size * 8;

   type Frame_Header is record
      ID      : ID_String;
      Size    : Size_Type;
      Flags_1 : Interfaces.Unsigned_8; -- alter handling, read_only
      Flags_0 : Interfaces.Unsigned_8; -- grouping, compression, encryption
   end record;
   Frame_Header_Byte_Size : constant := 10;
   for Frame_Header'Size use Frame_Header_Byte_Size * 8;

   Null_Header : constant Frame_Header :=
     (ID      => (others => ASCII.NUL),
      Size    => (others => 0),
      Flags_0 => 0,
      Flags_1 => 0);

   --  [2] section 4
   Text_Encoding_ISO_8859_1 : constant Ada.Streams.Stream_Element := 0;
   Text_Encoding_UTF_16     : constant Ada.Streams.Stream_Element := 1;
   Text_Encoding_UTF_16BE   : constant Ada.Streams.Stream_Element := 2;
   Text_Encoding_UTF_8      : constant Ada.Streams.Stream_Element := 3;

   function To_String (Data : in Ada.Streams.Stream_Element_Array) return String
   is begin
      return Result : String (Integer (Data'First) .. Integer (Data'Last)) do
         for I in Data'Range loop
            Result (Integer (I)) := Character'Val (Data (I));
         end loop;
      end return;
   end To_String;

   subtype UTF_16_Encoding is Ada.Strings.UTF_Encoding.Encoding_Scheme range
     Ada.Strings.UTF_Encoding.UTF_16BE .. Ada.Strings.UTF_Encoding.UTF_16LE;

   function To_Wide_String
     (Data     : in Ada.Streams.Stream_Element_Array;
      Encoding : in UTF_16_Encoding)
     return Wide_String
   is
      use Ada.Streams;
      use Ada.Strings.UTF_Encoding;
      J : Stream_Element_Offset := Data'First;
   begin
      return Result : Wide_String (1 .. Data'Length / 2) do
         for I in Result'Range loop
            Result (I) := Wide_Character'Val
              (case Encoding is
               when UTF_16BE => Integer (Data (J)) * 256 + Integer (Data (J + 1)),
               when UTF_16LE => Integer (Data (J)) + Integer (Data (J + 1)) * 256);
            J := J + 2;
         end loop;
      end return;
   end To_Wide_String;

   function To_Frame
     (Header    : in Frame_Header;
      Data      : in Ada.Streams.Stream_Element_Array;
      More_Data : in Ada.Streams.Stream_Element_Array := (1 .. 0 => 0))
     return Frame
   is
      use Ada.Streams;
      use all type Interfaces.Unsigned_8;

      function To_UTF_8 (First, Last : Stream_Element_Offset) return String
      is
         use GNATCOLL.Iconv;
      begin
         if not Has_Iconv then
            raise SAL.Programmer_Error with "GNATCOLL.Iconv not installed";
         end if;
         return Iconv
           (Input     => To_String (Data (First .. Last)),
            To_Code   => UTF8,
            From_Code => ISO_8859_1);
      end To_UTF_8;

      function To_Frame (First, Last : Stream_Element_Offset) return Frame
      is begin
         if (for some C of Data (First .. Last) => C = 0) then
            return (Header.ID, +"<multiple strings>");
         else
            return (Header.ID, +To_String (Data (First .. Last)));
         end if;
      end To_Frame;

   begin
      if not Ignore_Flags and Header.Flags_0 /= 0 then
         raise SAL.Not_Implemented with "frame flags_0:" & Image (Header.Flags_0);
      end if;
      --  We don't care about Flags_1, since we are only reading.

      if Header.ID (1) = 'T' then
         --  [3] section 4.2.
         if More_Data'Length > 0 then
            raise SAL.Not_Implemented with "More_Data > 0:" & Header.ID;
         end if;

         case Data (1) is
         when Text_Encoding_ISO_8859_1 =>
            --  Supposed to be null terminated, but sometimes not.
            if Data (Data'Last) = 0 then
               return (Header.ID, +To_UTF_8 (Data'First + 1, Data'Last - 1));
            else
               return (Header.ID, +To_UTF_8 (Data'First + 1, Data'Last));
            end if;

         when Text_Encoding_UTF_8 =>
            --  Supposed to be null terminated, but sometimes not.
            if Data (Data'Last) = 0 then
               return To_Frame (Data'First + 1, Data'Last - 1);
            else
               return To_Frame (Data'First + 1, Data'Last);
            end if;

         when Text_Encoding_UTF_16 =>
            declare
               use Ada.Strings.UTF_Encoding;
               use Ada.Strings.UTF_Encoding.Conversions;
               UTF_16_Data : constant Wide_String := To_Wide_String
                 (Data (Data'First + 3 .. Data'Last -
                          (if Data (Data'Last) = 0 and Data (Data'Last - 1) = 0 then 2 else 0)),
                  Encoding (To_String (Data (Data'First + 1 .. Data'First + 2))));
            begin
               return (Header.ID, +Convert (UTF_16_Data, UTF_8));
            end;

         when Text_Encoding_UTF_16BE =>
            declare
               use Ada.Strings.UTF_Encoding;
               use Ada.Strings.UTF_Encoding.Conversions;
               UTF_16_Data : constant Wide_String := To_Wide_String
                 (Data (Data'First + 1 .. Data'Last - (if Data (Data'Last) = 0 then 2 else 0)),
                  UTF_16BE);
            begin
               return (Header.ID, +Convert (UTF_16_Data, UTF_8));
            end;

         when others =>
            --  Older format; no encoding byte.
            return To_Frame (Data'First, Data'Last);
         end case;

      else
         return (Header.ID, +"");
      end if;
   end To_Frame;

   ----------
   --  Public subprograms

   function All_Frames (File : in SMM.Metadata.File) return Frame_Lists.List
   is
      use all type Ada.Streams.Stream_IO.Count;
      use all type Interfaces.Unsigned_8;

      File_Head  : File_Header;
      Total_Size : Ada.Streams.Stream_IO.Count;
      Frame_Head : Frame_Header;
      Stream     : constant access Ada.Streams.Root_Stream_Type'Class := File.Stream;
      Result     : Frame_Lists.List;

      function Valid_Header (Head : in Frame_Header) return Boolean
      is begin
         return Size (Head.Size) > 0 and
           Index (File) + Size (Head.Size) - 1 <= Total_Size and
           Is_Alphanumeric (Head.ID);
      end Valid_Header;

   begin
      File_Header'Read (Stream, File_Head);

      if not (File_Head.ID = "ID3" and
                (File_Head.Version_Msb in 3 | 4))
      then
         raise SAL.Invalid_Format with "file header not ID3:" & File_Head.ID &
           Interfaces.Unsigned_8'Image (File_Head.Version_Msb);
      end if;

      if not Ignore_Flags and  File_Head.Flags /= 0 then
         raise SAL.Not_Implemented with "file flags:" & Image (File_Head.Flags);
      end if;

      Total_Size := File_Header_Byte_Size + Size (File_Head.Size);

      Frame_Header'Read (Stream, Frame_Head);
      loop
         declare
            use Ada.Streams;
            use Ada.Streams.Stream_IO;

            --  If File_Head.Version_Msb is 3, some encoders incorrectly use a
            --  plain 32 bit integer for the frame size, instead of a 28 bit
            --  syncsafe integer. So first assume syncsafe. and see if the next
            --  item is a tag or padding. If not, try 32 bit.
            Size_28 : constant Count := Size (Frame_Head.Size);
            Size_32 : constant Count := Old_Size (Frame_Head.Size);

            Temp_Frame_Head_1 : Frame_Header;
            Temp_Frame_Head_2 : Frame_Header;

            Data : Stream_Element_Array (1 .. Stream_Element_Offset (Size_28));
            Last : Stream_Element_Offset;
         begin
            Stream.Read (Data, Last);

            --  There might be padding that is less than one frame long.
            Frame_Header'Read (Stream, Temp_Frame_Head_1);

            if Index (File) >= Total_Size then
               Result.Append (To_Frame (Frame_Head, Data));
               return Result;
            end if;

            if Valid_Header (Temp_Frame_Head_1) then
               --  Size_28 is correct, not in padding
               Result.Append (To_Frame (Frame_Head, Data));
               Frame_Head := Temp_Frame_Head_1;

            else
               --  Size_28 is wrong, or we are in padding. Try Size_32.
               declare
                  More_Data : Stream_Element_Array
                    (1 .. Stream_Element_Offset (Size_32 - Size_28 - Frame_Header_Byte_Size));
               begin
                  Stream.Read (More_Data, Last);

                  if File.Index >= Total_Size then
                     Result.Append (To_Frame (Frame_Head, Data, More_Data));
                     return Result;
                  end if;

                  Frame_Header'Read (Stream, Temp_Frame_Head_2);
                  if Temp_Frame_Head_2 = Null_Header then
                     --  We are now in padding
                     Result.Append (To_Frame (Frame_Head, Data, More_Data));
                     return Result;

                  elsif Valid_Header (Temp_Frame_Head_2) then
                     Result.Append (To_Frame (Frame_Head, Data & More_Data));
                     Frame_Head := Temp_Frame_Head_2;

                  elsif Temp_Frame_Head_1 = Null_Header then
                     --  We were in padding
                     Result.Append (To_Frame (Frame_Head, Data));
                     return Result;

                  else
                     raise SAL.Invalid_Format;
                  end if;
               end;
            end if;
         end;
      end loop;
   exception
   when E : SAL.Not_Implemented | SAL.Invalid_Format =>
      declare
         use Ada.Exceptions;
      begin
         Raise_Exception
           (Exception_Identity (E), File.Name & ": " & Exception_Message (E));
      end;
   end All_Frames;

   procedure Metadata
     (Abs_File_Name : in     String;
      ID3_Frames    :    out SMM.Metadata.Frame_Lists.List;
      Artist_ID     :    out SMM.Metadata.ID_String)
   is
      use Ada.Strings.Unbounded;
      File : SMM.Metadata.File;
   begin
      Artist_ID := Artist;
      File.Open (Abs_File_Name);
      ID3_Frames := All_Frames (File);

      if Is_Present (Artist, ID3_Frames) and then
        Length (Find (Artist, ID3_Frames)) > 0
      then
         Artist_ID := Artist;
      else
         if Is_Present (Album_Artist, ID3_Frames) and then
           Length (Find (Album_Artist, ID3_Frames)) > 0
         then
            Artist_ID := Album_Artist;
         else
            Ada.Text_IO.Put_Line (Abs_File_Name & " missing artist");
         end if;
      end if;

      if not Is_Present (Album, ID3_Frames) then
         Ada.Text_IO.Put_Line (Abs_File_Name & " missing album");
      end if;

      if not Is_Present (Title, ID3_Frames) then
         Ada.Text_IO.Put_Line (Abs_File_Name & " missing title");
      end if;
   end Metadata;

   procedure Create
     (Name    : in String;
      Content : in Frame_Lists.List)
   is
      use Ada.Strings.Unbounded;
      use Ada.Streams.Stream_IO;
      File : aliased File_Type;

      function Size (Item : in Frame) return Ada.Streams.Stream_IO.Count
      is begin
         return Ada.Streams.Stream_IO.Count (Frame_Header_Byte_Size + Ada.Strings.Unbounded.Length (Item.Data));
      end Size;

      function Size (Item : in Frame_Lists.List) return Ada.Streams.Stream_IO.Count
      is begin
         return Result : Ada.Streams.Stream_IO.Count := 0 do
            for I of Item loop
               Result := Result + Size (I);
            end loop;
         end return;
      end Size;
   begin
      Create (File, Out_File, Name);
      File_Header'Write (Stream (File), File_Header'("ID3", 3, 0, 0, To_Size (Size (Content))));
      for T of Content loop
         Frame_Header'Write
           (Stream (File),
            Frame_Header'(T.ID, To_Size (Ada.Streams.Stream_IO.Count (Length (T.Data))), 0, 0));
         String'Write (Stream (File), To_String (T.Data));
      end loop;
      String'Write (Stream (File), (1 .. 20 => ' ')); --  padding
      Close (File);
   end Create;

end SMM.ID3;
