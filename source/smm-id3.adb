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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Interfaces.C;
with SAL;
package body SMM.ID3 is

   function Is_Alphanumeric (Item : in ID_String) return Boolean
   is
      use Ada.Characters.Handling;
   begin
      for I in Item'Range loop
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
      ID      : ID_String;
      Size    : Size_Type;
      Flags_1 : Interfaces.Unsigned_8;
      Flags_0 : Interfaces.Unsigned_8;
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

   function To_Frame
     (Header : in Frame_Header;
      Data   : in Ada.Streams.Stream_Element_Array)
     return Frame
   is
      use Ada.Streams;
      use all type Interfaces.Unsigned_8;

      function To_UTF_8 (First, Last : Stream_Element_Offset) return Frame
      is begin
         if (for some C of Data (First .. Last) => C = 0) then
            return (Header.ID, +"<multiple strings>");
         else
            return (Header.ID, +To_String (Data (First .. Last)));
         end if;
      end To_UTF_8;

   begin
      if Header.Flags_0 /= 0 or Header.Flags_1 /= 0 then
         raise SAL.Not_Implemented with "frame flags not 0";
      end if;

      if Header.ID (1) = 'T' then
         --  [3] section 4.2.
         case Data (1) is
         when Text_Encoding_ISO_8859_1 | Text_Encoding_UTF_8 =>
            --  Supposed to be null terminated, but sometimes not.
            if Data (Data'Last) = 0 then
               return To_UTF_8 (Data'First + 1, Data'Last - 1);
            else
               return To_UTF_8 (Data'First + 1, Data'Last);
            end if;

         when Text_Encoding_UTF_16 | Text_Encoding_UTF_16BE =>
            raise SAL.Not_Implemented with "UTF-16 string";

         when others =>
            --  Older format; no encoding byte.
            return To_UTF_8 (Data'First, Data'Last);
         end case;

      else
         return (Header.ID, +"");
      end if;
   end To_Frame;

   function Size (Item : in Frame) return Ada.Streams.Stream_IO.Count
   is begin
      return Ada.Streams.Stream_IO.Count (Frame_Header_Byte_Size + Ada.Strings.Unbounded.Length (Item.Data));
   end Size;

   function Size (Item : in Frame_Lists.List) return Ada.Streams.Stream_IO.Count
   is
      use Ada.Streams.Stream_IO;
   begin
      return Result : Count := 0 do
         for I of Item loop
            Result := Result + Size (I);
         end loop;
      end return;
   end Size;

   function Read
     (Stream       : not null access Ada.Streams.Root_Stream_Type'Class;
      ID           : in              ID_String;
      No_Exception : in              Boolean)
     return String
   is
      use all type Ada.Streams.Stream_IO.Count;
      use all type Interfaces.C.char_array;
      use all type Interfaces.Unsigned_8;

      File_Head  : File_Header;
      Frame_Head : Frame_Header;
   begin
      File_Header'Read (Stream, File_Head);

      if not (File_Head.ID = "ID3" and
                (File_Head.Version_Msb = 3 or File_Head.Version_Msb = 4))
      then
         if No_Exception then
            return "";
         else
            raise SAL.Invalid_Format;
         end if;
      end if;

      Frame_Header'Read (Stream, Frame_Head);
      loop

         if Size (Frame_Head.Size) = 0 then
            --  We are in padding; no more ID3 headers.
            --
            --  Caller adds appropriate message.
            if No_Exception then
               return "";
            else
               raise SAL.Not_Found;
            end if;
         end if;

         exit when Frame_Head.ID = ID;

         --  Skip the contents, read the next header.
         declare
            use Ada.Streams;
            use Ada.Streams.Stream_IO;

            --  If Version_Msb is 3, some encoders incorrectly use a plain 32 bit
            --  integer for the frame size, instead of a 28 bit syncsafe integer.
            --  So first assume syncsafe. and see if the next item is a tag or
            --  padding. If not, try 32 bit.
            Size_28 : constant Count := Size (Frame_Head.Size);
            Size_32 : constant Count := Old_Size (Frame_Head.Size);

            Junk : Stream_Element_Array (1 .. Stream_Element_Offset (Size_28));
            Last : Stream_Element_Offset;
         begin
            Stream.Read (Junk, Last);
            Frame_Header'Read (Stream, Frame_Head);

            if File_Head.Version_Msb = 4 or
              Size (Frame_Head.Size) = 0 or
              Is_Alphanumeric (Frame_Head.ID)
            then
               --  Looks ok
               null;
            else
               declare
                  More_Junk : Stream_Element_Array
                    (1 .. Stream_Element_Offset (Size_32 - Size_28 - Frame_Header_Byte_Size));
               begin
                  Stream.Read (More_Junk, Last);
                  Frame_Header'Read (Stream, Frame_Head);
                  if Size (Frame_Head.Size) = 0 or Is_Alphanumeric (Frame_Head.ID) then
                     --  Looks ok
                     null;
                  else
                     --  give up
                     if No_Exception then
                        return "";
                     else
                        raise SAL.Invalid_Format with "invalid ID3 tag";
                     end if;
                  end if;
               end;
            end if;
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

   procedure Close (File : in out SMM.ID3.File)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream) then
         Close (File.Stream);
      end if;
   end Close;

   function Read
     (File         : in out SMM.ID3.File;
      ID          : in     ID_String;
      No_Exception : in     Boolean := True)
     return String
   is
      use Ada.Streams.Stream_IO;
   begin
      if not Is_Open (File.Stream) then
         raise Ada.IO_Exceptions.Use_Error with "file not open";
      end if;

      Reset (File.Stream, In_File);

      return Read (Ada.Streams.Stream_IO.Stream (File.Stream), ID, No_Exception);
   exception
   when SAL.Not_Found =>
      raise SAL.Not_Found with "ID '" & ID & "' not found in '" &
        Ada.Streams.Stream_IO.Name (File.Stream) & "'";

   when E : others =>
      raise SAL.Invalid_Format with
        "ID3 read '" & Ada.Streams.Stream_IO.Name (File.Stream) & "' failed with '" &
        Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E);
   end Read;

   procedure Create
     (Name    : in String;
      Content : in Frame_Lists.List)
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

   function All_Frames (File : in SMM.ID3.File) return Frame_Lists.List
   is
      use all type Ada.Streams.Stream_IO.Count;
      use all type Interfaces.C.char_array;
      use all type Interfaces.Unsigned_8;

      function Valid_Header (Head : in Frame_Header) return Boolean
      is begin
         return Size (Head.Size) > 0 and Is_Alphanumeric (Head.ID);
      end Valid_Header;

      File_Head  : File_Header;
      Total_Size : Ada.Streams.Stream_IO.Count;
      Frame_Head : Frame_Header;
      Stream     : constant access Ada.Streams.Root_Stream_Type'Class := Ada.Streams.Stream_IO.Stream (File.Stream);
      Result     : Frame_Lists.List;


   begin
      File_Header'Read (Stream, File_Head);

      if not (File_Head.ID = "ID3" and
                (File_Head.Version_Msb = 3 or File_Head.Version_Msb = 4))
      then
         raise SAL.Invalid_Format;
      end if;

      if File_Head.Flags /= 0 then
         raise SAL.Not_Implemented with "file flags not 0";
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

            if Index (File.Stream) >= Total_Size then
               Result.Append (To_Frame (Frame_Head, Data));
               return Result;
            end if;

            Frame_Header'Read (Stream, Temp_Frame_Head_1);

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
                  Frame_Header'Read (Stream, Temp_Frame_Head_2);
                  if Temp_Frame_Head_2 = Null_Header then
                     --  We are now in padding
                     Result.Append (To_Frame (Frame_Head, Data & More_Data));
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
   end All_Frames;

end SMM.ID3;
