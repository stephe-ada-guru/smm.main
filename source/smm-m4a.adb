--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Interfaces;
with SAL;
package body SMM.M4a is

   type Raw_Size is record
      Byte_3 : Interfaces.Unsigned_8;
      Byte_2 : Interfaces.Unsigned_8;
      Byte_1 : Interfaces.Unsigned_8;
      Byte_0 : Interfaces.Unsigned_8;
   end record;
   for Raw_Size'Size use 4 * 8;

   function To_Offset (Item : in Raw_Size) return Ada.Streams.Stream_Element_Offset
   is
      use Ada.Streams;
   begin
      return
        Stream_Element_Offset (Item.Byte_0) +
        256 * (Stream_Element_Offset (Item.Byte_1) +
                 256 * (Stream_Element_Offset (Item.Byte_2) +
                          256 * Stream_Element_Offset (Item.Byte_3)));
   end To_Offset;

   subtype Stream_Element_Array_4 is Ada.Streams.Stream_Element_Array (1 .. 4);

   function To_Offset (Item : in Stream_Element_Array_4) return Ada.Streams.Stream_Element_Offset
   is
      use Ada.Streams;
   begin
      return
        Stream_Element_Offset (Item (4)) +
        256 * (Stream_Element_Offset (Item (3)) +
                 256 * (Stream_Element_Offset (Item (2)) +
                          256 * Stream_Element_Offset (Item (1))));
   end To_Offset;

   function Valid_Label (Item : in SMM.Metadata.ID_String) return Boolean
   is
      subtype Label_Range is Character range 'a' .. 'z';
   begin
      return (for all Char of Item => Char in Label_Range);
   end Valid_Label;

   procedure Read_Frame
     (Data  : in     Ada.Streams.Stream_Element_Array;
      Last  : in out Ada.Streams.Stream_Element_Offset;
      Frame :    out SMM.Metadata.Frame)
   --  Search in Data starting at Last + 1 for "data", populate Frame
   --  from that data. Update Last to last element of Data read.
   is
      use Ada.Streams;
      Aart_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => Character'Pos ('a'),
         2 => Character'Pos ('A'),
         3 => Character'Pos ('R'),
         4 => Character'Pos ('T'));

      Alb_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => 16#a9#,
         2 => Character'Pos ('a'),
         3 => Character'Pos ('l'),
         4 => Character'Pos ('b'));

      Art_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => 16#a9#,
         2 => Character'Pos ('A'),
         3 => Character'Pos ('R'),
         4 => Character'Pos ('T'));

      Data_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => Character'Pos ('d'),
         2 => Character'Pos ('a'),
         3 => Character'Pos ('t'),
         4 => Character'Pos ('a'));

      Day_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => 16#a9#,
         2 => Character'Pos ('d'),
         3 => Character'Pos ('a'),
         4 => Character'Pos ('y'));

      Nam_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => 16#a9#,
         2 => Character'Pos ('n'),
         3 => Character'Pos ('a'),
         4 => Character'Pos ('m'));

      Too_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => 16#a9#,
         2 => Character'Pos ('t'),
         3 => Character'Pos ('o'),
         4 => Character'Pos ('o'));

      Trkn_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => Character'Pos ('t'),
         2 => Character'Pos ('r'),
         3 => Character'Pos ('k'),
         4 => Character'Pos ('n'));

      Wrt_Elements : constant Stream_Element_Array (1 .. 4) :=
        (1 => 16#a9#,
         2 => Character'Pos ('w'),
         3 => Character'Pos ('r'),
         4 => Character'Pos ('t'));


      function To_String (Data : in Stream_Element_Array) return Ada.Strings.Unbounded.Unbounded_String
      is
         Result : String (Integer (Data'First) .. Integer (Data'Last));
      begin
         for I in Data'Range loop
            Result (Integer (I)) := Character'Val (Data (I));
         end loop;
         return +Result;
      end To_String;

   begin
      loop
         Last := @ + 1;
         exit when Data (Last .. Last + 3) = Data_Elements;
      end loop;
      declare
         Size      : constant Stream_Element_Offset         := To_Offset (Data (Last - 4 .. Last - 1));
         M4a_Label : constant Stream_Element_Array (1 .. 4) := Data (Last - 8 .. Last - 5);

         function String_Data return Ada.Strings.Unbounded.Unbounded_String
         is begin
            return Result : constant Ada.Strings.Unbounded.Unbounded_String :=
              To_String (Data (Last + 12 .. Last + Size - 5))
            do
               Last := Last + Size - 5;
            end return;
         end String_Data;

      begin
         if M4a_Label = Aart_Elements then
            Frame.ID := SMM.Metadata.Album_Artist;
            Frame.Data := String_Data;

         elsif M4a_Label = Alb_Elements then
            Frame.ID := SMM.Metadata.Album;
            Frame.Data := String_Data;

         elsif M4a_Label = Art_Elements then
            Frame.ID := SMM.Metadata.Artist;
            Frame.Data := String_Data;

         elsif M4a_Label = Day_Elements then
            Frame.ID := SMM.Metadata.Year;
            Frame.Data := String_Data;

         elsif M4a_Label = Nam_Elements then
            Frame.ID := SMM.Metadata.Title;
            Frame.Data := String_Data;

         elsif M4a_Label = Too_Elements then
            Frame.ID := "?too";
            Frame.Data := Ada.Strings.Unbounded.Null_Unbounded_String;

         elsif M4a_Label = Trkn_Elements then
            Frame.ID := SMM.Metadata.Track;
            declare
               Track : constant Integer := Integer (Data (Last + 15));
            begin
               Frame.Data := +Track'Image;
               Last := Last + 24;
            end;

         elsif M4a_Label = Wrt_Elements then
            Frame.ID   := SMM.Metadata.Composer;
            Frame.Data := String_Data;

         end if;
      end;
   end Read_Frame;

   ----------
   --  Public subprograms

   procedure Next_Chunk
     (File  : in     SMM.Metadata.File;
      Chunk : in out M4a.Chunk)
   is
      use Ada.Streams;
      Stream    : access Root_Stream_Type'Class renames SMM.Metadata.Stream (File);
      Buffer    : Stream_Element_Array (1 .. 2048);
      Remaining : Stream_Element_Offset := Chunk.Size - 8;
      Last      : Stream_Element_Offset;
   begin
      loop
         exit when Remaining = 0; -- Finished this chunk.
         exit when File.End_Of_File; -- Something's wrong.
         if Remaining < Buffer'Last then
            Read (Stream.all, Buffer (1 .. Remaining), Last);
         else
            Read (Stream.all, Buffer, Last);
         end if;
         Remaining := @ - Last;
      end loop;

      if File.End_Of_File then
         --  No next chunk
         Chunk.Label := No_Chunk;
         Chunk.Size := 0;

      else
         declare
            Size_Raw : Raw_Size;
            Label    : SMM.Metadata.ID_String;
         begin
            Raw_Size'Read (Stream, Size_Raw);
            SMM.Metadata.ID_String'Read (Stream, Label);
            if Valid_Label (Label) then
               Chunk.Size  := To_Offset (Size_Raw);
               Chunk.Label := Label;

            else
               Chunk.Size  := 0;
               Chunk.Label := No_Chunk;
            end if;
         end;
      end if;
   end Next_Chunk;

   procedure Find_Chunk
     (File  : in     SMM.Metadata.File;
      Label : in     SMM.Metadata.ID_String;
      Chunk : in out M4a.Chunk)
   is
      Stream : access Ada.Streams.Root_Stream_Type'Class renames SMM.Metadata.Stream (File);
   begin
      declare
         Size_Raw : Raw_Size;
      begin
         Raw_Size'Read (Stream, Size_Raw);
         Chunk.Size := To_Offset (Size_Raw);
         SMM.Metadata.ID_String'Read (Stream, Chunk.Label);
         if not Valid_Label (Chunk.Label) then
            raise SAL.Not_Found;
         end if;
      end;

      loop
         exit when SMM.Metadata.End_Of_File (File);

         exit when Label = Chunk.Label;

         Next_Chunk (File, Chunk);
      end loop;
   end Find_Chunk;

   procedure Metadata
     (Abs_File_Name : in     String;
      Frames        :    out SMM.Metadata.Frame_Lists.List;
      Artist_ID     :    out SMM.Metadata.ID_String)
   is
      pragma Unreferenced (Artist_ID); -- FIXME:
      File  : SMM.Metadata.File;
      Chunk : SMM.M4a.Chunk;
   begin
      --  FIXME: Artist_ID := Artist;
      File.Open (Abs_File_Name);

      --  File contains nested chunks; song metadata is in moov | udta |
      --  meta | ilst.
      Find_Chunk (File, "moov", Chunk);
      Find_Chunk (File, "udta", Chunk);
      Find_Chunk (File, "meta", Chunk);

      declare
         use all type Ada.Streams.Stream_Element_Offset;
         Data : Ada.Streams.Stream_Element_Array (1 .. Chunk.Size - 8);
         Last : Ada.Streams.Stream_Element_Offset := Data'First - 1;
         Frame : SMM.Metadata.Frame;
      begin
         File.Read (Data, Last);
         pragma Assert (Last = Data'Last);
         Last := Data'First - 1;
         loop
            Read_Frame (Data, Last, Frame);
            Frames.Append (Frame);
            exit when Last = Data'Last;
         end loop;
      end;

      --  FIXME: read artist etc.
   end Metadata;

end SMM.M4a;
