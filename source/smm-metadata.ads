--  Abstract :
--
--  Common types for song meta data.
--
--  References:
--
--  [1] http://id3.org/d3v2.3.0            ID3 2.3.0 spec
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Streams.Stream_IO;
package SMM.Metadata is

   type File is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (File : in out SMM.Metadata.File);
   --  Close file.

   procedure Open (File : in out SMM.Metadata.File; Name : in String);
   --  Open for read.

   procedure Close (File : in out SMM.Metadata.File);

   function End_Of_File (File : in SMM.Metadata.File) return Boolean;

   function Index (File : in SMM.Metadata.File) return Ada.Streams.Stream_IO.Positive_Count;

   function Name (File : in SMM.Metadata.File) return String;

   function Stream (File : in SMM.Metadata.File) return access Ada.Streams.Root_Stream_Type'Class;

   procedure Read
     (File : in     SMM.Metadata.File;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   subtype ID_String is String (1 .. 4);

   --  Some common Frames, alphabetical by Ada var name
   Album          : constant ID_String := "TALB"; -- [1] Album/Movie/Show title
   Album_Artist   : constant ID_String := "TPE2"; -- [1] Band/orchestra/accompaniment = Album Artist
   Artist         : constant ID_String := "TPE1"; -- [1] Lead performer(s)/Soloist(s)
   Composer       : constant ID_String := "TCOM"; -- [1] Composer
   Orig_Year      : constant ID_String := "TORY"; -- [1] Original Release Year
   Recording_Time : constant ID_String := "TDRC"; -- [1] Recording Time
   Title          : constant ID_String := "TIT2"; -- [1] Title/songname/content description
   Track          : constant ID_String := "TRCK"; -- [1] Track number/position in a set
   Year           : constant ID_String := "TYER"; -- [1] Year

   type Frame is record
      ID   : ID_String;
      Data : Ada.Strings.Unbounded.Unbounded_String; -- In UTF-8 encoding
   end record;

   package Frame_Lists is new Ada.Containers.Doubly_Linked_Lists (Frame);

   function Is_Present (ID : in ID_String; Frames : in Frame_Lists.List) return Boolean
     is (for some F of Frames => ID = F.ID);

   function Find (ID : in ID_String; Frames : in Frame_Lists.List) return Ada.Strings.Unbounded.Unbounded_String;
   --  Return empty string if not found.

   function To_Track (Item : in String) return Integer;
   --  Handle <track>/<total>; -1 for empty string.

   function To_Year (Orig_Year, Year, Recording_Time : in String) return Integer;
   --  Orig_Year has preference, result has -1 for empty string

private

   type File is new Ada.Finalization.Limited_Controlled with record
      Stream_File : Ada.Streams.Stream_IO.File_Type;
   end record;

end SMM.Metadata;
