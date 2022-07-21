--  Abstract :
--
--  Interface to song meta data in an m4a file.
--
--  References:
--
--  [1] https://docs.fileformat.com/audio/m4a/
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

with Ada.Streams;
with SMM.Metadata;
package SMM.M4a is

   No_Chunk : constant Metadata.ID_String := "    ";

   type Chunk is record
      Label : Metadata.ID_String := No_Chunk;
      Size  : Ada.Streams.Stream_Element_Offset := 0;
   end record;

   procedure Find_Chunk
     (File  : in     SMM.Metadata.File;
      Label : in     SMM.Metadata.ID_String;
      Chunk : in out M4a.Chunk);
   --  Find Label in File.
   --
   --  On entry, if Chunk.Label is No_Chunk, File is before first chunk;
   --  otherwise it is after Chuck.Label.
   --
   --  On return, file is left after Chunk label matching Label.
   --
   --  If Label is not found, SAL.Not_Found is raised.

   procedure Next_Chunk
     (File  : in     SMM.Metadata.File;
      Chunk : in out M4a.Chunk);
   --  On entry, file current position is after Chunk label; advance to
   --  after next chunk label. Chunk is updated to next chunk, if a valid
   --  one is found; otherwise, Chunk.Label is No_Chunk.

   procedure Metadata
     (Abs_File_Name : in     String;
      Frames        :    out SMM.Metadata.Frame_Lists.List;
      Artist_ID     :    out SMM.Metadata.ID_String);
   --  Artist_ID is one of Artist, Album_Artist, whichever is present.

end SMM.M4a;
