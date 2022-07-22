--  Abstract :
--
--  See spe.
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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
package body SMM.Metadata is

   overriding procedure Finalize (File : in out SMM.Metadata.File)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream_File) then
         Close (File.Stream_File);
      end if;
   end Finalize;

   procedure Open (File : in out SMM.Metadata.File; Name : in String)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream_File) then
         raise Ada.IO_Exceptions.Use_Error with "file is already open with '" &
           Ada.Streams.Stream_IO.Name (File.Stream_File) & "'";
      end if;

      Open (File.Stream_File, In_File, Name);
   end Open;

   procedure Close (File : in out SMM.Metadata.File)
   is
      use Ada.Streams.Stream_IO;
   begin
      if Is_Open (File.Stream_File) then
         Close (File.Stream_File);
      end if;
   end Close;

   function End_Of_File (File : in SMM.Metadata.File) return Boolean
   is
      use Ada.Streams.Stream_IO;
   begin
      return End_Of_File (File.Stream_File);
   end End_Of_File;

   function Index (File : in SMM.Metadata.File) return Ada.Streams.Stream_IO.Positive_Count
   is
      use Ada.Streams.Stream_IO;
   begin
      return Index (File.Stream_File);
   end Index;

   function Name (File : in SMM.Metadata.File) return String
   is
      use Ada.Streams.Stream_IO;
   begin
      return Name (File.Stream_File);
   end Name;

   function Stream (File : in SMM.Metadata.File) return access Ada.Streams.Root_Stream_Type'Class
   is begin
      return Ada.Streams.Stream_IO.Stream (File.Stream_File);
   end Stream;

   procedure Read
     (File : in     SMM.Metadata.File;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is begin
      Ada.Streams.Stream_IO.Read (File.Stream_File, Item, Last);
   end Read;

   function To_Year (Orig_Year, Year, Recording_Time : in String) return Integer
   is begin
      if Orig_Year'Length > 0 then
         return Integer'Value (Orig_Year);
      end if;
      if Year'Length > 0 then
         return Integer'Value (Year);
      end if;
      if Recording_Time'Length > 0 then
         return Integer'Value (Recording_Time);
      end if;
      return -1;
   end To_Year;

   function To_Track (Item : in String) return Integer
   is
      use Ada.Strings.Fixed;
      Slash_Index : constant Integer := Index (Item, "/");
   begin
      if Item'Length = 0 then
         return -1;
      elsif Slash_Index = 0 then
         return Integer'Value (Item);
      else
         return Integer'Value (Item (Item'First .. Slash_Index - 1));
      end if;
   end To_Track;

   function Find (ID : in ID_String; Frames : in Frame_Lists.List) return Ada.Strings.Unbounded.Unbounded_String
   is begin
      for F of Frames loop
         if ID = F.ID then
            return F.Data;
         end if;
      end loop;
      return +"";
   end Find;

end SMM.Metadata;
