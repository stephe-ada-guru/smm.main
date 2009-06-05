--  Abstract :
--
--  Import new files into SMM db.
--
--  Copyright (C) 2008, 2009 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Directories;
with Ada.Text_IO;
with Interfaces;
with SAL.Config_Files;
with SAL.Time_Conversions;
procedure SMM.Import
  (Db  : in out SAL.Config_Files.Configuration_Type;
   Dir : in     String)
is
   Index : Interfaces.Unsigned_32;

   procedure Get_Initial_Index
   is
      use SAL.Config_Files;
      I : Iterator_Type;
   begin
      begin
         I := First (Db, Songs_Key);
      exception
      when SAL.Config_File_Error =>
         --  empty db
         Index := 1;
         return;
      end;

      loop
         exit when Is_Null (I);
         Index := Interfaces.Unsigned_32'Value (Current (I));
         Next (I);
      end loop;
   end Get_Initial_Index;

   procedure Import_Dir (Root : in String; Dir : in String)
   is
      use Ada.Directories;
      use SAL.Config_Files;
      use SAL.Time_Conversions;

      procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
      is
         use type Interfaces.Unsigned_32;
         Name        : constant String := Relative_Name_Sans_Extension (Root, Full_Name (Dir_Entry));
         Index_Image : constant String := Interfaces.Unsigned_32'Image (Index);
      begin
         case Kind (Dir_Entry) is
         when Directory =>
            if Simple_Name (Dir_Entry) = "." or
              Simple_Name (Dir_Entry) = ".."
            then
               return;
            end if;

            Import_Dir (Root, Name);

         when Ordinary_File =>
            --  FIXME: read filenames into sorted tree for duplicate detection
            if Verbosity > 0 then
               Ada.Text_IO.Put_Line ("adding file " & Name);
            end if;
            Write_String
              (Db, Songs_Key & "." & Index_Image & "." & File_Key, Name & "." & Extension (Full_Name (Dir_Entry)));
            Write_String (Db, Songs_Key & "." & Index_Image & "." & Last_Downloaded_Key, Time_Type'Image (0.0));
            Index := Index + 1;

         when Special_File =>
            raise SAL.Programmer_Error with "found special file";
         end case;
      end Process_Dir_Entry;
   begin
      if Verbosity > 1 then
         Ada.Text_IO.Put_Line ("reading directory " & Root & Dir);
      end if;

      if Exists (Dir) then
         Search
           (Dir,
            Pattern          => "*",
            Filter           =>
              (Ordinary_File => True,
               Directory     => True,
               Special_File  => False),
            Process          => Process_Dir_Entry'Access);
      else
         Ada.Text_IO.Put_Line (Root & Dir & " does not exist");
      end if;
   end Import_Dir;

begin
   Get_Initial_Index;
   if not SAL.Config_Files.Is_Present (Db, Root_Key) then
      SAL.Config_Files.Write_String (Db, Root_Key, Dir);
   end if;

   Ada.Directories.Set_Directory (SAL.Config_Files.Read (Db, Root_Key));
   Import_Dir (SAL.Config_Files.Read (Db, Root_Key), Dir);
end SMM.Import;
