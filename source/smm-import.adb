--  Abstract :
--
--  Import new files into SMM db.
--
--  Copyright (C) 2008 - 2010, 2012, 2014 Stephen Leake.  All Rights Reserved.
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;
with Ada.Text_IO;
with Interfaces;
with SAL.Config_Files;
procedure SMM.Import
  (Db       : in out SAL.Config_Files.Configuration_Type;
   Category : in String;
   Dir      : in     String)
is
   Index : Interfaces.Unsigned_32;

   Song_Files : String_Maps.Map;
   Pos        : String_Maps.Cursor;
   Inserted   : Boolean;

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => String,
      "=" => "=");

   function Build_Extensions return String_Lists.List
   is
      use String_Lists;
      Result : List;
   begin
      Result.Append ("mp3");
      Result.Append ("flac");
      return Result;
   end Build_Extensions;

   Extensions : constant String_Lists.List := Build_Extensions;

   procedure Get_Initial_Index
   is
      use SAL.Config_Files;
      use type Interfaces.Unsigned_32;
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
      Index := Index + 1;
   end Get_Initial_Index;

   procedure Fill_Song_Files
   is
      use SAL.Config_Files;
      I : Iterator_Type := First (Db, Songs_Key);
   begin
      loop
         exit when Is_Null (I);
         String_Maps.Insert (Song_Files, Normalize (Read (Db, I, File_Key)), I, Pos, Inserted);
         Next (I);
      end loop;

   end Fill_Song_Files;

   procedure Import_Dir (Root : in String; Dir : in String)
   is
      use Ada.Directories;
      use SAL.Config_Files;
      use SAL.Time_Conversions;

      procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
      is
         use type Interfaces.Unsigned_32;
         use type String_Lists.Cursor;

         Name        : constant String := Relative_Name (Root, Normalize (Full_Name (Dir_Entry)));
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
            if Extensions.Find (Extension (Name)) /= String_Lists.No_Element then
               String_Maps.Insert (Song_Files, Name, Null_Iterator, Pos, Inserted);
               if not Inserted then
                  if Verbosity > 1 then
                     Ada.Text_IO.Put_Line ("duplicate: " & Name);
                  end if;
               else
                  if Verbosity > 0 then
                     Ada.Text_IO.Put_Line ("adding file " & Name);
                  end if;
                  Write_String (Db, Songs_Key & "." & Index_Image & "." & File_Key, Name);
                  Write_String (Db, Songs_Key & "." & Index_Image & "." & Category_Key, Category);
                  Write_Last_Downloaded (Db, Songs_Key & "." & Index_Image, 0.0);
                  Index := Index + 1;
               end if;
            end if;

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

   Fill_Song_Files;

   if not SAL.Config_Files.Is_Present (Db, Root_Key) then
      SAL.Config_Files.Write_String (Db, Root_Key, Dir);
   end if;

   Ada.Directories.Set_Directory (SAL.Config_Files.Read (Db, Root_Key));
   Import_Dir (SAL.Config_Files.Read (Db, Root_Key), Dir);
end SMM.Import;
