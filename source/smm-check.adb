--  Abstract :
--
--  Check against db, report missing in either.
--
--  Copyright (C) 2016 - 2017 Stephen Leake.  All Rights Reserved.
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
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Config_Files;
procedure SMM.Check (Db : in out SAL.Config_Files.Configuration_Type)
is
   Db_Count   : Integer := 0;
   Disk_Count : Integer := 0;

   Db_Root : constant String := SAL.Config_Files.Read (Db, Root_Key);

   Song_Files : String_Maps.Map; -- Searchable list of all song titles in db

   procedure Fill_Song_Files
   is
      use String_Maps;
      use SAL.Config_Files;
      I        : Iterator_Type := First (Db, Songs_Key);
      Pos      : String_Maps.Cursor;
      Inserted : Boolean;
   begin
      loop
         exit when Is_Null (I);
         String_Maps.Insert (Song_Files, Normalize (Read (Db, I, File_Key)), I, Pos, Inserted);
         if not Inserted then
            Put_Line ("db duplicate: " & Key (Pos));
         end if;
         Next (I);
      end loop;

   end Fill_Song_Files;

   procedure Check_Dir (Dir : in String)
   is
      use Ada.Directories;
      use SAL.Config_Files;
      use SAL.Time_Conversions;

      Found_Mp3           : Boolean := False;
      Found_Liner_Notes   : Boolean := False;
      Found_AlbumArt_Huge : Boolean := False;

      procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
      is
         use String_Maps;
         use Ada.Strings.Fixed;

         Name : constant String := Relative_Name (Db_Root, Normalize (Full_Name (Dir_Entry)));
      begin
         case Kind (Dir_Entry) is
         when Directory =>
            if Simple_Name (Dir_Entry) = "." or
              Simple_Name (Dir_Entry) = ".."
            then
               return;
            end if;

            Check_Dir (Name);

         when Ordinary_File =>
            if Extension (Name) = "mp3" then
               Disk_Count := Disk_Count + 1;
               Found_Mp3 := True;

               if No_Element = Song_Files.Find (Name) then
                  Put_Line ("db missing: " & Name);
               end if;

            elsif 0 < Index (Name, "liner_notes.pdf") then
               Found_Liner_Notes := True;

            elsif 0 < Index (Name, "AlbumArt_huge") then
               Found_AlbumArt_Huge := True;
            end if;

         when Special_File =>
            raise SAL.Programmer_Error with "found special file";
         end case;
      end Process_Dir_Entry;
   begin
      if Verbosity > 0 then
         Put_Line ("checking directory " & Dir);
      end if;

      Search
        (Dir,
         Pattern          => "*",
         Filter           =>
           (Ordinary_File => True,
            Directory     => True,
            Special_File  => False),
         Process          => Process_Dir_Entry'Access);

      if Found_Mp3 then
         if not Found_Liner_Notes then
            Put_Line ("liner_notes missing  : " & Dir);
         end if;
         if not Found_AlbumArt_Huge then
            Put_Line ("AlbumArt_huge missing: " & Dir);
         end if;
      end if;
   end Check_Dir;

begin
   Fill_Song_Files;

   Check_Dir (Db_Root);

   Put_Line ("Disk files:" & Integer'Image (Disk_Count));

   for I of Song_Files loop
      Db_Count := Db_Count + 1;
      declare
         File : constant String := SAL.Config_Files.Read (Db, I, File_Key);
      begin
         if not Ada.Directories.Exists (File) then
            Put_Line ("db extra: " & File);
         end if;
      end;
   end loop;

   Put_Line ("db files:" & Integer'Image (Db_Count));
end SMM.Check;
