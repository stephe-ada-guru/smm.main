--  Abstract :
--
--  Import new files into SMM db.
--
--  Copyright (C) 2008 - 2010, 2012, 2014, 2018 Stephen Leake.  All Rights Reserved.
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
with SAL;
with SMM.Database;
with SMM.ID3;
procedure SMM.Import
  (DB          : in SMM.Database.Database;
   Source_Root : in String;
   Category    : in String;
   Dir         : in String)
is
   Index : Integer;

   procedure Get_Initial_Index
   is
      use SMM.Database;
      I : constant Cursor := Last (DB);
   begin
      if I.Has_Element then
         Index := I.ID + 1;
      else
         --  Empty db
         Index := 1;
      end if;
   end Get_Initial_Index;

   procedure Import_Dir (Root : in String; Dir : in String)
   is
      use Ada.Directories;
      use SMM.Database;

      procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
      is
         Abs_Name : constant String := Full_Name (Dir_Entry);
         Name     : constant String := Relative_Name (Root, Normalize (Abs_Name));
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

            declare
               use SMM.ID3;
               Ext        : constant String   := Extension (Name);
               ID3_Frames : Frame_Lists.List;
               Artist_ID  : SMM.ID3.ID_String := SMM.ID3.Artist;
            begin
               if Ext = "mp3" then
                  if Verbosity > 0 then
                     Ada.Text_IO.Put_Line ("adding file " & Name);
                  end if;

                  Metadata (Abs_Name, ID3_Frames, Artist_ID);

                  DB.Insert
                    (ID        => Index,
                     File_Name => Name,
                     Category  => Category,
                     Artist    => -Find (Artist_ID, ID3_Frames),
                     Album     => -Find (SMM.ID3.Album, ID3_Frames),
                     Title     => -Find (SMM.ID3.Title, ID3_Frames),
                     Track     => SMM.ID3.To_Track (-Find (SMM.ID3.Track, ID3_Frames)));

                  Index := Index + 1;
               else
                  --  not a recognized music file extension; ignore
                  null;
               end if;
            end;

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

   Import_Dir (Source_Root, Dir);
end SMM.Import;
