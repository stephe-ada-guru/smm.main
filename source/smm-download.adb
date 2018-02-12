--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008 - 2009, 2011 - 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Calendar;
with Ada.Directories; use Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;
with SMM.Database;
with SMM.Song_Lists;
procedure SMM.Download
  (DB                : in SMM.Database.Database;
   Source_Root       : in String;
   Category          : in String;
   Destination       : in String;
   Song_Count        : in Ada.Containers.Count_Type;
   New_Song_Count    : in Ada.Containers.Count_Type;
   Over_Select_Ratio : in Float;
   Download_Time     : in SMM.Database.Time_String := SMM.Database.UTC_Image (Ada.Calendar.Clock);
   Seed              : in Integer                  := 0)
is
   use Ada.Containers;
   use SMM.Song_Lists;
   use SMM.Song_Lists.Song_Lists;
   Songs        : List;
   I            : Cursor;
   Count        : Integer         := 0;
   Category_Dir : constant String := Destination & Category & '/';

   Playlist_File_Name : constant String := Destination & Category & ".m3u";
   Playlist_File      : File_Type;

   procedure Download_Album_Aux (Source_Dir, Target_Dir : in String)
   is
      procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
      is begin
         Copy_File
           (Source_Name => Full_Name (Dir_Ent),
            Target_Name => Compose (Target_Dir, Simple_Name (Dir_Ent)));
      exception
      when Ada.IO_Exceptions.Use_Error =>
         --  nothing else we can do.
         Put_Line (Standard_Error, Destination & " Copy_Aux Use_Error; probably disk full");
         return;
      end Copy_Aux;

   begin
      Search
        (Directory => Source_Dir,
         Pattern   => "AlbumArt*.jpg",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      Search
        (Directory => Source_Dir,
         Pattern   => "liner_notes.pdf",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);
   end Download_Album_Aux;

begin
   if not Exists (Destination) then
      Put_Line ("creating directory " & Destination);
      Create_Directory (Destination);
   end if;

   if not Ada.Directories.Exists (Playlist_File_Name) then
      Put_Line ("creating playlist file " & Playlist_File_Name);
      begin
         Create (Playlist_File, Out_File, Playlist_File_Name);
      exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("playlist file " & Playlist_File_Name & " cannot be created");
         raise;
      end;

   else
      begin
         Open (Playlist_File, Append_File, Playlist_File_Name);
      exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("playlist file " & Playlist_File_Name & " cannot be opened");
         raise;
      end;
   end if;

   Least_Recent_Songs (DB, Category, Songs, Song_Count, New_Song_Count, Over_Select_Ratio, Seed => Seed);

   Put_Line ("downloading" & Count_Type'Image (Songs.Length) & " songs");

   I := First (Songs);
   loop
      exit when I = No_Element;
      declare
         use SMM.Database;
         Cursor : constant SMM.Database.Cursor := SMM.Database.Find_ID (DB, Element (I));

         Relative   : constant String := Cursor.File_Name;
         Source     : constant String := Source_Root & Relative;
         Target     : constant String := Category_Dir & Relative;
         Target_Dir : constant String := Containing_Directory (Target);

         Last_Downloaded : constant Time_String := Cursor.Last_Downloaded;
         Prev_Downloaded : constant Time_String := Cursor.Prev_Downloaded;
      begin
         if not Exists (Source) then
            --  Bad file name in db file
            Put_Line (Standard_Error, "File not found: '" & Source & "'");

         else
            Put_Line (Last_Downloaded & ", " & Prev_Downloaded & " : " & Relative);

            if not Exists (Target_Dir) then
               begin
                  Create_Path (Target_Dir);
                  Download_Album_Aux (Containing_Directory (Source), Target_Dir);
               exception
               when Ada.IO_Exceptions.Use_Error =>
                  Put_Line ("can't create directory " & Target_Dir);
               end;
            end if;

            begin
               Copy_File
                 (Source_Name => Source,
                  Target_Name => Target);
            exception
            when Ada.IO_Exceptions.Use_Error =>
               --  Just stop downloading; nothing else we can do.
               Close (Playlist_File);
               Put_Line (Standard_Error, Destination & " Use_Error; probably disk full");
               return;
            end;

            Put_Line (Playlist_File, Relative_Name (Destination, Target));

            Cursor.Write_Last_Downloaded (DB, Download_Time);
         end if;

         Next (I);
         Count := Count + 1;
      end;
   end loop;

   Close (Playlist_File);

end SMM.Download;
