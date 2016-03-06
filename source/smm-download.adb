--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008 - 2009, 2011 - 2016 Stephen Leake.  All Rights Reserved.
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

with Ada.IO_Exceptions;
with Ada.Calendar;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;
with SAL.Config_Files;
with SAL.Time_Conversions;
procedure SMM.Download
  (Db             : in out SAL.Config_Files.Configuration_Type;
   Category       : in     String;
   Destination    : in     String;
   Song_Count     : in     Ada.Containers.Count_Type;
   New_Song_Count : in     Ada.Containers.Count_Type;
   Seed           : in     Integer := 0)
is
   use Ada.Containers;
   use Song_Lists;
   Songs        : List;
   I            : Cursor;
   Count        : Integer         := 0;
   Source_Root  : constant String := SAL.Config_Files.Read (Db, Root_Key);
   Category_Dir : constant String := Destination & Category & '/';

   Download_Time : constant SAL.Time_Conversions.Time_Type := SAL.Time_Conversions.To_TAI_Time (Ada.Calendar.Clock);

   Playlist_File_Name : constant String := Destination & Category & ".m3u";
   Playlist_File      : File_Type;

   procedure Download_Album_Art (Source_Dir, Target_Dir : in String)
   is
      procedure Copy_Art (Dir_Ent : in Directory_Entry_Type)
      is begin
         Copy_File
           (Source_Name => Full_Name (Dir_Ent),
            Target_Name => Compose (Target_Dir, Simple_Name (Dir_Ent)));
      exception
      when Ada.IO_Exceptions.Use_Error =>
         --  nothing else we can do.
         Put_Line (Standard_Error, Destination & " Copy_Art Use_Error; probably disk full");
         return;
      end Copy_Art;

   begin
      Search
        (Directory => Source_Dir,
         Pattern   => "*.jpg",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Art'Access);
   end Download_Album_Art;

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

   Least_Recent_Songs (Db, Category, Songs, Song_Count, New_Song_Count, Seed => Seed);

   Put_Line ("downloading" & Count_Type'Image (Songs.Length) & " songs");

   I := First (Songs);
   loop
      exit when I = No_Element;
      declare
         use type SAL.Time_Conversions.Time_Type;
         Relative   : constant String := SAL.Config_Files.Read (Db, Element (I), File_Key);
         Source     : constant String := Source_Root & Relative;
         Target     : constant String := Category_Dir & Relative;
         Target_Dir : constant String := Containing_Directory (Target);

         Last_Downloaded : constant SAL.Time_Conversions.Time_Type := Read_Last_Downloaded (Db, Element (I));
         Prev_Downloaded : constant SAL.Time_Conversions.Time_Type := Read_Prev_Downloaded (Db, Element (I));
      begin
         Put_Line (To_String (Last_Downloaded) & ", " & To_String (Prev_Downloaded) & " : " & Relative);

         if not Exists (Target_Dir) then
            begin
               Create_Path (Target_Dir);
               Download_Album_Art (Containing_Directory (Source), Target_Dir);
            exception
            when Ada.IO_Exceptions.Use_Error =>
               Put_Line ("can't create directory " & Target_Dir);
            end;
         end if;

         if not Exists (Source) then
            --  Bad file name in db file
            Put_Line (Standard_Error, "File not found: '" & Source & "'");
         else

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

            Write_Last_Downloaded (Db, Element (I), Download_Time);
         end if;

         Next (I);
         Count := Count + 1;
      end;
   end loop;

   Close (Playlist_File);

end SMM.Download;
