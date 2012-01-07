--  Abstract :
--
--  download files to a music player
--
--  Copyright (C) 2008 - 2009, 2011, 2012 Stephen Leake.  All Rights Reserved.
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
with Ada.Directories;
with Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Config_Files;
with SAL.Time_Conversions;
procedure SMM.Download
  (Db          : in out SAL.Config_Files.Configuration_Type;
   Category    : in String;
   Destination : in String;
   Song_Count  : in Integer)
is
   use Song_Lists;
   Songs       : List_Type;
   I           : Iterator_Type;
   Count       : Integer         := 0;
   Source_Root : constant String := SAL.Config_Files.Read (Db, Root_Key);

   Download_Time : constant String := SAL.Time_Conversions.Time_Type'Image
     (SAL.Time_Conversions.To_Time (Ada.Real_Time.Clock));

begin
   if not Ada.Directories.Exists (Destination) then
      Put_Line ("creating directory " & Destination);
      Ada.Directories.Create_Directory (Destination);
   end if;

   Least_Recent_Songs (Db, Category, Songs, Song_Count);

   I := First (Songs);
   loop
      exit when Is_Null (I);
      declare
         Source : constant String := Source_Root & SAL.Config_Files.Read (Db, Current (I), File_Key);
         Target : constant String := Destination & Ada.Directories.Simple_Name (Source);
      begin
         if Verbosity > 0 then
            Put_Line ("downloading " & Source);
            Put_Line ("to          " & Target);
         else
            if Count mod 10 = 0 then
               New_Line;
            end if;
            Put (".");
         end if;

         Ada.Directories.Copy_File
           (Source_Name => Source,
            Target_Name => Target);

         SAL.Config_Files.Write (Db, Current (I), Last_Downloaded_Key, Download_Time);

         Next (I);
         Count := Count + 1;
      end;
   end loop;
exception
when Ada.IO_Exceptions.Use_Error =>
   --  Just stop downloading; nothing else we can do.
   Put_Line (Destination & " Use_Error; probably disk full");
end SMM.Download;
