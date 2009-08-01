--  Abstract :
--
--  download files to a music player
--
--  Copyright (C) 2008 - 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Real_Time;
with Ada.Directories;
with Ada.Text_IO;
with SAL.Config_Files;
with SAL.Time_Conversions;
procedure SMM.Download
  (Db          : in out SAL.Config_Files.Configuration_Type;
   Category    : in String;
   Destination : in String)
is
   use Song_Lists;
   Songs       : List_Type;
   I           : Iterator_Type;
   Count       : Integer         := 0;
   Source_Root : constant String := SAL.Config_Files.Read (Db, Root_Key);

   Download_Time : constant String := SAL.Time_Conversions.Time_Type'Image
     (SAL.Time_Conversions.To_Time (Ada.Real_Time.Clock));

begin
   Least_Recent_Songs (Db, Category, Song_Count => Download_File_Count, Songs => Songs);

   I := First (Songs);
   loop
      exit when Is_Null (I);
      declare
         Source : constant String := Source_Root & SAL.Config_Files.Read (Db, Current (I), File_Key);
         Target : constant String := Destination & Ada.Directories.Simple_Name (Source);
      begin
         if Verbosity > 0 then
            Ada.Text_IO.Put_Line ("downloading " & Source);
            Ada.Text_IO.Put_Line ("to          " & Target);
         else
            if Count mod 10 = 0 then
               Ada.Text_IO.New_Line;
            end if;
            Ada.Text_IO.Put (".");
         end if;

         Ada.Directories.Copy_File
           (Source_Name => Source,
            Target_Name => Target);

         SAL.Config_Files.Write (Db, Current (I), Last_Downloaded_Key, Download_Time);

         Next (I);
         Count := Count + 1;
      end;
   end loop;
end SMM.Download;
