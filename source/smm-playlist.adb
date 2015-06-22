--  Abstract :
--
--  create a playlist of least-recenty heard songs
--
--  Copyright (C) 2009, 2010, 2012, 2015 Stephen Leake.  All Rights Reserved.
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
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Time_Conversions;
procedure SMM.Playlist
  (Db             : in out SAL.Config_Files.Configuration_Type;
   Category       : in     String;
   Destination    : in     String;
   Replace        : in     Boolean;
   Max_Song_Count : in     Ada.Containers.Count_Type;
   New_Song_Count : in     Ada.Containers.Count_Type)
is
   use Song_Lists;

   Playlist_File : Ada.Text_IO.File_Type;

   Songs : List;

   I               : Cursor;
   Source_Root     : constant String := SAL.Config_Files.Read (Db, Root_Key);
   Relative        : Boolean;
   Relative_Prefix : Ada.Strings.Unbounded.Unbounded_String;

   Output_Time : constant SAL.Time_Conversions.Time_Type := SAL.Time_Conversions.To_Time (Ada.Real_Time.Clock);
begin
   declare
      use Ada.Directories;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      Mode       : File_Mode;
      --  Full_Name normalizes directory separators
      Containing : constant String  := Full_Name (Containing_Directory (Destination));
      Full_Root  : constant String  := Full_Name (Source_Root);
      Found      : constant Integer := Index (Source => Containing, Pattern => Full_Root);
   begin
      Relative := Found = 1 and Full_Root'Length /= Containing'Length;

      if Relative then
         declare
            use Ada.Strings.Unbounded;
            use Ada.Strings.Maps;
            Containing_Last : Integer                    := Containing'Last + 1;
            Dir_Sep         : constant Character_Mapping := To_Mapping ("/\", "//");
         begin
            Build_Prefix :
            loop
               Containing_Last := Index
                 (Source  => Containing (Full_Root'Length .. Containing_Last - 1),
                  Pattern => "/",
                  Mapping => Dir_Sep,
                  Going   => Backward);

               if Containing_Last > 0 then
                  Relative_Prefix := Relative_Prefix & "../";
               else
                  exit Build_Prefix;
               end if;
            end loop Build_Prefix;
         end;
      end if;

      if Replace then
         Mode := Out_File;
      else
         Mode := Append_File;
      end if;

      if Exists (Destination) then
         Open (Playlist_File, Mode, Destination);
      else
         Create (Playlist_File, Out_File, Destination);
      end if;
   end;

   Least_Recent_Songs (Db, Category, Song_Count => Max_Song_Count, New_Song_Count => New_Song_Count, Songs => Songs);

   I := First (Songs);
   loop
      exit when I = No_Element;
      declare
         use Ada.Strings.Unbounded;
         Source : constant String := SAL.Config_Files.Read (Db, Element (I), File_Key);
      begin
         if Relative then
            Ada.Text_IO.Put_Line (Playlist_File, To_String (Relative_Prefix) & Source);
         else
            Ada.Text_IO.Put_Line (Playlist_File, Source_Root & Source);
         end if;

         Write_Last_Downloaded (Db, Element (I), Output_Time);

         Next (I);
      end;
   end loop;

   Ada.Text_IO.Close (Playlist_File);
exception
when others =>
   if Ada.Text_IO.Is_Open (Playlist_File) then
      Ada.Text_IO.Close (Playlist_File);
   end if;
   raise;
end SMM.Playlist;
