--  Abstract :
--
--  See below
--
--  Copyright (C) 2012, 2015 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

procedure SMM.Download
  (Db             : in out SAL.Config_Files.Configuration_Type;
   Category       : in     String;
   Destination    : in     String;
   Song_Count     : in     Ada.Containers.Count_Type;
   New_Song_Count : in     Ada.Containers.Count_Type;
   Seed           : in     Integer := 0);

--  Download Song_Count least-recently-played files, including about
--  New_Song_Count / 2 new songs (if any) to Destination directory,
--  add to playlist.
--
--  Destination is an absolute path to the download directory (ends in
--  '/'). Playlist is in Destination with name <Category>.m3u; songs
--  are stored in subdirs named <Category>.
--
--  Seed is passed to SMM.Least_Recent_Songs.
