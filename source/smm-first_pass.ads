--  Abstract :
--
--  First pass of playlists.
--
--  Copyright (C) 2007, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

procedure SMM.First_Pass
  (Category   : in     String;
   Root_Dir   : in     String;
   File_Count :    out Integer);
--  Delete files from Root_Dir that are not mentioned in playlist file
--  "<category>.m3u". Return count of files remaining in playlist.

