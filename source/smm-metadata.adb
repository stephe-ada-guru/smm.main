--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SMM.ID3;
package body SMM.Metadata is

   ----------
   --  Public subprograms

   procedure Read_Meta
     (File_Name : in     String;
      Artist    :    out Ada.Strings.Unbounded.Unbounded_String;
      Album     :    out Ada.Strings.Unbounded.Unbounded_String;
      Title     :    out Ada.Strings.Unbounded.Unbounded_String)
   is
      File : SMM.ID3.File;
   begin
      File.Open (File_Name);
      Artist := To_Unbounded_String (File.Read (SMM.ID3.Artist));
      Album  := To_Unbounded_String (File.Read (SMM.ID3.Album));
      Title  := To_Unbounded_String (File.Read (SMM.ID3.Title));
   end Read_Meta;

end SMM.Metadata;
