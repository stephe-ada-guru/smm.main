--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2011, 2012 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;
procedure SMM.First_Pass
  (Category   : in     String;
   Root_Dir   : in     String;
   File_Count :    out Integer)
is
   Playlist_File_Name : constant String := Category & ".m3u";
   Last_File_Name     : constant String := Category & ".last";
   Target_Dir         : constant String := Category;

   Mentioned_Files : String_Lists.List;

   procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
   is begin
      case Kind (Dir_Entry) is
      when Directory =>
         declare
            Name : constant String := Simple_Name (Dir_Entry);
         begin
            if Name = "." or Name = ".." then
               null;
            else
               Search
                 (Full_Name (Dir_Entry),
                  Pattern => "*",
                  Filter  => (Directory => True, Ordinary_File => True, others => False),
                  Process => Process_Dir_Entry'Access);

               --  FIXME: delete dir if empty
            end if;
         end;

      when Ordinary_File =>
         declare
            Name : constant String := Ada.Characters.Handling.To_Lower
              (Relative_Name (Root_Dir, Normalize (Full_Name (Dir_Entry))));
         begin
            if String_Lists.Contains (Mentioned_Files, Name) then
               if Verbosity > 1 then
                  Put_Line ("keeping " & Name);
               end if;
            else
               if Verbosity > 0 then
                  Put_Line ("deleting " & Name);
               end if;
               if not Debug then
                  Delete_File (Full_Name (Dir_Entry));
               end if;
            end if;
         end;

      when Special_File =>
         raise SAL.Programmer_Error;
      end case;
   end Process_Dir_Entry;

begin
   Ada.Directories.Set_Directory (Root_Dir);

   if Verbosity > 1 then
      Put_Line ("processing directory (phase 1) " & Target_Dir);
   end if;

   if Ada.Directories.Exists (Playlist_File_Name) then
      if Ada.Directories.Exists (Last_File_Name) then
         Edit_Playlist (Playlist_File_Name, Last_File_Name);
      end if;

      Read_Playlist (Playlist_File_Name, Mentioned_Files);
   else
      Put_Line (Playlist_File_Name & " does not exist; it will be created");
   end if;

   --  Search Target_Dir, delete files not in playlist

   if Exists (Target_Dir) then
      begin
         Search
           (Target_Dir,
            Pattern => "*",
            Filter  => (Directory => True, Ordinary_File => True, others => False),
            Process => Process_Dir_Entry'Access);
      exception
      when Ada.Text_IO.Name_Error =>
         raise Ada.Text_IO.Name_Error with Target_Dir;
      end;
   else
      --  A playlist with no corresponding directory, or a
      --  playlist that mentions a directory that has been
      --  deleted.
      Put_Line (Target_Dir & " does not exist");
   end if;

   File_Count := Integer (String_Lists.Length (Mentioned_Files));

   if Verbosity > 1 then
      Put_Line (Integer'Image (File_Count) & " mentioned files");
   end if;
end SMM.First_Pass;
