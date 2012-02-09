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

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;
procedure SMM.Second_Pass (Category, Root_Dir : in String)
is
   Playlist_File_Name : constant String := Category & ".m3u";
   Target_Dir         : constant String := Category;

   Mentioned_Files : String_Lists.List;

   File : File_Type;

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
            end if;
         end;

      when Ordinary_File =>
         declare
            Name : constant String := Relative_Name (Root_Dir, Normalize (Full_Name (Dir_Entry)));
         begin
            if not String_Lists.Contains (Mentioned_Files, Name) then
               if Verbosity > 0 then
                  Put_Line ("adding " & Name);
               end if;
               if not Debug then
                  Put_Line (File, Name);
               end if;
            end if;
         end;

      when Special_File =>
         raise SAL.Programmer_Error;
      end case;
   end Process_Dir_Entry;

begin
   if Verbosity > 1 then
      Put_Line ("adding directory " & Target_Dir);
   end if;

   Ada.Directories.Set_Directory (Root_Dir);

   if not Ada.Directories.Exists (Playlist_File_Name) then
      Put_Line ("creating playlist file " & Playlist_File_Name);
      declare
         File : File_Type;
      begin
         Create (File, Out_File, Playlist_File_Name);
         Close (File);
      end;
   end if;

   Read_Playlist (Playlist_File_Name, Mentioned_Files);

   if not Debug then
      begin
         Open (File, Append_File, Playlist_File_Name);
      exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("playlist file " & Playlist_File_Name & " cannot be opened");
         raise;
      end;
   end if;

   Search
     (Target_Dir,
      Pattern => "*",
      Filter  => (Directory => True, Ordinary_File => True, others => False),
      Process => Process_Dir_Entry'Access);

   if not Debug then
      Close (File);
   end if;

end SMM.Second_Pass;
