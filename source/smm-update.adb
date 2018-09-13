--  Abstract :
--
--  Update metadata
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Text_IO;
with SAL;
with SMM.Database;
with SMM.ID3;
procedure SMM.Update
  (DB          : in SMM.Database.Database;
   Source_Root : in String;
   Dir         : in String)
is
   --  Dir relative to Source_Root

   procedure Update_File (Name : in String)
   is
      use SMM.ID3;
      ID3_Frames : Frame_Lists.List;
      Artist_ID  : ID_String := SMM.ID3.Artist;
      I          : Database.Cursor; -- no init for exception handler
   begin
      I := DB.Find_File_Name (Name);
      if not I.Has_Element then
         raise SAL.Not_Found with "not found in db: '" & Name & "'";
      end if;

      if Verbosity > 0 then
         Ada.Text_IO.Put_Line ("updating file " & Name);
      end if;

      Metadata (Source_Root & Name, ID3_Frames, Artist_ID);

      DB.Update
        (I,
         Artist       => -Find (Artist_ID, ID3_Frames),
         Album_Artist => -Find (SMM.ID3.Alt_Artist, ID3_Frames),
         Album        => -Find (SMM.ID3.Album, ID3_Frames),
         Title        => -Find (SMM.ID3.Title, ID3_Frames));
   exception
   when E : SAL.Not_Found =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   when E : others =>
      Ada.Text_IO.Put_Line
        ("file '" & Name & "' exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
           Ada.Exceptions.Exception_Message (E));
   end Update_File;

   procedure Update_Dir (Root : in String; Dir : in String)
   is
      procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
      is
         Abs_Name : constant String := Full_Name (Dir_Entry);
         Name     : constant String := Relative_Name (Root, Normalize (Abs_Name));
         Ext      : constant String := Extension (Name);
      begin
         case Kind (Dir_Entry) is
         when Directory =>
            if Simple_Name (Dir_Entry) = "." or
              Simple_Name (Dir_Entry) = ".."
            then
               return;
            end if;

            Update_Dir (Root, Name);

         when Ordinary_File =>

            if Ext = "mp3" then
               Update_File (Name);
            else
               --  not a recognized music file extension; ignore
               null;
            end if;

         when Special_File =>
            raise SAL.Programmer_Error with "found special file";
         end case;
      end Process_Dir_Entry;

   begin
      if Verbosity > 1 then
         Ada.Text_IO.Put_Line ("reading directory " & Root & Dir);
      end if;

      if Exists (Dir) then
         Search
           (Dir,
            Pattern          => "*",
            Filter           =>
              (Ordinary_File => True,
               Directory     => True,
               Special_File  => False),
            Process          => Process_Dir_Entry'Access);
      else
         Ada.Text_IO.Put_Line (Root & Dir & " does not exist");
      end if;
   end Update_Dir;

begin
   if Exists (Dir) then
      case Kind (Dir) is
      when Directory =>
         Update_Dir (Source_Root, Dir);

      when Ordinary_File =>
         Update_File (Dir);

      when Special_File =>
         raise SAL.Programmer_Error;
      end case;

   else
      raise SAL.Not_Found with "file not found: '" & Dir & "'";
   end if;
end SMM.Update;
