--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008, 2009, 2011 - 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with SAL;
package body SMM is

   function Find_Home return String
   is
      use Ada.Environment_Variables;
   begin
      if Exists ("SMM_HOME") then
         return Value ("SMM_HOME");
      elsif Exists ("HOME") then
         return Value ("HOME") & "/smm";
      elsif Exists ("APPDATA") then
         return Value ("APPDATA") & "/smm";
      else
         raise SAL.Not_Found with "must define either APPDATA or HOME environment variable";
      end if;
   end Find_Home;

   function Normalize (Path : in String) return String
   is
      Result : String := Path;
   begin
      for I in Result'Range loop
         if Result (I) = '\' then
            Result (I) := '/';
         end if;
      end loop;
      return Result;
   end Normalize;

   function Relative_Name
     (Root      : in String;
      Full_Name : in String)
      return String
   is
      Dir_Root : constant String := As_Directory (Root);
   begin
      if Dir_Root = Full_Name (Full_Name'First .. Full_Name'First + Dir_Root'Length - 1) then
         return Full_Name (Full_Name'First + Dir_Root'Length .. Full_Name'Last);
      else
         raise SAL.Programmer_Error with Full_Name & " not relative to root " & Dir_Root;
      end if;
   end Relative_Name;

   function As_Directory (Path : in String) return String
   is
      Temp : constant String := Normalize (Path);
   begin
      if Temp (Temp'Last) = '/' then
         return Temp;
      else
         return Temp & '/';
      end if;
   end As_Directory;

   function As_File (Path : in String) return String
   is
      Temp : constant String := Normalize (Path);
   begin
      if Path'Length = 0 then
         return Path;
      end if;

      if Temp (Temp'Last) = '/' then
         return Temp (Temp'First .. Temp'Last - 1);
      else
         return Temp;
      end if;
   end As_File;

   procedure Edit_Playlist
     (Playlist_File_Name : in String;
      Last_File_Name     : in String)
   is
      use Ada.Directories;
      use Ada.Text_IO;

      Output_File_Name : constant String := Playlist_File_Name & ".tmp";

      Input_File  : File_Type;
      Output_File : File_Type;
   begin
      if Verbosity > 1 then
         Put_Line ("editing playlist " & Playlist_File_Name);
      end if;

      begin
         Open (Input_File, In_File, Last_File_Name);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line ("last file " & Last_File_Name & " cannot be opened");
         raise;
      end;

      --  special case; empty last file; nothing to do
      if End_Of_Line (Input_File) then
         Close (Input_File);
         Delete_File (Last_File_Name);
         return;
      end if;

      begin
         Create (Output_File, Out_File, Output_File_Name);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line ("tmp file " & Output_File_Name & " cannot be opened");
         raise;
      end;

      declare
         Last_Played : constant String := Get_Line (Input_File);
         Found       : Boolean         := False;
      begin
         Close (Input_File);

         begin
            Open (Input_File, In_File, Playlist_File_Name);
         exception
         when Ada.IO_Exceptions.Name_Error =>
            Put_Line ("playlist file " & Playlist_File_Name & " cannot be opened");
            raise;
         end;

         loop
            exit when End_Of_File (Input_File);

            declare
               Line : constant String := Get_Line (Input_File);
            begin
               if Found then
                  Put_Line (Output_File, Line);
               else
                  Found := Line = Last_Played;
               end if;
            end;
         end loop;
         Close (Input_File);
         Close (Output_File);
      end;

      Delete_File (Playlist_File_Name);
      Rename (Output_File_Name, Playlist_File_Name);
      Delete_File (Last_File_Name);

   end Edit_Playlist;

   procedure Read_Playlist
     (File_Name  : in     String;
      Files      :    out String_Lists.List)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      if Verbosity > 1 then
         Put_Line ("reading playlist " & File_Name);
      end if;

      begin
         Open (File, In_File, File_Name);
      exception
      when Name_Error =>
         Put_Line ("playlist file " & File_Name & " cannot be opened");
         raise;
      end;

      --  special case; empty file
      if End_Of_File (File) then
         Close (File);

         Files := String_Lists.Empty_List;

         return;
      end if;

      loop -- exit on End_Error
         declare
            use Ada.Directories;
            Name : constant String := Get_Line (File);
         begin
            String_Lists.Append (Files, Ada.Characters.Handling.To_Lower (Name));
         end;
      end loop;
   exception
   when End_Error =>
      Close (File);
   end Read_Playlist;

end SMM;
