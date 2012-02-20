--  Abstract :
--
--  main procedure for SMM application
--
--  Copyright (C) 2008 - 2012 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Command_Line_IO;
with SAL.Config_Files;
with SMM.Download;
with SMM.First_Pass;
with SMM.Import;
with SMM.Playlist;
procedure SMM.Driver
is
   procedure Put_Usage
   is begin
      Put_Line
        ("smm [--db=<db_file>] [--verbosity=<int>] [--max-song-count=<int>] [--min-download-count=<int>]" &
           " [--debug] <operation> [arg]...");
      Put_Line ("  <db_file> : defaults to ~/.smm/smm.db or $APPDATA/smm or $SMM_HOME");
      Put_Line ("  categories: {instrumental | vocal | ...}");
      Put_Line ("  operations:");
      Put_Line ("  download_playlist <category> <target_dir>");
      Put_Line ("    manage downloaded files and playlist:");
      Put_Line ("    1) delete files in target_dir either not in playlist target_dir/../<category>.m3u,");
      Put_Line ("       or already played (indicated in target_dir/../<category>.last)");
      Put_Line ("    2) download default amount of music to target_dir");
      Put_Line ("    3) delete played files, add new files to playlist, delete target_dir/../<category>.last");
      New_Line;
      Put_Line ("  playlist <category> [<file>] [--replace]");
      Put_Line ("    create a playlist in <file> (same songs as 'download' would do)");
      Put_Line ("    <file> default specified in smm.db by Playlists key");
      Put_Line ("    --replace - overwrite file; otherwise append");
      Put_Line ("    if <file> is in database root, paths in playlist are relative");
      New_Line;
      Put_Line ("  import <category> <dir>");
      Put_Line ("    scan <dir> for new music; dir must be relative to database root dir");
   end Put_Usage;

   Db_File_Name : access String;
   Db           : SAL.Config_Files.Configuration_Type;
   Next_Arg     : Integer := 1;

   Max_Song_Count     : Integer;
   Min_Download_Count : Integer;

   function Find_Home return String
   is
      use Ada.Environment_Variables;
   begin
      if Exists ("SMM_HOME") then
         return Value ("SMM_HOME");
      elsif Exists ("HOME") then
         return Value ("HOME") & "/.smm";
      elsif Exists ("APPDATA") then
         return Value ("APPDATA") & "/smm";
      else
         raise Playlist_Error with "must define either APPDATA or HOME environment variable";
      end if;
   end Find_Home;

   Home : constant String := Find_Home;

   type Command_Type is (Download_Playlist, Playlist, Import);

   procedure Get_Command is new SAL.Command_Line_IO.Gen_Get_Discrete (Command_Type, "command", Next_Arg);

   Command : Command_Type;
begin
   if Argument (Next_Arg)'Length > 5 and then
     Argument (Next_Arg)(1 .. 5) = "--db="
   then
      Db_File_Name := new String'(Argument (Next_Arg)(6 .. Argument (Next_Arg)'Last));
      Next_Arg := Next_Arg + 1;
   else
      Db_File_Name := new String'(Home & "/smm.db");
   end if;

   if Argument (Next_Arg)'Length > 12 and then
     Argument (Next_Arg)(1 .. 12) = "--verbosity="
   then
      Verbosity := Integer'Value (Argument (Next_Arg)(13 .. Argument (Next_Arg)'Last));
      Next_Arg := Next_Arg + 1;
   else
      Verbosity := 0;
   end if;

   if Argument (Next_Arg)'Length > 17 and then
     Argument (Next_Arg)(1 .. 17) = "--max-song-count="

   then
      Max_Song_Count := Integer'Value (Argument (Next_Arg)(18 .. Argument (Next_Arg)'Last));
      Next_Arg       := Next_Arg + 1;
   else
      Max_Song_Count := 60;
   end if;

   if Argument (Next_Arg)'Length > 21 and then
     Argument (Next_Arg)(1 .. 21) = "--min-download-count="

   then
      Min_Download_Count := Integer'Value (Argument (Next_Arg)(22 .. Argument (Next_Arg)'Last));
      Next_Arg           := Next_Arg + 1;
   else
      Min_Download_Count := 30;
   end if;

   if Argument (Next_Arg) = "--debug" then

      Debug    := True;
      Next_Arg := Next_Arg + 1;
   else
      Debug := False;
   end if;

   SAL.Config_Files.Open
     (Db,
      Db_File_Name.all,
      Duplicate_Key         => SAL.Config_Files.Raise_Exception,
      Read_Only             => False,
      Case_Insensitive_Keys => True);

   begin
      Get_Command (Command);
   exception
   when SAL.Parameter_Error =>
      Put_Usage;
      return;
   end;

   case Command is
   when Download_Playlist =>
      declare
         Category   : constant String := Argument (Next_Arg);
         Root_Dir   : constant String := As_Directory (Argument (Next_Arg + 1));
         Song_Count : Integer;
      begin
         Verbosity := Verbosity + 1;
         SMM.First_Pass (Category, Root_Dir, Song_Count);
         if Max_Song_Count - Song_Count >= Min_Download_Count then
            Verbosity := Verbosity - 1;
            SMM.Download (Db, Category, Root_Dir, Max_Song_Count - Song_Count);
            Verbosity := Verbosity + 1;
         end if;
      end;

   when Playlist =>
      declare
         Replace   : Boolean         := False;
         Category  : constant String := Argument (Next_Arg);
         File_Name : access String;
      begin
         Next_Arg := Next_Arg + 1;
         if Next_Arg <= Argument_Count then
            if Argument (Next_Arg) = "--replace" then
               Replace := True;
            else
               File_Name := new String'(Ada.Directories.Full_Name (Argument (Next_Arg)));
               Next_Arg := Next_Arg + 1;
               if Next_Arg <= Argument_Count and then
                 Argument (Next_Arg) = "--replace"
               then
                  Replace := True;
               end if;
            end if;
         end if;

         if File_Name = null then
            --  Use Full_Name so directory separators match Containing_Directory
            File_Name := new String'
              (As_Directory (SAL.Config_Files.Read (Db, Playlist_Key)) & Category & ".m3u");
         end if;

         SMM.Playlist (Db, Category, File_Name.all, Replace, Max_Song_Count);
      end;

   when Import =>
      declare
         Category : constant String := Argument (Next_Arg);
         Root     : constant String := Argument (Next_Arg + 1);
      begin
         Verbosity := Integer'Max (Verbosity, 1);
         if Root (Root'Last) /= '/' then
            SMM.Import (Db, Category, Root & '/');
         else
            SMM.Import (Db, Category, Root);
         end if;
      end;

   end case;

   SAL.Config_Files.Close (Db);

exception
when E : others =>
   SAL.Config_Files.Close (Db);
   Put_Line
     ("exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E));
   Put_Usage;
end SMM.Driver;
