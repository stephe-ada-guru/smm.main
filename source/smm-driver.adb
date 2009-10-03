--  Abstract :
--
--  main procedure for SMM application
--
--  Copyright (C) 2008, 2009 Stephen Leake.  All Rights Reserved.
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
with SMM.Second_Pass;
procedure SMM.Driver
is
   procedure Put_Usage
   is begin
      Put_Line ("smm [--db=<db_file>] [--verbosity=<int>] <operation> [arg]...");
      Put_Line ("  <db_file> : defaults to ~/.smm/smm.db");
      Put_Line ("  categories: {instrumental | vocal | ...}");
      Put_Line ("  operations:");
      Put_Line ("  download <category> <target_dir>");
      Put_Line ("    downloads default amount of music to target_dir");
      Put_Line ("    music is drawn from least-recently downloaded songs in category");
      New_Line;
      Put_Line ("  download_playlist <category> <target_dir>");
      Put_Line ("    manage downloaded files and playlist:");
      Put_Line ("    1) delete files in target_dir not in playlist target_dir/../category.m3u");
      Put_Line ("    2) download default amount of music to target_dir");
      Put_Line ("    3) add new files to playlist");
      New_Line;
      Put_Line ("  playlist <category> [<file>] [--replace]");
      Put_Line ("    create a playlist in <file> (same songs as 'download' would do)");
      Put_Line ("    <file> defaults to ~/.smm/<file>.m3u");
      Put_Line ("    --replace - overwrite file; otherwise append");
      Put_Line ("    if <file> is in database root, paths in playlist are relative");
      New_Line;
      Put_Line ("  import <dir>");
      Put_Line ("    scan <dir> for new music; dir must be relative to database root dir");
   end Put_Usage;

   Db_File_Name : access String;
   Db           : SAL.Config_Files.Configuration_Type;
   Next_Arg     : Integer := 1;

   Home : constant String := Ada.Environment_Variables.Value ("HOME");

   type Command_Type is (Download, Download_Playlist, Playlist, Import);

   procedure Get_Command is new SAL.Command_Line_IO.Gen_Get_Discrete (Command_Type, "command", Next_Arg);

   Command : Command_Type;
begin
   if Argument (Next_Arg)'Length > 5 and then
     Argument (Next_Arg)(1 .. 5) = "--db="
   then
      Db_File_Name := new String'(Argument (Next_Arg)(6 .. Argument (Next_Arg)'Last));
      Next_Arg := Next_Arg + 1;
   else
      Db_File_Name := new String'(Home & "/.smm/smm.db");
   end if;

   if Argument (Next_Arg)'Length > 12 and then
     Argument (Next_Arg)(1 .. 12) = "--verbosity="
   then
      Verbosity := Integer'Value (Argument (Next_Arg)(13 .. Argument (Next_Arg)'Last));
      Next_Arg := Next_Arg + 1;
   else
      Verbosity := 0;
   end if;

   SAL.Config_Files.Open
     (Db,
      Db_File_Name.all,
      Duplicate_Key         => SAL.Config_Files.Raise_Exception,
      Read_Only             => False,
      Case_Insensitive_Keys => False);

   begin
      Get_Command (Command);
   exception
   when SAL.Parameter_Error =>
      Put_Usage;
      return;
   end;

   case Command is
   when Download =>
      SMM.Download (Db, Argument (Next_Arg), As_Directory (Argument (Next_Arg + 1)));

   when Download_Playlist =>
      declare
         Category   : constant String := Argument (Next_Arg);
         Root_Dir   : constant String := As_Directory (Argument (Next_Arg + 1));
         File_Count : Integer;
      begin
         Verbosity := Verbosity + 1;
         SMM.First_Pass (Category, Root_Dir, File_Count);
         if File_Count < Download_File_Count then
            Verbosity := Verbosity - 1;
            SMM.Download (Db, Category, Root_Dir & Category & '/');
            Verbosity := Verbosity + 1;
            SMM.Second_Pass (Category, Root_Dir);
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
            File_Name := new String'(Home & "/.smm/" & Category & ".m3u");
         end if;

         SMM.Playlist (Db, Category, File_Name.all, Replace);
      end;

   when Import =>
      declare
         Root : constant String := Argument (Next_Arg);
      begin
         if Root (Root'Last) /= '/' then
            SMM.Import (Db, Root & '/');
         else
            SMM.Import (Db, Root);
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
