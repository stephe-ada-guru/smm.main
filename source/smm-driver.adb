--  Abstract :
--
--  main procedure for SMM application
--
--  Copyright (C) 2008 - 2013, 2015 - 2020, 2022 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions.Traceback;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Command_Line_IO;
with SMM.Check;
with SMM.Copy;
with SMM.Database;
with SMM.History;
with SMM.ID3;
with SMM.Import;
with SMM.Update;
procedure SMM.Driver
is
   procedure Put_Usage
   is begin
      Put_Line ("smm [options] <operation> [arg]...");
      Put_Line ("  options:");
      Put_Line ("  --db=<db_file> : defaults to ~/smm/smm.db or $APPDATA/smm or $SMM_HOME");
      Put_Line ("  --verbosity=<int>");
      Put_Line ("  --ignore_id3_flags : ignore ID3 file, frame flag settings that we nominally don't support.");
      New_Line;
      Put_Line ("  categories: {instrumental | vocal | ...}");
      New_Line;
      Put_Line ("  operations:");
      Put_Line ("  playlist <category> [<file>] [--replace]");
      Put_Line ("    create a playlist in <file> (same songs as 'download' would do)");
      Put_Line ("    <file> default specified in smm.db by Playlists key");
      Put_Line ("    --replace - overwrite file; otherwise append");
      Put_Line ("    if <file> is in database root, paths in playlist are relative");
      New_Line;
      Put_Line ("  copy_playlist <playlist> <playlist_dir>");
      Put_Line ("    copy playlist and referenced files to playlist_dir");
      Put_Line ("    current directory must be database root dir");
      New_Line;
      Put_Line ("  import <category> <dir>");
      Put_Line ("    scan <dir> for new music; dir must be relative to database root dir");
      New_Line;
      Put_Line ("  update <file | dir>");
      Put_Line ("    update metadata for <file>, or all .mp3/.m4a files in <dir>.");
      New_Line;
      Put_Line ("  rename <old file name> <new file name>");
      Put_Line ("    change file name.");
      New_Line;
      Put_Line ("  check");
      Put_Line ("    compare music files to db, report any missing files/fields.");
      New_Line;
      Put_Line ("  history");
      Put_Line ("    output histogram (in gnuplot files) of download interval (last to previous).");
      Put_Line ("    list all new songs.");
   end Put_Usage;

   procedure Check_Arg (Expected_Count : in Integer)
   is begin
      if Argument_Count < Expected_Count then
         raise SAL.Parameter_Error;
      end if;
   end Check_Arg;

   Home : constant String := Find_Home;

   Source_Root  : constant String := As_Directory (Ada.Directories.Current_Directory);
   DB_File_Name : Ada.Strings.Unbounded.String_Access := new String'(Home & "/smm.db");
   DB           : SMM.Database.Database;
   Next_Arg     : Integer         := 1;

   type Command_Type is (Copy_Playlist, Import, Update, Rename, Check, History);

   procedure Get_Command is new SAL.Command_Line_IO.Gen_Get_Discrete_Proc (Command_Type, "command", Next_Arg);

   Command : Command_Type;

begin
   loop
      exit when Argument (Next_Arg)'Length < 2 or else Argument (Next_Arg) (1 .. 2) /= "--";

      if Argument (Next_Arg)'Length > 5 and then
        Argument (Next_Arg)(1 .. 5) = "--db="
      then
         DB_File_Name := new String'(Argument (Next_Arg)(6 .. Argument (Next_Arg)'Last));
         Next_Arg     := Next_Arg + 1;
      end if;

      if Argument (Next_Arg)'Length > 12 and then
        Argument (Next_Arg)(1 .. 12) = "--verbosity="
      then
         Verbosity := Integer'Value (Argument (Next_Arg)(13 .. Argument (Next_Arg)'Last));
         Next_Arg := Next_Arg + 1;
      else
         Verbosity := 0;
      end if;

      if Argument (Next_Arg) = "--ignore_id3_flags" then
         SMM.ID3.Ignore_Flags := True;
         Next_Arg := Next_Arg + 1;

      elsif Argument (Next_Arg) = "--help" then
         Put_Usage;
         return;
      end if;
   end loop;

   DB.Open (DB_File_Name.all);

   begin
      Get_Command (Command);
   exception
   when SAL.Parameter_Error =>
      Put_Usage;
      return;
   end;

   case Command is
   when Copy_Playlist =>
      Check_Arg (Next_Arg + 1);
      declare
         Playlist_Name : constant String := Argument (Next_Arg);
         Playlist_Dir    : constant String := As_Directory (Argument (Next_Arg + 1));
      begin
         SMM.Copy (Playlist_Name, Playlist_Dir);
      end;

   when Import =>
      Check_Arg (Next_Arg + 1);
      declare
         Category    : constant String := Argument (Next_Arg);
         Import_Root : constant String := As_Directory (Argument (Next_Arg + 1));
      begin
         Verbosity := Integer'Max (Verbosity, 1);
         SMM.Import (DB, Source_Root, Category, Import_Root);
      end;

   when Update =>
      Check_Arg (Next_Arg);
      SMM.Update (DB, Source_Root, Relative_Name (Source_Root, Argument (Next_Arg)));

   when Rename =>
      Check_Arg (Next_Arg + 1);
      declare
         Old_Name : constant String := Relative_Name (Source_Root, Argument (Next_Arg));
         New_Name : constant String := Relative_Name (Source_Root, Argument (Next_Arg + 1));

         use SMM.Database;
         I : constant Cursor := Find_File_Name (DB, Old_Name);
      begin
         if I.Has_Element then
            DB.Update (I, File_Name => New_Name);
         else
            raise Ada.IO_Exceptions.Name_Error with "old file name '" & Old_Name & "' not found in db";
         end if;
      end;

   when Check =>
      SMM.Check (DB, Source_Root);

   when History =>
      SMM.History (DB);
   end case;

exception
when SAL.Parameter_Error =>
   Put_Usage;
   Set_Exit_Status (Failure);

when E : Ada.IO_Exceptions.Name_Error =>
   Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Message (E));
   Set_Exit_Status (Failure);

when E : others =>
   Put_Line
     (Standard_Error,
      "exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E));
   Put_Line
     (Standard_Error,
      GNAT.Traceback.Symbolic.Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (E)));
   Put_Usage;
   Set_Exit_Status (Failure);
end SMM.Driver;
