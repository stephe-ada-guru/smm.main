--  Abstract :
--
--  main procedure for SMM application
--
--  Copyright (C) 2008 - 2013, 2015 - 2020, 2022, 2025 Stephen Leake.  All Rights Reserved.
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
with SMM.Compare_Playlist.HTML;
with SMM.Compare_Playlist.Spotify;
with SMM.Compare_Phone;
with SMM.Database;
with SMM.History;
with SMM.ID3;
with SMM.Import;
with SMM.Update;
with SMM.Update_Playlist;
procedure SMM.Driver
is
   procedure Put_Usage
   is begin
      Put_Line ("smm [options] <operation> [arg]...");
      Put_Line ("  options:");
      Put_Line ("  --db=<db_file> : defaults to $SMM_HOME/smm.db or $HOME/smm/smm.db or $APPDATA/smm/smm.db");
      Put_Line ("  --verbosity=<int>");
      Put_Line ("  --max_errors=<int> : in Compare_Playlist, stop after <int> errors.");
      Put_Line ("  --ignore_id3_flags : ignore ID3 file, frame flag settings that we nominally don't support.");
      New_Line;
      Put_Line ("  categories: {instrumental | vocal | ...}");
      New_Line;
      Put_Line ("  operations:");
      Put_Line ("  update_playlist <category> <count> <playlist_file> [--replace]");
      Put_Line ("    add to a playlist in <file> <count> least recently played songs of <category> ");
      Put_Line ("    --replace - overwrite file; otherwise append");
      Put_Line ("    if <file> is in music root, paths in playlist are relative");
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
      Put_Line ("  delete <file name>");
      Put_Line ("    delete the db entry for <file name>.");
      New_Line;
      Put_Line ("  check [--ignore_metadata]");
      Put_Line ("    compare music files to db, report any missing files/fields.");
      New_Line;
      Put_Line ("  history");
      Put_Line ("    output histogram (in gnuplot files) of download interval (last to previous).");
      Put_Line ("    list all new songs.");
      New_Line;
      Put_Line ("  compare_playlist <category> <spotify <missing_file> | html html_file>");
      Put_Line ("    compare list of music files marked category in db to corresponding Spotify playlist.");
      Put_Line ("    category must be one of 'best', 'protest'.");
      New_Line;
      Put_Line ("  compare_phone <phone_ls_file>");
      Put_Line ("    compare dates of local music files against music files on phone; report those changed.");
   end Put_Usage;

   procedure Check_Arg (Expected_Count : in Integer)
   is begin
      if Argument_Count < Expected_Count then
         raise SAL.Parameter_Error with "missing argument";
      end if;
   end Check_Arg;

   Source_Root  : constant String := As_Directory (Ada.Directories.Current_Directory);
   DB_File_Name : Ada.Strings.Unbounded.String_Access := new String'(Find_DB_Filename);
   DB           : SMM.Database.Database;
   Next_Arg     : Integer         := 1;

   type Command_Type is
     (Update_Playlist, Compare_Phone, Import, Update, Rename, Delete, Check, History, Compare_Playlist);

   procedure Get_Command is new SAL.Command_Line_IO.Gen_Get_Discrete_Proc (Command_Type, "command", Next_Arg);

   Command : Command_Type;

begin
   loop
      exit when Next_Arg > Argument_Count or else
        Argument (Next_Arg)'Length < 2 or else
        Argument (Next_Arg) (1 .. 2) /= "--";

      if Argument (Next_Arg)'Length > 5 and then
        Argument (Next_Arg)(1 .. 5) = "--db="
      then
         DB_File_Name := new String'(Argument (Next_Arg)(6 .. Argument (Next_Arg)'Last));
         Next_Arg     := Next_Arg + 1;

      elsif Argument (Next_Arg) = "--help" then
         Put_Usage;
         return;

      elsif Argument (Next_Arg) = "--ignore_id3_flags" then
         SMM.ID3.Ignore_Flags := True;
         Next_Arg := Next_Arg + 1;

      elsif Argument (Next_Arg)'Length > 13 and then
        Argument (Next_Arg)(1 .. 13) = "--max_errors="
      then
         Max_Errors := Integer'Value (Argument (Next_Arg)(14 .. Argument (Next_Arg)'Last));
         Next_Arg := Next_Arg + 1;

      elsif Argument (Next_Arg)'Length > 12 and then
        Argument (Next_Arg)(1 .. 12) = "--verbosity="
      then
         Verbosity := Integer'Value (Argument (Next_Arg)(13 .. Argument (Next_Arg)'Last));
         Next_Arg := Next_Arg + 1;
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
   when Update_Playlist =>
      Check_Arg (Next_Arg + 2);
      declare
         Playlist_File     : constant String                    := Argument (Next_Arg);
         Category          : constant String                    := Argument (Next_Arg + 1);
         Count             : constant Ada.Containers.Count_Type :=
           Ada.Containers.Count_Type'Value (Argument (Next_Arg + 2));
         Replace           : constant Boolean                   := Next_Arg + 3 <= Argument_Count;
         New_Song_Count    : constant Ada.Containers.Count_Type := 5;
         Over_Select_Ratio : constant Float                     := 1.1;
      begin
         SMM.Update_Playlist (DB, Playlist_File, Category, Count, New_Song_Count, Over_Select_Ratio, Replace);
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

   when Delete =>
      Check_Arg (Next_Arg);
      declare
         Name : constant String := Relative_Name (Source_Root, Argument (Next_Arg));

         use SMM.Database;
         I : constant Cursor := Find_File_Name (DB, Name);
      begin
         if I.Has_Element then
            DB.Delete (I);
         else
            raise Ada.IO_Exceptions.Name_Error with "file name '" & Name & "' not found in db";
         end if;
      end;

   when Check =>
      declare
         Ignore_Metadata : constant Boolean :=
           (if Argument_Count >= Next_Arg then True else False);
      begin
         SMM.Check (DB, Source_Root, Ignore_Metadata);
      end;

   when History =>
      SMM.History (DB);

   when Compare_Playlist =>
      Check_Arg (Next_Arg + 2);
      declare
         Category   : constant String := Argument (Next_Arg);
         Other_Loc  : constant String := Argument (Next_Arg + 1);
         Other_File : constant String := Argument (Next_Arg + 2);
      begin
         if Other_Loc = "spotify" then
               SMM.Compare_Playlist.Spotify (DB, Category, Spotify_Missing => Other_File);
         else
            SMM.Compare_Playlist.HTML (DB, Category, HTML_Filename => Other_File);
         end if;
      end;

   when Compare_Phone =>
      Check_Arg (Next_Arg);
      SMM.Compare_Phone (Source_Root, Phone_Filename => Argument (Next_Arg));
   end case;

exception
when E : SAL.Parameter_Error =>
   Put_Line (Ada.Exceptions.Exception_Message (E));
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
   Set_Exit_Status (Failure);
end SMM.Driver;
