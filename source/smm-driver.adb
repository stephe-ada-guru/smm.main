--  Abstract :
--
--  main procedure for SMM application
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Command_Line_IO;
with SAL.Config_Files;
with SMM.Import;
procedure SMM.Driver
is
   procedure Put_Usage
   is begin
      Put_Line ("smm [--db=<db_file>] [--verbosity=<int>] <operation> [arg]...");
      Put_Line ("  <db_file> : defaults to ~/.smm/smm.db");
      Put_Line ("  categories: {instrumental | vocal}");
      Put_Line ("  operations:");
      Put_Line ("  download <category> <target_dir>");
      Put_Line ("    downloads default amount of music to target_dir");
      Put_Line ("    music is drawn from least-recently downloaded songs in category");
      New_Line;
      Put_Line ("  import <root_dir>");
      Put_Line ("    scan <root_dir> for new music");
      New_Line;
      Put_Line ("  set_category <category> <file>");
      Put_Line ("    <file> can have '*' wildcards");
   end Put_Usage;

   Db_File_Name : access String;
   Db           : SAL.Config_Files.Configuration_Type;
   Next_Arg     : Integer := 1;

   type Command_Type is (Download, Import, Set_Category);

   procedure Get_Command is new SAL.Command_Line_IO.Gen_Get_Discrete (Command_Type, "command", Next_Arg);

   Command : Command_Type;
begin
   if Argument (Next_Arg)'Length > 5 and then
     Argument (Next_Arg)(1 .. 5) = "--db="
   then
      Db_File_Name := new String'(Argument (Next_Arg)(6 .. Argument (Next_Arg)'Last));
      Next_Arg := Next_Arg + 1;
   else
      Db_File_Name := new String'("~/.smm/smm.db");
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
      raise SAL.Not_Implemented;

   when Import =>
      SMM.Import (Db, Argument (Next_Arg));

   when Set_Category =>
      raise SAL.Not_Implemented;

   end case;

   SAL.Config_Files.Close (Db);

exception
when E : others =>
   SAL.Config_Files.Close (Db);
   Put_Line
     ("exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E));
end SMM.Driver;
