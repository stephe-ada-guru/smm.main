--  Abstract :
--
--  main procedure for add 'play_after' entries to SMM db
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions.Traceback;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Config_Files; use SAL.Config_Files;
with SMM;
procedure Add_Play_After
is
   procedure Put_Usage
   is begin
      Put_Line
        ("add_play_before [--db=<db_file>]");
      Put_Line ("  <db_file> : defaults to ~/smm/smm.db or $APPDATA/smm or $SMM_HOME");
   end Put_Usage;

   Db_File_Name : access String;
   Db           : Configuration_Type;

   Home : constant String := SMM.Find_Home;

begin
   if Argument_Count > 0 and then
     Argument (1)'Length > 5 and then
     Argument (1)(1 .. 5) = "--db="
   then
      Db_File_Name := new String'(Argument (1)(6 .. Argument (1)'Last));
   else
      Db_File_Name := new String'(Home & "/smm.db");
   end if;

   Open
     (Db,
      Db_File_Name.all,
      Duplicate_Key         => Raise_Exception,
      Read_Only             => False,
      Case_Insensitive_Keys => True);

   declare
      I : Iterator_Type := First (Db, SMM.Songs_Key);
   begin
      loop
         exit when I = Null_Iterator;

         if Is_Present (Db, I, "Play_Before") then
            Put (".");
            declare
               Song_Id   : constant String := Current (I);
               Before_Id : constant String := Read (Db, I, "Play_Before");
               Find_Key : constant String := ' ' & Before_Id;
            begin
               if Before_Id = Song_Id then
                  Put_Line ("db ERROR: " & Song_Id & ".Play_Before = " & Before_Id);
               else
                  declare
                     J : constant Iterator_Type := Find (Db, Root_Key => SMM.Songs_Key, Key => Find_Key);
                  begin
                     Write (Db, J, "Play_After", Song_Id);
                  end;
               end if;
            exception
            when SAL.Not_Found =>
               Put_Line ("db ERROR: " & SMM.Songs_Key & ". " & Before_Id & "not found");
            end;
         end if;

         Next (I);
      end loop;
   end;

   Close (Db);

exception
when E : others =>
   Close (Db);
   Put_Line
     ("exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (E)));
   Put_Usage;
end Add_Play_After;
