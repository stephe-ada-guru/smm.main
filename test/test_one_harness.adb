--  Abstract :
--
--  Run one test
--
--  Copyright (C) 2007 - 2009, 2013, 2015 - 2016, 2018, 2025, 2026 Stephen Leake.  All Rights Reserved.
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

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SMM;
with SMM.Database_Remote.IP.Test;
procedure Test_One_Harness
is
   --  command line arguments:
   Usage : constant String := "[<aunit verbose> [test_name [routine_name [smm/test verbosity]]]";
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  test_name, routine_name can be '' to set trace for all routines.

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

begin
   declare
      use Ada.Command_Line;
   begin
      Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

      case Argument_Count is
      when 0 | 1 =>
         null;

      when 2 =>
         Filter.Test_Name := Ada.Strings.Unbounded.To_Unbounded_String (Argument (2));

      when others =>
         Filter.Test_Name    := Ada.Strings.Unbounded.To_Unbounded_String (Argument (2));
         Filter.Routine_Name := Ada.Strings.Unbounded.To_Unbounded_String (Argument (3));
      end case;

      SMM.Verbosity := (if Argument_Count >= 4 then Integer'Value (Argument (4)) else 0);
   end;

   Add_Test (Suite, Test_Case_Access'(new SMM.Database_Remote.IP.Test.Test_Case
                                        (Server_IP => new String'("127.0.0.1"),
                                         Port      => 16#9002#,
                                         Debug     => SMM.Verbosity)));

   --  When run from Alire.make, current directory is smm.work_1, so the
   --  'make' commands to create empty dbs should work.
   --
   --  Ada.Text_IO.Put_Line ("current directory: " & Ada.Directories.Current_Directory);
   --  Ada.Directories.Set_Directory ("../..");
   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Usage);
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line
     (Ada.Text_IO.Standard_Error,
      GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_One_Harness;
