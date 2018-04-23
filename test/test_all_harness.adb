--  Abstract :
--
--  Run all AUnit tests.
--
--  Copyright (C) 2009, 2011 - 2013, 2015, 2016, 2018 Stephen Leake.  All Rights Reserved.
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
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SMM.Database.Test;
with SMM.ID3.Test;
with SAL;
with Test_Copy;
with Test_Download;
with Test_First_Pass_No_Last;
with Test_First_Pass_With_Last;
with Test_Import;
with Test_Least_Recent;
with Test_Play_Before;
with Test_Server;
procedure Test_All_Harness
is
   --  command line arguments:
   Usage : constant String := "[<verbose> [test_name [routine_name ]]";
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  test_name, routine_name can be '' to set trace for all routines.
   --
   --  Set_Up_Case is run without checking the filter.

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
         Filter.Set_Name (Argument (2));

      when 3 =>
         declare
            Test_Name    : String renames Argument (2);
            Routine_Name : String renames Argument (3);
         begin
            if Test_Name = "" then
               Filter.Set_Name (Routine_Name);
            elsif Routine_Name = "" then
               Filter.Set_Name (Test_Name);
            else
               Filter.Set_Name (Test_Name & " : " & Routine_Name);
            end if;
         end;
      when others =>
         raise SAL.Programmer_Error with Usage;
      end case;
   end;

   Add_Test (Suite, new SMM.Database.Test.Test_Case);
   Add_Test (Suite, new SMM.ID3.Test.Test_Case);
   Add_Test (Suite, new Test_Copy.Test_Case (Verbosity => 0));
   Add_Test (Suite, new Test_Download.Test_Case (Verbosity => 0));
   Add_Test (Suite, new Test_First_Pass_No_Last.Test_Case (Verbosity => 0, Debug => False));
   Add_Test (Suite, new Test_First_Pass_With_Last.Test_Case (Verbosity => 0, Debug => False));
   Add_Test (Suite, new Test_Import.Test_Case);
   Add_Test (Suite, new Test_Least_Recent.Test_Case);
   Add_Test (Suite, new Test_Play_Before.Test_Case);
   Add_Test (Suite, new Test_Server.Test_Case (Server_Ip => new String'("192.168.1.83"), Debug => 0));

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

   case Status is
   when AUnit.Success =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   when AUnit.Failure =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end case;

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_All_Harness;
