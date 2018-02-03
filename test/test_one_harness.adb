--  Abstract :
--
--  Run one test
--
--  Copyright (C) 2007 - 2009, 2013, 2015 - 2016, 2018 Stephen Leake.  All Rights Reserved.
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
with AUnit.Simple_Test_Cases;
with AUnit.Test_Filters;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Test_Server;
procedure Test_One_Harness
is
   --  command line arguments: [routine_name [debug_level]]
   --
   --  routine_name can be '' to set debug for all routines.
   --
   --  AUnit design forces this awkward order of declarations.

   Debug : constant Integer := (if Argument_Count > 1 then Integer'Value (Argument (2)) else 0);

   Tc : constant AUnit.Simple_Test_Cases.Test_Case_Access := new Test_Server.Test_Case
     (Server_IP => new String'("192.168.1.83"),
      Debug => Debug);

   function New_Name_Filter (Routine_Name : in String) return AUnit.Test_Filters.Test_Filter_Access
   is
      use AUnit.Test_Filters;
   begin
      return Filter : constant Test_Filter_Access := new Name_Filter do
         Set_Name (Name_Filter (Filter.all), Tc.Name.all & " : " & Routine_Name);
      end return;
   end New_Name_Filter;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           =>
        (if Argument_Count > 0 and then Argument (1) /= ""
         then New_Name_Filter (Argument (1))
         else null));

   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

begin
   Add_Test (Suite, Tc);

   Run (Suite, Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_One_Harness;
