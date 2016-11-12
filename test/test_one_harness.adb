--  Abstract :
--
--  Run one test
--
--  Copyright (C) 2007 - 2009, 2013, 2015 - 2016 Stephen Leake.  All Rights Reserved.
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
with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Test_Server;
procedure Test_One_Harness
is
   Suite  : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

   Debug : Integer := 0;
begin
   if Argument_Count > 0 then
      Debug := Integer'Value (Argument (1));
   end if;

   Add_Test (Suite, new Test_Server.Test_Case (Server_IP => new String'("192.168.1.83"), Debug => Debug));

   Run (Suite, AUnit.Options.Default_Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_One_Harness;
