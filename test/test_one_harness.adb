--  Abstract :
--
--  Run one test
--
--  Copyright (C) 2007 - 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line;
with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites;
with Test_First_Pass;
procedure Test_One_Harness
is
   use AUnit.Test_Suites;

   Suite  : constant Access_Test_Suite := new Test_Suite;

   Result : AUnit.Test_Results.Result;

   Setup_Only : Boolean := False;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      if Ada.Command_Line.Argument (1) = "setup_only" then
         Setup_Only := True;
      end if;
   end if;

   Add_Test (Suite, new Test_First_Pass.Test_Case (Verbosity => 2, Setup_Only => Setup_Only, Debug => 0));

   Run (Suite.all, Result);

   AUnit.Test_Results.Text_Reporter.Report (Result);

end Test_One_Harness;
