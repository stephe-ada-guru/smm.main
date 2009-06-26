--  Abstract :
--
--  Run one test of books.
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Results.Text_Reporter;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line;
with Test_Books.Nominal;
procedure Test_Books.One_Harness
is
   Suite       : constant Access_Test_Suite := new Test_Suite;
   Result      : AUnit.Test_Results.Result;
   Debug_Level : Integer                    := 0;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Debug_Level := Integer'Value (Ada.Command_Line.Argument (1));
   end if;

   Add_Test
     (Suite,
      new Test_Books.Nominal.Test_Case
        (Config_File => new String'("../../test/test.config"),
         Debug_Level => Debug_Level));

   Run (Suite.all, Result);
   AUnit.Test_Results.Text_Reporter.Report (Result);
end Test_Books.One_Harness;
