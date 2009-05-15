--  Abstract :
--
--  Run all tests of books.
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
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
with Ada.Command_Line;
with Ada.Text_IO;
with Test_Books.Database.Author;
with Test_Books.Database.AuthorTitle;
with Test_Books.Database.Title;
with Test_Books.Errors;
with Test_Books.Nominal;
procedure Test_Books.All_Harness
is
   procedure Put_Usage
   is begin
      Ada.Text_IO.Put_Line ("usage: <root_directory> <debug_level>");
   end Put_Usage;

   Suite  : constant Access_Test_Suite := new Test_Suite;
   Result : AUnit.Test_Results.Result;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Usage;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   Add_Test
     (Suite,
      new Test_Books.Database.Author.Test_Case
        (Root_Directory => new String'(Ada.Command_Line.Argument (1)),
         Debug_Level    => Integer'Value (Ada.Command_Line.Argument (2))));

   Add_Test
     (Suite,
      new Test_Books.Database.Title.Test_Case
        (Root_Directory => new String'(Ada.Command_Line.Argument (1)),
         Debug_Level    => Integer'Value (Ada.Command_Line.Argument (2))));

   --  AuthorTitle assumes Author and Title have added stuff to the database.
   Add_Test
     (Suite,
      new Test_Books.Database.AuthorTitle.Test_Case
        (Root_Directory => new String'(Ada.Command_Line.Argument (1)),
         Debug_Level    => Integer'Value (Ada.Command_Line.Argument (2))));

   Add_Test
     (Suite,
      new Test_Books.Errors.Test_Case
        (Root_Directory => new String'(Ada.Command_Line.Argument (1)),
         Debug_Level    => Integer'Value (Ada.Command_Line.Argument (2))));

   Add_Test
     (Suite,
      new Test_Books.Nominal.Test_Case
        (Root_Directory => new String'(Ada.Command_Line.Argument (1)),
         Debug_Level    => Integer'Value (Ada.Command_Line.Argument (2))));

   Run (Suite.all, Result);
   AUnit.Test_Results.Text_Reporter.Report (Result);
end Test_Books.All_Harness;
