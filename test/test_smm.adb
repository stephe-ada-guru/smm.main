--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2025  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with AUnit.Assertions;
with GNAT.OS_Lib;
package body Test_SMM is

   procedure Empty_Database_1
   is
      Make_Args : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List ("--silent -f Alire.make empty_database_test_1");
      Success : Boolean;
   begin
      GNAT.OS_Lib.Spawn
        ("/usr/bin/make",
         Make_Args.all,
         Success => Success);

      GNAT.OS_Lib.Free (Make_Args);

      AUnit.Assertions.Assert (Success, "make empty_database_1 failed");

   end Empty_Database_1;

   procedure Empty_Database_2
   is
      Make_Args : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List ("--silent -f Alire.make empty_database_test_2");
      Success : Boolean;
   begin
      GNAT.OS_Lib.Spawn
        ("/usr/bin/make",
         Make_Args.all,
         Success => Success);

      GNAT.OS_Lib.Free (Make_Args);

      AUnit.Assertions.Assert (Success, "make empty_database_2 failed");

   end Empty_Database_2;

end Test_SMM;
