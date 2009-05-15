--  Abstract :
--
--  See spec
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
--

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Books.Database.Data_Tables.Author;
with SAL.Config_Files;
package body Test_Books.Database.Author is

   Config : aliased SAL.Config_Files.Configuration_Type;

   DB : Books.Database.Database_Access;

   Author_Table : Books.Database.Data_Tables.Author.Table_Access;

   procedure Add_Author
     (First  : in String;
      Middle : in String;
      Last   : in String)
   is
      use Books.Database.Data_Tables.Author;
      Tab : Books.Database.Data_Tables.Author.Table renames Author_Table.all;
   begin
      Insert
        (Tab,
         First_Name  => First,
         Middle_Name => Middle,
         Last_Name   => Last);

      Assert (First_Name (Tab) = First, "got First_Name => " & First_Name (Tab));
      Assert (Middle_Name (Tab) = Middle, "got Middle_Name => " & Middle_Name (Tab));
      Assert (Last_Name (Tab) = Last, "got Last_Name => " & Last_Name (Tab));
   end Add_Author;

   procedure Fetch_Author
     (ID     : in String;
      First  : in String;
      Middle : in String;
      Last   : in String)
   is
      use Books.Database.Data_Tables.Author;
      Tab : Books.Database.Data_Tables.Author.Table renames Author_Table.all;
   begin
      Books.Database.Data_Tables.Fetch (Tab, Books.Database.Value (ID));

      Assert (First_Name (Tab) = First, "got First_Name => " & First_Name (Tab));
      Assert (Middle_Name (Tab) = Middle, "got Middle_Name => " & Middle_Name (Tab));
      Assert (Last_Name (Tab) = Last, "got Last_Name => " & Last_Name (Tab));
   end Fetch_Author;

   ----------
   --  Test procedures

   procedure Add_Authors (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Add_Author ("Arthur", "C.", "Clarke");
      Add_Author ("Isacc", "", "Asimov");
   end Add_Authors;

   procedure Fetch_Authors (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Fetch_Author ("1", "Arthur", "C.", "Clarke");
      Fetch_Author ("2", "Isacc", "", "Asimov");
   end Fetch_Authors;

   procedure Fetch_Fresh (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Books.Database.Data_Tables;
   begin
      --  Verify that Fetch works without a previous Insert.
      Books.Database.Free (Books.Database.Table_Access (Author_Table));
      Author_Table := new Books.Database.Data_Tables.Author.Table (DB);

      Fetch_Author ("1", "Arthur", "C.", "Clarke");
      Fetch_Author ("2", "Isacc", "", "Asimov");
   end Fetch_Fresh;

   ----------
   --  Public bodies

   function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Books.Database.Author");
   end Name;

   procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Add_Authors'Access, "Add_Authors");
      Register_Routine (T, Fetch_Authors'Access, "Fetch_Authors");
      Register_Routine (T, Fetch_Fresh'Access, "Fetch_Fresh");
   end Register_Tests;

   procedure Set_Up_Case (T : in out Test_Case)
   is
   begin
      --  We assume no authors are in database.
      SAL.Config_Files.Open (Config, T.Root_Directory.all & "/test.config");
      DB           := new Books.Database.Database (Config'Access);
      Author_Table := new Books.Database.Data_Tables.Author.Table (DB);
   end Set_Up_Case;

   procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Books.Database.Free (Books.Database.Table_Access (Author_Table));
      Books.Database.Free (DB);
      SAL.Config_Files.Close (Config);
   end Tear_Down_Case;

end Test_Books.Database.Author;
