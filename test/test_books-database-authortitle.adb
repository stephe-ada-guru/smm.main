--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
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
--

with AUnit.Test_Cases.Registration;
with Books.Database.Link_Tables.AuthorTitle;
with SAL.Config_Files;
package body Test_Books.Database.AuthorTitle is

   Config : aliased SAL.Config_Files.Configuration_Type;

   DB : Books.Database.Database_Access;

   AuthorTitle_Table : Books.Database.Link_Tables.AuthorTitle.Table_Access;

   ----------
   --  Test procedures

   procedure Add_AuthorTitles (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Books.Database.Link_Tables;
      use Books.Database.Link_Tables.AuthorTitle;
   begin
      Insert
        (AuthorTitle_Table.all,
         (Author => 1, Title => 1));

      Insert
        (AuthorTitle_Table.all,
         (Author => 2, Title => 2));

      Fetch_Links_Of (AuthorTitle_Table.all, Author, 1);

      Check ("author", ID (AuthorTitle_Table.all, Author), Expected => 1);
      Check ("title", ID (AuthorTitle_Table.all, Title), Expected => 1);

      Fetch_Links_Of (AuthorTitle_Table.all, Author, 2);

      Check ("author", ID (AuthorTitle_Table.all, Author), Expected => 2);
      Check ("title", ID (AuthorTitle_Table.all, Title), Expected => 2);

   end Add_AuthorTitles;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Books.Database.AuthorTitle");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Add_AuthorTitles'Access, "Add_AuthorTitles");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
   begin
      --  We assume the required authors and titles are in the
      --  database (from test_books-database-author,
      --  test_books-database-title), but not the authortitle entries.
      SAL.Config_Files.Open (Config, T.Config_File.all);
      DB                := new Books.Database.Database (Config'Access);
      AuthorTitle_Table := new Books.Database.Link_Tables.AuthorTitle.Table (DB);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Books.Database.Free (Books.Database.Table_Access (AuthorTitle_Table));
      Books.Database.Free (DB);
      SAL.Config_Files.Close (Config);
   end Tear_Down_Case;

end Test_Books.Database.AuthorTitle;
