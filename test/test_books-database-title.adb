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

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration;
with Books.Database.Data_Tables.Title;
with Interfaces;
with SAL.Config_Files;
package body Test_Books.Database.Title is

   Config : aliased SAL.Config_Files.Configuration_Type;

   DB : Books.Database.Database_Access;

   Title_Table : Books.Database.Data_Tables.Title.Table_Access;

   procedure Check_Title
     (Title        : in String;
      Year         : in Interfaces.Unsigned_16;
      Year_Valid   : in Boolean;
      Comment      : in String;
      Rating       : in Interfaces.Unsigned_8;
      Rating_Valid : in Boolean)
   is
      use Books.Database;
      Tab : Data_Tables.Title.Table renames Title_Table.all;
      use type Interfaces.Unsigned_16;
      use type Interfaces.Unsigned_8;
   begin
      Assert (Data_Tables.Title.Title (Tab) = Title, "got Title => " & Data_Tables.Title.Title (Tab));

      if Year_Valid then
         Assert
           (Data_Tables.Title.Year (Tab) = Year,
            "got Year => " & Interfaces.Unsigned_16'Image (Data_Tables.Title.Year (Tab)));
      end if;

      Assert (Data_Tables.Title.Comment (Tab) = Comment, "got Comment => " & Data_Tables.Title.Comment (Tab));

      if Rating_Valid then
         Assert
           (Data_Tables.Title.Rating (Tab) = Rating,
            "got Rating => " & Interfaces.Unsigned_8'Image (Data_Tables.Title.Rating (Tab)));
      end if;

   end Check_Title;

   procedure Add_Title
     (Title        : in String;
      Year         : in Interfaces.Unsigned_16;
      Year_Valid   : in Boolean;
      Comment      : in String;
      Rating       : in Interfaces.Unsigned_8;
      Rating_Valid : in Boolean)
   is
      use Books.Database;
      Tab : Data_Tables.Title.Table renames Title_Table.all;
   begin
      Data_Tables.Title.Insert
        (Tab,
         Title        => Title,
         Year         => Year,
         Year_Valid   => Year_Valid,
         Comment      => Comment,
         Rating       => Rating,
         Rating_Valid => Rating_Valid);

      Check_Title (Title, Year, Year_Valid, Comment, Rating, Rating_Valid);

   end Add_Title;

   procedure Fetch_Title
     (ID           : in String;
      Title        : in String;
      Year         : in Interfaces.Unsigned_16;
      Year_Valid   : in Boolean;
      Comment      : in String;
      Rating       : in Interfaces.Unsigned_8;
      Rating_Valid : in Boolean)
   is
      use Books.Database;
      Tab : Data_Tables.Title.Table renames Title_Table.all;
   begin
      Data_Tables.Fetch (Tab, Books.Database.Value (ID));
      Check_Title (Title, Year, Year_Valid, Comment, Rating, Rating_Valid);
   end Fetch_Title;

   ----------
   --  Test procedures

   procedure Add_Titles (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Add_Title
        (Title        => "2001",
         Year         => 1970,
         Year_Valid   => True,
         Comment      => "Obelisk",
         Rating       => 9,
         Rating_Valid => True);

      Add_Title
        (Title        => "Foundation",
         Year         => 1970,
         Year_Valid   => True,
         Comment      => "Harry Seldon",
         Rating       => 9,
         Rating_Valid => True);

   end Add_Titles;

   procedure Fetch_Titles (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Fetch_Title
        (ID           => "1",
         Title        => "2001",
         Year         => 1970,
         Year_Valid   => True,
         Comment      => "Obelisk",
         Rating       => 9,
         Rating_Valid => True);

      Fetch_Title
        (ID           => "2",
         Title        => "Foundation",
         Year         => 1970,
         Year_Valid   => True,
         Comment      => "Harry Seldon",
         Rating       => 9,
         Rating_Valid => True);

   end Fetch_Titles;

   procedure Fetch_Fresh (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Books.Database.Data_Tables;
   begin
      --  Verify that Fetch works without a previous Insert.
      Books.Database.Free (Books.Database.Table_Access (Title_Table));
      Title_Table := new Books.Database.Data_Tables.Title.Table (DB);

      Fetch_Title
        (ID           => "1",
         Title        => "2001",
         Year         => 1970,
         Year_Valid   => True,
         Comment      => "Obelisk",
         Rating       => 9,
         Rating_Valid => True);

      Fetch_Title
        (ID           => "2",
         Title        => "Foundation",
         Year         => 1970,
         Year_Valid   => True,
         Comment      => "Harry Seldon",
         Rating       => 9,
         Rating_Valid => True);

   end Fetch_Fresh;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Books.Database.Title");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Add_Titles'Access, "Add_Titles");
      Register_Routine (T, Fetch_Titles'Access, "Fetch_Titles");
      Register_Routine (T, Fetch_Fresh'Access, "Fetch_Fresh");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
   begin
      --  We assume no titles are in database.
      SAL.Config_Files.Open (Config, T.Config_File.all);
      DB           := new Books.Database.Database (Config'Access);
      Title_Table := new Books.Database.Data_Tables.Title.Table (DB);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Books.Database.Free (Books.Database.Table_Access (Title_Table));
      Books.Database.Free (DB);
      SAL.Config_Files.Close (Config);
   end Tear_Down_Case;

end Test_Books.Database.Title;
