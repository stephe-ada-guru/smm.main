--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2016, 2018, 2019, 2025, 2026 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks;
with GNATCOLL.JSON.AUnit;
with SMM.Database_Remote.Disk;
with Test_SMM;
package body SMM.Database.Diff.Test_Apply is

   Local_Disk_DB  : aliased SMM.Database.Database;
   Local_DB       : aliased SMM.Database_Remote.Disk.Database (Local_Disk_DB'Access);
   Remote_Disk_DB : aliased SMM.Database.Database;
   Remote_DB      : aliased SMM.Database_Remote.Disk.Database (Remote_Disk_DB'Access);

   procedure Check
     (Label    : in String;
      DB       : in SMM.Database_Remote.Database'Class;
      ID       : in Song_ID;
      Expected : in GNATCOLL.JSON.JSON_Value)
   is begin
      GNATCOLL.JSON.AUnit.Check (Label, DB.Get_JSON (ID), Expected);
   end Check;

   --  test procedures

   procedure Insert (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use GNATCOLL.JSON;
      use AUnit.Checks;

      Local_Changes  : JSON_Array;
      Remote_Changes : JSON_Array;

      Song_1 : constant JSON_Value := Read
        ("{""ID"":1, " &
           """Modified"":""2000-01-02 00:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Arthur/C./Clarke.mp3"", " &
           """Category"":""vocal"", " &
           """Album_Artist"":""Arthur"", " &
           """Album"":""C."", " &
           """Title"":""Clarke""}}");

      Song_2 : constant JSON_Value := Read
        ("{""ID"":2, " &
           """Modified"":""2000-01-02 00:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Isaac/Asimov.mp3"", " &
           """Category"":""vocal"", " &
           """Album_Artist"":""Isaac"", " &
           --  no album
           """Title"":""Asimov""}}");

      Diff : Diff_Type :=
        (Local_DB      => Local_DB'Access,
         Remote_DB     => Remote_DB'Access,
         Sync_ID       => Invalid_Song_ID,
         Show_Progress => null,
         Verbosity     => Test_Case (T).Verbosity);

   begin
      Append (Local_Changes, To_Insert (Song_1));
      Append (Local_Changes, To_Insert (Song_2));

      Append (Remote_Changes, To_Insert (Song_1));
      Append (Remote_Changes, To_Insert (Song_2));

      Apply (Diff, Local_Changes, Remote_Changes);

      Check ("local song 1", Local_DB, 1, Song_1);
      Check ("local song 2", Local_DB, 2, Song_2);

      Check ("remote author 1", Remote_DB, 1, Song_1);
      Check ("remote author 2", Remote_DB, 2, Song_2);

      Diff.Update_Sync_ID;

      Check ("Sync_ID", Diff.Sync_ID, 2);
   end Insert;

   procedure Update_Delete (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use GNATCOLL.JSON;

      Local_Changes  : JSON_Array;
      Remote_Changes : JSON_Array;

      Song_1 : constant JSON_Value := Read
        ("{""ID"":1, " &
           """Modified"":""2000-01-02 00:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Arthur/C./Clarke.mp3"", " &
           """Category"":""vocal"", " &
           """Album_Artist"":""Arthur"", " &
           """Album"":""C."", " &
           """Title"":""Clarke""}}");

      Song_2 : constant JSON_Value := Read
        ("{""ID"":2, " &
           """Modified"":""2000-01-02 00:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Isaac/Asimov.mp3"", " &
           """Category"":""vocal"", " &
           """Album_Artist"":""Isaac"", " &
           --  no album
           """Title"":""Asimov""}}");

      Song_1_Delete : constant JSON_Value := Read -- delete
        ("{""ID"":1, " &
           """Deleted"":""2000-01-02 02:00:00""}");

      Song_2_Update : constant JSON_Value := Read -- Change modified, Album_Artist spelling
        ("{""ID"":2, " &
           """Modified"":""2000-01-02 01:00:00"", " &
           """Data"":" &
           "{""Album_Artist"":""Is""}}");

      Song_2_Expected : constant JSON_Value := Read
        ("{""ID"":2, " &
           """Modified"":""2000-01-02 01:00:00"", " &
           """Data"":" &
           "{""File_Name"":""Isaac/Asimov.mp3"", " &
           """Category"":""vocal"", " &
           """Album_Artist"":""Is"", " &
           --  no album
           """Title"":""Asimov""}}");

      Diff : Diff_Type :=
        (Local_DB'Access, Remote_DB'Access, Invalid_Song_ID,
         Show_Progress => null,
         Verbosity     => Test_Case (T).Verbosity);
   begin
      Append (Local_Changes, To_Insert (Song_1));
      Append (Local_Changes, To_Insert (Song_2));

      Append (Remote_Changes, To_Insert (Song_1));
      Append (Remote_Changes, To_Insert (Song_2));

      Apply (Diff, Local_Changes, Remote_Changes);

      Local_Changes  := Empty_Array;
      Remote_Changes := Empty_Array;

      Append (Local_Changes, To_Update (Song_1_Delete));

      Append (Remote_Changes, To_Update (Song_2_Update));

      Apply (Diff, Local_Changes, Remote_Changes);

      Set_Field (Song_1, "Deleted", String'(Song_1_Delete.Get ("Deleted")));
      Check ("1", Local_DB, 1, Song_1_Delete);
      Check ("2", Remote_DB, 2, Song_2_Expected);
   end Update_Delete;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("smm-database-diff-test_apply.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Insert'Access, "Insert");
      Register_Routine (T, Update_Delete'Access, "Update_Delete");
   end Register_Tests;

   overriding procedure Set_Up (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Test_SMM.Empty_Database_1;
      Test_SMM.Empty_Database_2;

      Local_Disk_DB.Open ("smm_test_1.db");
      Remote_Disk_DB.Open ("smm_test_2.db");
   end Set_Up;

end SMM.Database.Diff.Test_Apply;
