--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2016, 2018, 2019, 2025 Stephen Leake.  All Rights Reserved.
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
with SMM.Database_Remote.Disk;
with GNATCOLL.JSON.AUnit;
with Test_SMM;
package body SMM.Database.Diff.Test_Compute is

   --  Edit dbs on even days, sync on odd days
   Jan_1_2000 : aliased constant Time_String := "2000-01-01 00:00:00";
   Jan_2_2000 : aliased constant Time_String := "2000-01-02 00:00:00";
   --  Jan_3_2000 : aliased constant Time_String := "2000-01-03 00:00:00";
   Jan_4_2000 : aliased constant Time_String := "2000-01-04 00:00:00";
   Jan_5_2000 : aliased constant Time_String := "2000-01-05 00:00:00";
   --  Jan_6_2000 : aliased constant Time_String := "2000-01-06 00:00:00";
   --  Jan_7_2000 : aliased constant Time_String := "2000-01-07 00:00:00";
   --  Jan_8_2000 : aliased constant Time_String := "2000-01-08 00:00:00";
   --  Jan_9_2000 : aliased constant Time_String := "2000-01-09 00:00:00";

   --  DB_1 and DB_2 are the "real" databases; DB_Local and
   --  DB_Remote are set to either DB_1 or DB_2 to allow testing
   --  data flow in both directions (to/from remote).
   DB_1 : aliased SMM.Database.Database;
   DB_2 : aliased SMM.Database.Database;

   DB_Local  : SMM.Database_Remote.Database_Access;
   DB_Remote : SMM.Database_Remote.Database_Access;

   Sync_ID : Song_ID;

   procedure DB_1_Local
   is begin
      Database_Remote.Free (DB_Remote);
      Database_Remote.Free (DB_Local);

      DB_Local  := new SMM.Database_Remote.Disk.Database (DB_1'Access);
      DB_Remote := new SMM.Database_Remote.Disk.Database (DB_2'Access);
   end DB_1_Local;

   procedure DB_2_Local
   is begin
      Database_Remote.Free (DB_Remote);
      Database_Remote.Free (DB_Local);

      DB_Local := new SMM.Database_Remote.Disk.Database (DB_2'Access);
      DB_Remote := new SMM.Database_Remote.Disk.Database (DB_1'Access);
   end DB_2_Local;
   pragma Unreferenced (DB_2_Local);

   procedure Insert_Song
     (DB           : in out SMM.Database.Database;
      ID           : in     Song_ID;
      File_Name    : in     String;
      Album_Artist : in     String;
      Album        : in     String;
      Title        : in     String;
      Modified     : in     Time_String)
   is begin
      DB.Insert
        (ID           => ID,
         File_Name    => File_Name,
         Category     => "vocal",
         Artist       => "",
         Album_Artist => Album_Artist,
         Composer     => "",
         Album        => Album,
         Year         => No_Year,
         Title        => Title,
         Track        => No_Track,
         Modified     => Modified);
   end Insert_Song;

   procedure Fill_Database (DB : in out SMM.Database.Database)
   is
   begin
      Insert_Song (DB, 1, "Arthur/C./Clarke.mp3", "Arthur", "C.", "Clarke", Jan_2_2000);
      Insert_Song (DB, 2, "Isaac/Asimov.mp3", "Isaac", "", "Asimov", Jan_2_2000);
      Insert_Song (DB, 3, "Stanley Kubrick/2001.mp3", "Stanley Kubrick", "", "2001", Jan_2_2000);

      --  No edits yet.

      --  No sync yet.
      Sync_ID := Invalid_Song_ID;
   end Fill_Database;

   procedure Check
     (Label    : in String;
      Computed : in GNATCOLL.JSON.JSON_Array;
      Expected : in GNATCOLL.JSON.JSON_Array)
   is begin
      GNATCOLL.JSON.AUnit.Check (Label, Computed, Expected);
   end Check;

   ----------
   --  Test procedures

   procedure Empty_Remote (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use Ada.Containers;
      use AUnit.Checks;
      use GNATCOLL.JSON;

      List : ID_Lists.List;

      Diff : Diff_Type :=
        (DB_Local, DB_Remote, Sync_ID,
         Show_Progress  => null,
         Verbosity      => Test_Case (T).Verbosity);

      Remote_Changes : JSON_Array;
      Expected       : JSON_Array;

      procedure Set_Expected (Max_Changes : in Ada.Containers.Count_Type)
      is begin
         Clear (Expected);

         List := DB_Local.Get_New (Diff.Sync_ID, Max_Changes);
         for ID of List loop
            Append (Expected, To_Insert (DB_Local.Get_JSON (ID)));
         end loop;
      end Set_Expected;

   begin
      --  Nothing in DB_Remote; DB_Local has data from Fill_Database. Doing
      --  Init_Remote, in 2 stages.

      Set_Expected (Max_Changes => 2);
      Check ("1 expected length", Length (Expected), 2);
      Init_Remote (Diff, Max_Changes => 2, Remote_Changes => Remote_Changes);
      Check ("1 changes", Remote_Changes, Expected);
      Check ("1 sync_id", Diff.Sync_ID, 2);

      Set_Expected (Max_Changes => 2);
      Check ("2 expected length", Length (Expected), 1);
      Init_Remote (Diff, Max_Changes => 2, Remote_Changes => Remote_Changes);
      Check ("2 changes", Remote_Changes, Expected);
      Check ("2 sync_id", Diff.Sync_ID, 3);
   end Empty_Remote;

   procedure Same (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use GNATCOLL.JSON;

      Diff           : Diff_Type;
      Local_Changes  : JSON_Array;
      Conflicts      : JSON_Array;
      Remote_Changes : JSON_Array;
   begin
      --  Same stuff in DB_Local, DB_Remote
      Fill_Database (DB_2);

      Diff :=
        (DB_Local, DB_Remote, Invalid_Song_ID,
         Show_Progress => null,
         Verbosity     => Test_Case (T).Verbosity);

      --  All DB modified set to Jan_2_2000, so this finds and compares all
      --  records, and they all match.

      Inc_Diff (Diff, Jan_1_2000, Local_Changes, Conflicts, Remote_Changes);

      Check ("1 Local", Local_Changes, Empty_Array);
      Check ("1 Conflicts", Conflicts, Empty_Array);
      Check ("1 Remote", Remote_Changes, Empty_Array);
   end Same;

   procedure New_Stuff (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      use GNATCOLL.JSON;

      Diff           : Diff_Type;
      Local_Changes  : JSON_Array;
      Conflicts      : JSON_Array;
      Remote_Changes : JSON_Array;
      Temp           : JSON_Value;
      Expected       : JSON_Array;

      Last_ID : constant Song_ID := DB_1.Last_ID;
   begin
      --  New stuff in local (= DB_1)
      Sync_ID := Last_ID;

      Insert_Song (DB_1, Last_ID + 1, "Isaac Asimov/Pebble in the Sky/Early Earth vs Trantor.mp3",
                   "Isaac Asimov", "Pebble in the Sky", "Early Earth vs Trantor", Jan_4_2000);
      Append (Expected, To_Insert (DB_1.Get_JSON (Last_ID + 1)));

      Insert_Song (DB_1, Last_ID + 2, "Arthur C. Clark/A Space Odessey/2010.mp3",
                   "Arthur C. Clark", "A Space Odessey", "2010", Jan_4_2000);
      Append (Expected, To_Insert (DB_1.Get_JSON (Last_ID + 2)));

      Insert_Song (DB_1, Last_ID + 3, "Arthur C. Clark/Rendezvous With Rama/Rama 1.mp3",
                   "Arthur C. Clark", "Rendezvous With Rama", "Rama 1", Jan_4_2000);
      Append (Expected, To_Insert (DB_1.Get_JSON (Last_ID + 3)));

      Diff :=
        (DB_Local, DB_Remote, Sync_ID,
         Show_Progress  => null,
         Verbosity      => Test_Case (T).Verbosity);

      Inc_Diff (Diff, Jan_5_2000, Local_Changes, Conflicts, Remote_Changes);

      Check ("1 Local", Local_Changes, Empty_Array);
      Check ("1 Conflicts", Conflicts, Empty_Array);
      Check ("1 Remote", Remote_Changes, Expected);
   end New_Stuff;

   --  procedure Colliding (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   --  is
   --     pragma Unreferenced (T);
   --     use GNATCOLL.JSON;
   --     use Standard.AUnit.Checks;

   --     Diff               : Diff_Type;
   --     Local_Changes      : JSON_Array;
   --     Conflicts          : JSON_Array;
   --     Remote_Changes     : JSON_Array;
   --     Expected_Local     : JSON_Array;
   --     Expected_Remote    : JSON_Array;
   --     Expected_Conflicts : JSON_Array;
   --  begin
   --     --  DB_1 is local

   --     --  This has the same title index as New_Stuff DB_2 7, but lacks the
   --     --  comment. It could be treated as an update, but this is not likely
   --     --  in general, so we treat it as a modified/modified conflict.

   --     Title_Table.Insert ("Pebble in the Sky", 1950, True, "", "paperback", Jan_4_2000); -- 7
   --     Append
   --       (Expected_Conflicts, To_Conflict
   --          (Title,
   --           Title_Table.Get_JSON,
   --           Read
   --             ("{""ID"":7, ""Modified"":""2000-01-02 00:00:00"", " &
   --                """Data"":{" &
   --                """Title"":""Pebble in the Sky"", " &
   --                """Year"":1950, " &
   --                """Comment"":""Early Earth vs Trantor"", " &
   --                """Location"":""paperback""}}")
   --          ));

   --     --  This has the same title index as New_Stuff DB_2 8, but lacks
   --     --  a comment and has a conflicting location;
   --     --  modified/modified conflict.

   --     Title_Table.Insert ("2010", 1982, True, "back to Jupiter", "paperback", Jan_4_2000); -- 8
   --     Append
   --       (Expected_Conflicts,
   --        To_Conflict
   --          (Title,
   --           Title_Table.Get_JSON,
   --           Read
   --             ("{""ID"":8, ""Modified"":""2000-01-02 00:00:00"", " &
   --                """Data"":{" &
   --                """Title"":""2010"", " &
   --                """Year"":1982, " &
   --                --  no comment
   --                """Location"":""hardcover""}}") -- conflicting location
   --          ));

   --     --  Different title index; renumber local, insert remote, insert local
   --     Title_Table.Insert ("The Hammer of God", 1992, True, "", "", Jan_4_2000); -- 9b

   --     Append (Expected_Local, To_Renumber (Title, Old_ID => 9, New_ID => 10));
   --     --  renumbers data 9b to 10, updates link 5b:2-9 to 6:2-10

   --     Append
   --       (Expected_Local,
   --        To_Insert
   --          (Title,
   --           Read
   --             ("{""ID"":9, ""Modified"":""2000-01-02 00:00:00"", " &
   --                """Data"":{" &
   --                """Title"":""Rendezvous With Rama"", " &
   --                """Year"":1973, " &
   --                """Comment"":""alien ship"", " &
   --                """Location"":""hardcover""}}")));
   --     Append
   --       (Expected_Remote,
   --        To_Insert
   --          (Title,
   --           Read
   --             ("{""ID"":10, ""Modified"":""2000-01-04 00:00:00"", " &
   --                """Data"":{" &
   --                """Title"":""The Hammer of God"", " &
   --                """Year"":1992}}")));

   --     Links (Author, Title).Insert ((2, 7), Jan_4_2000); -- 3 same
   --     Links (Author, Title).Insert ((1, 8), Jan_4_2000); -- 4 same
   --     Links (Author, Title).Insert ((2, 9), Jan_4_2000); -- 5b conflict

   --     Diff :=
   --       (DB_Local, DB_Remote, Sync_Data_IDs, Sync_Link_IDs,
   --        Show_Progress => null,
   --        Verbosity     => 0);

   --     Inc_Diff_Data (Diff, Jan_3_2000, Local_Changes, Conflicts, Remote_Changes);

   --     Check ("1 Local", Local_Changes, Expected_Local);
   --     Check ("1 Conflicts", Conflicts, Expected_Conflicts);
   --     Check ("1 Remote", Remote_Changes, Expected_Remote);

   --     Diff.Apply (Local_Changes, Remote_Changes);

   --     Inc_Diff_Links (Diff, Jan_3_2000, Local_Changes, Remote_Changes);

   --     Append (Expected_Links_Local, To_Renumber (Author, Title, Old_ID => 5, New_ID => 6));
   --     Append
   --       (Expected_Links_Local,
   --        To_Insert
   --          (Author, Title,
   --           Read ("{""ID"":5, ""Modified"":""2000-01-02 00:00:00"", ""Data"":{""Author"":1, ""Title"":9}}")));
   --     Append
   --       (Expected_Links_Remote,
   --        To_Insert
   --          (Author, Title,
   --           Read ("{""ID"":6, ""Modified"":""2000-01-04 00:00:00"", ""Data"":{""Author"":2, ""Title"":10}}")));

   --     Check ("1 links Local", Local_Changes, Expected_Links_Local);
   --     Check ("1 links Remote", Remote_Changes, Expected_Links_Remote);

   --     Diff.Apply (Local_Changes, Remote_Changes);

   --     Diff.Update_IDs;
   --     Sync_Data_IDs := Diff.Sync_Data_IDs;
   --     Sync_Link_IDs := Diff.Sync_Link_IDs;

   --     Check ("Sync_Data_IDs (Author)", Sync_Data_IDs (Author), 2);
   --     Check ("Sync_Data_IDs (Title)", Sync_Data_IDs (Title), 10);
   --     Check ("Sync_Link_IDs (Author, Title)", Sync_Link_IDs (Author, Title), 6);

   --     --  Manually resolve the conflicts
   --     Title_Table.Fetch (7);
   --     Title_Table.Update ("Pebble in the Sky", 1950, True, "Early Earth vs Trantor", "paperback", Jan_7_2000);
   --     Title_Table.Fetch (8);
   --     Title_Table.Update ("2010", 1982, True, "", "hardcover", Jan_7_2000);
   --  end Colliding;

   --  procedure Update (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   --  is
   --     pragma Unreferenced (T);
   --     use GNATCOLL.JSON;

   --     Diff           : Diff_Type;
   --     Local_Changes  : JSON_Array;
   --     Conflicts      : JSON_Array;
   --     Remote_Changes : JSON_Array;
   --     Expected       : JSON_Array;
   --  begin
   --     --  DB_1 is local

   --     --  Change comment in a previously synced record (drop "Early")
   --     Title_Table.Fetch (7);
   --     Title_Table.Update ("Pebble in the Sky", 1950, True, "Earth vs Trantor", "paperback", Jan_8_2000);
   --     Append (Expected, To_Update (Title, Title_Table.Get_JSON));

   --     --  Can't update links, so no test for that.

   --     Diff :=
   --       (DB_Local, DB_Remote, Sync_Data_IDs, Sync_Link_IDs,
   --        Show_Progress => null,
   --        Verbosity     => 0);

   --     Inc_Diff_Data (Diff, Jan_6_2000, Local_Changes, Conflicts, Remote_Changes);

   --     Check ("1 Local", Local_Changes, Empty_Array);
   --     Check ("1 Conflicts", Conflicts, Empty_Array);
   --     Check ("1 Remote", Remote_Changes, Expected);

   --     DB_2_Local;

   --     Diff :=
   --       (DB_Local, DB_Remote, Sync_Data_IDs, Sync_Link_IDs,
   --        Show_Progress => null,
   --        Verbosity     => 0);

   --     Inc_Diff_Data (Diff, Jan_6_2000, Local_Changes, Conflicts, Remote_Changes);

   --     Check ("2 Local", Local_Changes, Expected);
   --     Check ("2 Conflicts", Conflicts, Empty_Array);
   --     Check ("2 Remote", Remote_Changes, Empty_Array);

   --     --  get dbs in sync for next test
   --     Diff.Apply (Local_Changes, Remote_Changes);
   --  end Update;

   --  procedure Delete (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   --  is
   --     pragma Unreferenced (T);
   --     use Standard.AUnit.Checks;
   --     use GNATCOLL.JSON;

   --     Diff                : Diff_Type;
   --     Local_Changes       : JSON_Array;
   --     Conflicts           : JSON_Array;
   --     Remote_Changes      : JSON_Array;
   --     Conflicts_Expected  : JSON_Array;
   --     DB_1_Data_Expected  : JSON_Array;
   --     DB_1_Links_Expected : JSON_Array;
   --  begin
   --     --  Delete previously synced records, also update a matching one
   --     DB_2_Local;

   --     Title_Table.Mark_Deleted (7, Jan_9_2000); -- delete in DB_2 only
   --     Append
   --       (DB_1_Data_Expected,
   --        To_Update (Title, Read ("{""ID"":7, ""Deleted"":""2000-01-09 00:00:00""}")));

   --     Links (Author, Title).Fetch (3); -- Delete in DB_2 only
   --     Check ("1 link first", Links (Author, Title).ID (Author), 2);
   --     Check ("1 link last", Links (Author, Title).ID (Title), 7);
   --     Links (Author, Title).Mark_Deleted (3, Jan_9_2000);
   --     Append
   --       (DB_1_Links_Expected,
   --        To_Update (Author, Title, Read ("{""ID"":3, ""Deleted"":""2000-01-09 00:00:00""}")));

   --     Title_Table.Fetch (6); -- delete in DB_1, update in DB_2
   --     Title_Table.Update ("Foundation and Empire", 1952, True, "a comment", "paperback", Jan_9_2000);

   --     DB_1_Local;

   --     Title_Table.Mark_Deleted (6, Jan_9_2000); -- delete in DB_1, update in DB_2
   --     Append
   --       (Conflicts_Expected,
   --        To_Conflict
   --          (Title,
   --           Local_JSON  => Read
   --             ("{""ID"":6, ""Modified"":""2000-01-09 00:00:00"", " &
   --                """Data"":{" &
   --                """Title"":""Foundation and Empire"", " &
   --                """Year"":1952, " &
   --                """Comment"":""a comment"", " &
   --                """Location"":""paperback""}}"),
   --           Remote_JSON => Read ("{""ID"":6, ""Deleted"":""2000-01-09 00:00:00""}")));

   --     DB_2_Local;

   --     Diff :=
   --       (DB_Local, DB_Remote, Sync_Data_IDs, Sync_Link_IDs,
   --        Show_Progress => null,
   --        Verbosity     => 0);

   --     Inc_Diff_Data (Diff, Jan_8_2000, Local_Changes, Conflicts, Remote_Changes);

   --     Check ("1 Local", Local_Changes, Empty_Array);
   --     Check ("1 Remote", Remote_Changes, DB_1_Data_Expected);
   --     Check ("1 Conflicts", Conflicts, Conflicts_Expected);

   --     Inc_Diff_Links (Diff, Jan_8_2000, Local_Changes, Remote_Changes);

   --     Check ("1 links Local", Local_Changes, Empty_Array);
   --     Check ("1 links Remote", Remote_Changes, DB_1_Links_Expected);

   --     --  Diff in other direction.
   --     DB_1_Local;

   --     Diff :=
   --       (DB_Local, DB_Remote, Sync_Data_IDs, Sync_Link_IDs,
   --        Show_Progress => null,
   --        Verbosity     => 0);

   --     Inc_Diff_Data (Diff, Jan_8_2000, Local_Changes, Conflicts, Remote_Changes);

   --     Conflicts_Expected := Empty_Array;
   --     Append
   --       (Conflicts_Expected,
   --        To_Conflict
   --          (Title,
   --           Remote_JSON  => Read
   --             ("{""ID"":6, ""Modified"":""2000-01-09 00:00:00"", " &
   --                """Data"":{" &
   --                """Title"":""Foundation and Empire"", " &
   --                """Year"":1952, " &
   --                """Comment"":""a comment"", " &
   --                """Location"":""paperback""}}"),
   --           Local_JSON => Read ("{""ID"":6, ""Deleted"":""2000-01-09 00:00:00""}")));

   --     Check ("2 Local", Local_Changes, DB_1_Data_Expected);
   --     Check ("2 Conflicts", Conflicts, Conflicts_Expected);
   --     Check ("2 Remote", Remote_Changes, Empty_Array);

   --     Inc_Diff_Links (Diff, Jan_8_2000, Local_Changes, Remote_Changes);

   --     Check ("2 links Local", Local_Changes, DB_1_Links_Expected);
   --     Check ("2 links Remote", Remote_Changes, Empty_Array);

   --     --  get dbs in sync for next test
   --     Title_Table.Mark_Deleted (7, Jan_9_2000);
   --     Links (Author, Title).Mark_Deleted (3, Jan_9_2000);
   --  end Delete;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("smm-database-diff-test_compute.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Empty_Remote'Access, "Empty_Remote");
      Register_Routine (T, Same'Access, "Same");
      Register_Routine (T, New_Stuff'Access, "New_Stuff");
      --  Register_Routine (T, Colliding'Access, "Colliding");
      --  Register_Routine (T, Update'Access, "Update");
      --  Register_Routine (T, Delete'Access, "Delete");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      Test_SMM.Empty_Database_1;
      Test_SMM.Empty_Database_2;

      DB_1_Local;
      DB_1.Open ("smm_test_1.db");
      Fill_Database (DB_1);

      Open (DB_2, "smm_test_2.db");
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Database_Remote.Free (DB_Remote);
      Database_Remote.Free (DB_Local);
   end Tear_Down_Case;

end SMM.Database.Diff.Test_Compute;
