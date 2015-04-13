--  Abstract :
--
--  See spec
--
--  Copyright (C) 2007 - 2009, 2015 Stephen Leake.  All Rights Reserved.
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
with SAL.Config_Files;
with SMM;
package body Test_Play_Before is

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use SMM.Song_Lists;
      use SAL.Config_Files;

      Db : Configuration_Type;

      Songs  : List_Type;
      Song_I : SMM.Song_Lists.Iterator_Type;

      procedure Check
        (I        : in out SMM.Song_Lists.Iterator_Type;
         Expected : in     String)
      is
         use AUnit.Checks;
      begin
         Check ("", SAL.Config_Files.Read (Db, Current (I), "File"), Expected);
         Next (I);
      end Check;

   begin
      --  Create the test environment; a config file with some
      --  Play_Before items.

      Open (Db, "tmp/smm.db", Read_Only => False, Duplicate_Key => Raise_Exception);
      Write (Db, "Songs. 1.File", "intro_1.mp3");
      Write (Db, "Songs. 1.Play_Before", "2");
      Write (Db, "Songs. 1.Last_Downloaded", "2.0");
      Write (Db, "Songs. 2.File", "song_1.mp3");
      Write (Db, "Songs. 2.Last_Downloaded", "1.0");
      Write (Db, "Songs. 3.File", "song_3.mp3");
      Write (Db, "Songs. 3.Last_Downloaded", "3.0");
      Write (Db, "Songs. 4.File", "intro_5.mp3");
      Write (Db, "Songs. 4.Play_Before", "5");
      Write (Db, "Songs. 4.Last_Downloaded", "4.0");
      Write (Db, "Songs. 5.File", "song_5.mp3");
      Write (Db, "Songs. 5.Last_Downloaded", "5.0");

      Append (Songs, Find (Db, "Songs", " 1"));
      Append (Songs, Find (Db, "Songs", " 3"));
      Append (Songs, Find (Db, "Songs", " 2"));
      Append (Songs, Find (Db, "Songs", " 4"));

      SMM.Play_Before (Db, Songs);

      Song_I := SMM.Song_Lists.First (Songs);

      Check (Song_I, "intro_1.mp3");
      Check (Song_I, "song_1.mp3");
      Check (Song_I, "song_3.mp3");
      Check (Song_I, "intro_5.mp3");
      Check (Song_I, "song_5.mp3");

      Close_No_Save (Db);

   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Play_Before");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_Play_Before;
