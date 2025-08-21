--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019, 2025 Stephen Leake All Rights Reserved.
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

with Ada.Text_IO;
package body SMM.Song_Lists is

   procedure Least_Recent_Songs
     (DB                : in     SMM.Database.Database;
      Category          : in     String;
      Songs             :    out Song_Lists.List;
      Song_Count        : in     Ada.Containers.Count_Type;
      New_Song_Count    : in     Ada.Containers.Count_Type;
      Over_Select_Ratio : in     Float;
      Seed              : in     Integer := 0)
   is
      --  Requirements:
      --
      --  When the database is new, we want to select random sets of songs.
      --
      --  When all songs have a non-zero Last_Downloaded, we want the
      --  least recently downloaded, but we still want to mix them up
      --  so the play order is not constant each time thru the
      --  database.
      --
      --  When a new album is added, we want it to be mixed in
      --  gradually, not all in one playlist. We do this by only
      --  including New_Song_Count songs from the set with zero
      --  Last_Downloaded (if there are enough non-zero
      --  Last_Downloaded).
      --
      --  Algorithm:
      --
      --  First read all songs with matching category into Time_List,
      --  grouped by Last_Downloaded.
      --
      --  Then build a list of Over_Select * Song_Count songs from
      --  least recent sections
      --
      --  Randomize list, return Song_Count songs from it.

      use Ada.Containers;
      use Song_Lists;

      Count_Limit  : constant Count_Type := Count_Type (Float (Song_Count) * Over_Select_Ratio);

      DB_I      : SMM.Database.Cursor := SMM.Database.First_By_Last_Downloaded (DB); -- oldest date
      New_Songs : Song_Lists.List;
      New_Song_Added_Count : Count_Type := 0;
   begin
      loop
         exit when Songs.Length >= Count_Limit;

         if not DB_I.Has_Element then
            raise SAL.Parameter_Error with "'" & Category & "' doesn't match" &
              (if Songs.Length = 0 then "any" else "enough") & " songs";
         end if;

         if DB_I.Category_Contains (Category) and
           (not DB_I.Category_Contains ("dont_play")) and
           (not DB_I.Play_After_Is_Present) -- only play this when Play_Before is included.
         then
            if DB_I.Last_Downloaded = SMM.Database.Default_Time_String then
               if New_Song_Added_Count < New_Song_Count then
                  if Verbosity >= 2 then
                     Ada.Text_IO.Put_Line ("New:" & DB_I.ID'Image & " " & DB_I.Last_Downloaded);
                  end if;
                  New_Songs.Append (DB_I.ID);
                  New_Song_Added_Count := @ + 1;
               end if;
            else
               if Verbosity >= 2 then
                  Ada.Text_IO.Put_Line ("adding" & DB_I.ID'Image & " " & DB_I.Last_Downloaded);
               end if;
               Songs.Append (DB_I.ID);
            end if;
         end if;

         DB_I.Next;
      end loop;

      Randomize (Songs, Seed);
      if Songs.Length > Song_Count then
         --  This may delete some new songs, but they'll get added
         --  next time.
         Songs.Delete_Last (Songs.Length - Song_Count);
      end if;

      Play_Before (DB, Songs);
   end Least_Recent_Songs;

   procedure Play_Before
     (DB    : in     SMM.Database.Database;
      Songs : in out Song_Lists.List)
   is
      use SMM.Database;

      type Item_Type is record
         First_Song_Songs  : Song_Lists.Cursor;
         First_Song_ID     : Integer;
         Second_Song_ID    : Integer;
         Second_Song_Songs : Song_Lists.Cursor;
      end record;

      package Item_Lists is new Ada.Containers.Doubly_Linked_Lists (Item_Type);

      Have_Play_Before : Item_Lists.List;
      Item_I           : Item_Lists.Cursor;

      use Item_Lists;
      use Song_Lists;

      Songs_I : Song_Lists.Cursor := First (Songs);

   begin
      Fill_Have_Play_Before :
      loop
         exit Fill_Have_Play_Before when Songs_I = Song_Lists.No_Element;

         if Find_ID (DB, Element (Songs_I)).Play_Before_Is_Present then
            declare
               Song_ID   : constant Integer := Element (Songs_I);
               Before_ID : constant Integer := Find_ID (DB, Song_ID).Play_Before;
            begin
               if Before_ID = Song_ID then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line
                    ("db ERROR: " & Integer'Image (Song_ID) & ".Play_Before = " & Integer'Image (Before_ID));
                  Ada.Text_IO.New_Line;
               else
                  Append
                    (Have_Play_Before,
                     (First_Song_Songs  => Songs_I,
                      First_Song_ID     => Song_ID,
                      Second_Song_ID    => Before_ID,
                      Second_Song_Songs => Song_Lists.No_Element));

               end if;
            end;
         end if;

         Next (Songs_I);
      end loop Fill_Have_Play_Before;

      Item_I := First (Have_Play_Before);
      Find_Second_Song :
      loop
         --  FIXME: we never insert a play_after in Least_Recent_Songs, so this
         --  loop is not needed.
         exit Find_Second_Song when Item_I = Item_Lists.No_Element;

         declare
            Item : Item_Type renames Have_Play_Before.Reference (Item_I);
         begin
            Songs_I := First (Songs);
            Find_Song :
            loop
               exit Find_Song when Songs_I = Song_Lists.No_Element;
               if Item.Second_Song_ID = Element (Songs_I) then
                  Item.Second_Song_Songs := Songs_I;
                  exit Find_Song;
               end if;

               Next (Songs_I);

            end loop Find_Song;
         end;

         Next (Item_I);
      end loop Find_Second_Song;

      Item_I := First (Have_Play_Before);
      Place_Second_Song :
      loop
         exit Place_Second_Song when Item_I = Item_Lists.No_Element;

         declare
            Item : Item_Type renames Element (Item_I);
         begin
            if Item.Second_Song_Songs = Song_Lists.No_Element then
               --  not in Songs yet
               if Verbosity >= 2 then
                  Ada.Text_IO.Put_Line ("adding" & Item.Second_Song_ID'Image);
               end if;
               Insert
                 (Container => Songs,
                  Before    => Next (Item.First_Song_Songs),
                  New_Item  => Item.Second_Song_ID);
            else
               Splice
                 (Container => Songs,
                  Position  => Item.Second_Song_Songs,
                  Before    => Next (Item.First_Song_Songs));
            end if;
         end;

         Next (Item_I);
      end loop Place_Second_Song;

   end Play_Before;

end SMM.Song_Lists;
