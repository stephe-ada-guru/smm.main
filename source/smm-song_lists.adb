--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

      Min_Randomize_Count : constant Count_Type := Count_Type (Over_Select_Ratio * Float (Song_Count));
      Time_List           : Time_Lists.List;

      procedure Finish
      is begin
         Randomize (Songs, Seed);
         if Songs.Length > Song_Count then
            Songs.Delete_Last (Songs.Length - Song_Count);
         end if;
         Play_Before (DB, Songs);
      end Finish;

      procedure Add_All (Time_List_I : in Time_Lists.Cursor)
      is begin
         if Verbosity > 0 then
            Ada.Text_IO.Put_Line ("adding songs from " & Time_Lists.Element (Time_List_I).Last_Downloaded);
         end if;

         Song_Lists.Splice
           (Source => Time_Lists.Reference (Time_List, Time_List_I).Songs,
            Target => Songs,
            Before => Song_Lists.No_Element);
      end Add_All;

      Time_List_I : Time_Lists.Cursor;

      use Song_Lists;
      use Time_Lists;
   begin
      declare
         All_Songs_I : SMM.Database.Cursor := SMM.Database.First (DB);
      begin
         loop
            --  Note that we can't exit on finding Count songs; there
            --  may be more later that were downloaded less recently.
            --  DB is sorted on ID.
            --  FIXME: provide DB iterator sorted on last_downloaded.
            exit when not All_Songs_I.Has_Element;

            if Category = All_Songs_I.Category and
              (not All_Songs_I.Play_After_Is_Present) -- only play this when Play_Before is included.
            then
               Insert (DB, All_Songs_I.ID, Time_List);
            end if;

            All_Songs_I.Next;
         end loop;
      end;

      Time_List_I := Time_List.First;

      --  FIXME: Time_List_I can be null if Category doesn't match any songs (ie spelled wrong).

      if Element (Time_List_I).Last_Downloaded = SMM.Database.Default_Time_String then
         --  New songs

         if Time_List.Length = 1 then
            --  New db; all songs have default Last_Downloaded
            Ada.Text_IO.Put_Line ("new db; all new songs");
            Songs := Element (Time_List_I).Songs;
            Finish;
            return;

         elsif Element (Time_List_I).Songs.Length > New_Song_Count then
            --  Only include a few new songs
            if Verbosity > 0 then
               Ada.Text_IO.Put_Line ("adding " & Count_Type'Image (New_Song_Count) & " new songs");
            end if;

            declare
               Source  : Song_Lists.List renames Element (Time_List_I).Songs;
               Songs_I : Song_Lists.Cursor := Source.First;
            begin
               for I in 1 .. New_Song_Count loop
                  Songs.Prepend (Element (Songs_I));
                  Next (Songs_I);
               end loop;
            end;
            Next (Time_List_I);
         else
            --  There are only a few new songs; include them all
            if Verbosity > 0 then
               Ada.Text_IO.Put_Line ("adding " & Count_Type'Image (Element (Time_List_I).Songs.Length) & " new songs");
            end if;
            Songs := Element (Time_List_I).Songs;
            Next (Time_List_I);
         end if;
      end if;

      loop
         exit when Songs.Length >= Min_Randomize_Count or Time_List_I = Time_Lists.No_Element;

         Add_All (Time_List_I);

         Next (Time_List_I);
      end loop;

      Finish;

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

   procedure Insert
     (DB   : in     SMM.Database.Database;
      Item : in     Integer;
      List : in out Time_Lists.List)
   is
      use SMM.Database;
      use Time_Lists;

      Item_Cur                : constant SMM.Database.Cursor    := Find_ID (DB, Item);
      Last_Downloaded         : constant Time_String            := Item_Cur.Last_Downloaded;
      Last_Downloaded_Element : constant Time_List_Element_Type := (Last_Downloaded, Song_Lists.Empty_List);

      I : Time_Lists.Cursor := List.Find (Last_Downloaded_Element);
   begin
      if I = No_Element then
         declare
            Temp_List : Time_Lists.List;
         begin
            Temp_List.Prepend ((Last_Downloaded, Song_Lists.Empty_List));
            Time_Lists_Sorting.Merge
              (Target => List,
               Source => Temp_List);
         end;
         I := List.Find (Last_Downloaded_Element);
      end if;

      List.Reference (I).Songs.Append (Item);
   end Insert;

end SMM.Song_Lists;
