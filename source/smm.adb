--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008, 2009, 2011 - 2014 Stephen Leake.  All Rights Reserved.
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

with Ada.IO_Exceptions;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Config_Files.Integer;
with SAL.Gen.Alg.Find_Linear.Sorted;
with SAL.Poly.Lists.Double.Gen_Randomize;
package body SMM is

   function Normalize (Path : in String) return String
   is
      Result : String := Path;
   begin
      for I in Result'Range loop
         if Result (I) = '\' then
            Result (I) := '/';
         end if;
      end loop;
      return Result;
   end Normalize;

   function Relative_Name
     (Root      : in String;
      Full_Name : in String)
      return String
   is begin
      if Root = Full_Name (Full_Name'First .. Full_Name'First + Root'Length - 1) then
         return Full_Name (Full_Name'First + Root'Length .. Full_Name'Last);
      else
         raise SAL.Programmer_Error with Full_Name & " not relative to root " & Root;
      end if;
   end Relative_Name;

   function As_Directory (Path : in String) return String
   is
      Temp : constant String := Normalize (Path);
   begin
      if Temp (Temp'Last) = '/' then
         return Temp;
      else
         return Temp & '/';
      end if;
   end As_Directory;

   Last_Downloaded_Key : constant String := "Last_Downloaded";

   function Read_Last_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type
   is
      use SAL.Config_Files;
      use SAL.Time_Conversions;
      Temp : constant String := Read (Db, I, Last_Downloaded_Key);
   begin
      if Temp'Length = Extended_ASIST_Time_String_Type'Last and then Temp (5) = '-' then
         --  ASIST string
         return To_TAI_Time (Temp, Absolute => True);
      else
         --  Time_type'image; old db, or unit test.
         return Time_Type'Value (Temp);
      end if;
   end Read_Last_Downloaded;

   procedure Write_Last_Downloaded
     (Db   : in out SAL.Config_Files.Configuration_Type;
      I    : in     SAL.Config_Files.Iterator_Type;
      Time : in     SAL.Time_Conversions.Time_Type)
   is
      use SAL.Config_Files;
   begin
      if Is_Present (Db, I, Last_Downloaded_Key) then
         Write (Db, I, Prev_Downloaded_Key,  Read (Db, I, Last_Downloaded_Key));
      end if;
      SAL.Config_Files.Write (Db, I, Last_Downloaded_Key, SAL.Time_Conversions.To_Extended_ASIST_String (Time));
   end Write_Last_Downloaded;

   procedure Write_Last_Downloaded
     (Db       : in out SAL.Config_Files.Configuration_Type;
      Root_Key : in     String;
      Time     : in     SAL.Time_Conversions.Time_Type)
   is
      use SAL.Config_Files;
      Key : constant String := Root_Key & "." & Last_Downloaded_Key;
   begin
      if Is_Present (Db, Key) then
         Write (Db, Root_Key & "." & Prev_Downloaded_Key,  Read (Db, Key));
      end if;
      Write (Db, Key, SAL.Time_Conversions.To_Extended_ASIST_String (Time));
   end Write_Last_Downloaded;

   procedure Randomize is new Song_Lists.Gen_Randomize;

   type Time_List_Element_Type is record
      Last_Downloaded : SAL.Time_Conversions.Time_Type;
      Songs           : Song_Lists.List_Type;
   end record;

   type Time_List_Node_Type is access Time_List_Element_Type;

   package Time_Lists_Aux is new SAL.Aux.Indefinite_Private_Items (Time_List_Element_Type, Time_List_Node_Type);
   package Time_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => Time_List_Element_Type,
      Item_Node_Type    => Time_List_Node_Type,
      To_Item_Node      => Time_Lists_Aux.To_Item_Node,
      Free_Item         => Time_Lists_Aux.Free_Item,
      Copy              => Time_Lists_Aux.Copy_Item_Node,
      Node_Storage_Pool => SAL.Storage_Pools.Integer_Access_Type'Storage_Pool);

   package Time_Lists_Algorithms is new SAL.Gen.Alg
     (Item_Node_Type => Time_List_Node_Type,
      Container_Type => Time_Lists.List_Type,
      Iterator_Type  => Time_Lists.Iterator_Type,
      Current        => Time_Lists.Current,
      First          => Time_Lists.First,
      Last           => Time_Lists.Last,
      None           => Time_Lists.None,
      Is_Null        => Time_Lists.Is_Null,
      Next_Procedure => Time_Lists.Next,
      Next_Function  => Time_Lists.Next);

   function Count is new Time_Lists_Algorithms.Count;

   function Is_Equal (Left : in Time_List_Node_Type; Right : in SAL.Time_Conversions.Time_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded = Right;
   end Is_Equal;

   package Time_Lists_Find is new Time_Lists_Algorithms.Find_Linear
     (Item_Type         => Time_List_Element_Type,
      Key_Type          => SAL.Time_Conversions.Time_Type,
      Is_Equal_Node_Key => Is_Equal,
      Delete            => Time_Lists.Delete,
      Insert_Before     => Time_Lists.Insert_Before);

   function Is_Equal (Left : in Time_List_Node_Type; Right : in Time_List_Element_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded = Right.Last_Downloaded;
   end Is_Equal;

   function Is_Equal (Left, Right : in Time_List_Node_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded = Right.Last_Downloaded;
   end Is_Equal;

   function Is_Greater_Equal (Left : in Time_List_Node_Type; Right : in Time_List_Element_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded >= Right.Last_Downloaded;
   end Is_Greater_Equal;

   function Is_Greater_Equal (Left, Right : in Time_List_Node_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded >= Right.Last_Downloaded;
   end Is_Greater_Equal;

   function Is_Greater_Equal (Left : in Time_List_Node_Type; Right : in SAL.Time_Conversions.Time_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded >= Right;
   end Is_Greater_Equal;

   package Time_Lists_Find_Sorted is new Time_Lists_Find.Sorted
     (Is_Equal_Node_Item         => Is_Equal,
      Is_Equal_Node_Node         => Is_Equal,
      Is_Greater_Equal_Node_Item => Is_Greater_Equal,
      Is_Greater_Equal_Node_Key  => Is_Greater_Equal,
      Is_Greater_Equal_Node_Node => Is_Greater_Equal,
      Prev_Function              => Time_Lists.Prev,
      Prev_Procedure             => Time_Lists.Prev,
      Splice_Before              => Time_Lists.Splice_Before);

   procedure Insert
     (Db   : in     SAL.Config_Files.Configuration_Type;
      List : in out Time_Lists.List_Type;
      Item : in     SAL.Config_Files.Iterator_Type)
   is
      Last_Downloaded : constant SAL.Time_Conversions.Time_Type := Read_Last_Downloaded (Db, Item);

      I : Time_Lists.Iterator_Type := Time_Lists_Find.Find_Equal
        (Start => Time_Lists.First (List),
         Key   => Last_Downloaded);
   begin
      if Time_Lists.Is_Null (I) then
         Time_Lists_Find_Sorted.Add
           (Container     => List,
            Item          => (Last_Downloaded => Last_Downloaded, Songs => Song_Lists.Null_List),
            Item_Iterator => I);
      end if;
      Song_Lists.Insert_Tail (Time_Lists.Current (I).Songs, Item);
   end Insert;

   procedure Least_Recent_Songs
     (Db             : in     SAL.Config_Files.Configuration_Type;
      Category       : in     String;
      Songs          :    out Song_Lists.List_Type;
      Song_Count     : in     Integer;
      New_Song_Count : in     Integer;
      Seed           : in     Integer                             := 0)
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
      --  Then build a list of Min_Randomize_Count from least recent
      --  sections
      --
      --  Randomize list, return Song_Count songs from it.

      use type SAL.Time_Conversions.Time_Type;

      Min_Randomize_Count : constant Integer := 2 * Song_Count;
      Time_List           : Time_Lists.List_Type;

      procedure Finish
      is begin
         Randomize (Songs, Seed);
         Song_Lists.Truncate (Songs, Song_Count);
         Play_Before (Db, Songs);
      end Finish;

      procedure Add_All (Time_List_I : in Time_Lists.Iterator_Type)
      is
         use Song_Lists;
         Time_List_Element : Time_List_Element_Type renames Time_Lists.Current (Time_List_I).all;
         Source : Song_Lists.List_Type renames Time_List_Element.Songs;
      begin
         if Verbosity > 0 then
            Ada.Text_IO.Put_Line
              ("adding songs from " &
                 SAL.Time_Conversions.To_Extended_ASIST_String (Time_List_Element.Last_Downloaded));
         end if;

         Splice_After
           (Source => Source,
            First  => First (Source),
            Last   => Last (Source),
            Dest   => Songs,
            After  => Last (Songs));
      end Add_All;

      Time_List_I : Time_Lists.Iterator_Type;

      use Song_Lists;
      use Time_Lists;
   begin
      declare
         use SAL.Config_Files;
         All_Songs : SAL.Config_Files.Iterator_Type := First (Db, Songs_Key);
      begin
         loop
            --  Note that we can't exit on finding Count songs; there
            --  may be more later that were downloaded least recently.
            --  Db is not sorted.
            exit when Is_Null (All_Songs);

            if Category = Read (Db, All_Songs, Category_Key, Default => "vocal", Missing_Key => Ignore) then
               Insert (Db, Time_List, All_Songs);
            end if;

            Next (All_Songs);
         end loop;
      end;

      Time_List_I := Time_Lists.First (Time_List);

      if Current (Time_List_I).Last_Downloaded = 0.0 then
         if Count (Time_List) = 1 then
            --  New db; all songs have zero Last_Downloaded
            Ada.Text_IO.Put_Line ("new db; all new songs");
            Songs := Head (Time_List).Songs;
            Finish;
            return;

         elsif Count (Current (Time_List_I).Songs) > New_Song_Count then
            --  Only include a few new songs
            if Verbosity > 0 then
               Ada.Text_IO.Put_Line ("adding " & Integer'Image (New_Song_Count) & " new songs");
            end if;

            declare
               Source : Song_Lists.List_Type renames Current (Time_List_I).Songs;
               Last   : Song_Lists.Iterator_Type := First (Source);
            begin
               for I in 2 .. New_Song_Count loop
                  Next (Last);
               end loop;

               Splice_After (Source, First (Source), Last, Songs, First (Songs));
            end;
            Next (Time_List_I);
         else
            --  There are only a few new songs; include them all
            if Verbosity > 0 then
               Ada.Text_IO.Put_Line ("adding " & Integer'Image (Count (Current (Time_List_I).Songs)) & " new songs");
            end if;
            Songs := Current (Time_List_I).Songs;
            Next (Time_List_I);
         end if;
      end if;

      if Count (Current (Time_List_I).Songs) >= Min_Randomize_Count - Count (Songs) then
         Add_All (Time_List_I);
         Finish;
         return;
      else
         loop
            exit when Count (Songs) >= Min_Randomize_Count or
              Time_Lists.Is_Null (Time_List_I);

            Add_All (Time_List_I);

            Time_Lists.Next (Time_List_I);
         end loop;

         Finish;
         return;
      end if;

   end Least_Recent_Songs;

   procedure Edit_Playlist
     (Playlist_File_Name : in String;
      Last_File_Name     : in String)
   is
      use Ada.Directories;
      use Ada.Text_IO;

      Output_File_Name : constant String := Playlist_File_Name & ".tmp";

      Input_File  : File_Type;
      Output_File : File_Type;
   begin
      if Verbosity > 1 then
         Put_Line ("editing playlist " & Playlist_File_Name);
      end if;

      begin
         Open (Input_File, In_File, Last_File_Name);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line ("last file " & Last_File_Name & " cannot be opened");
         raise;
      end;

      --  special case; empty last file; nothing to do
      if End_Of_Line (Input_File) then
         Close (Input_File);
         Delete_File (Last_File_Name);
         return;
      end if;

      begin
         Create (Output_File, Out_File, Output_File_Name);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line ("tmp file " & Output_File_Name & " cannot be opened");
         raise;
      end;

      declare
         Last_Played : constant String := Get_Line (Input_File);
         Found       : Boolean         := False;
      begin
         Close (Input_File);

         begin
            Open (Input_File, In_File, Playlist_File_Name);
         exception
         when Ada.IO_Exceptions.Name_Error =>
            Put_Line ("playlist file " & Playlist_File_Name & " cannot be opened");
            raise;
         end;

         loop
            exit when End_Of_File (Input_File);

            declare
               Line : constant String := Get_Line (Input_File);
            begin
               if Found then
                  Put_Line (Output_File, Line);
               else
                  Found := Line = Last_Played;
               end if;
            end;
         end loop;
         Close (Input_File);
         Close (Output_File);
      end;

      Delete_File (Playlist_File_Name);
      Rename (Output_File_Name, Playlist_File_Name);
      Delete_File (Last_File_Name);

   end Edit_Playlist;

   procedure Read_Playlist
     (File_Name  : in     String;
      Files      :    out String_Lists.List)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      if Verbosity > 1 then
         Put_Line ("reading playlist " & File_Name);
      end if;

      begin
         Open (File, In_File, File_Name);
      exception
      when Name_Error =>
         Put_Line ("playlist file " & File_Name & " cannot be opened");
         raise;
      end;

      --  special case; empty file
      if End_Of_File (File) then
         Close (File);

         Files := String_Lists.Empty_List;

         return;
      end if;

      loop -- exit on End_Error
         declare
            use Ada.Directories;
            Name : constant String := Get_Line (File);
         begin
            String_Lists.Append (Files, Ada.Characters.Handling.To_Lower (Name));
         end;
      end loop;
   exception
   when End_Error =>
      Close (File);
   end Read_Playlist;

   procedure Play_Before
     (Db    : in     SAL.Config_Files.Configuration_Type;
      Songs : in out Song_Lists.List_Type)
   is
      type Item_Type is record
         First_Song_Songs  : Song_Lists.Iterator_Type;
         First_Song_Db     : SAL.Config_Files.Iterator_Type;
         Second_Song_Id    : Integer;
         Second_Song_Songs : Song_Lists.Iterator_Type;
      end record;

      type Item_Access_Type is access Item_Type;

      package Item_Lists_Aux is new SAL.Aux.Indefinite_Private_Items (Item_Type, Item_Access_Type);

      package Item_Lists is new SAL.Poly.Lists.Double
        (Item_Type         => Item_Type,
         Item_Node_Type    => Item_Access_Type,
         To_Item_Node      => Item_Lists_Aux.To_Item_Node,
         Free_Item         => Item_Lists_Aux.Free_Item,
         Copy              => Item_Lists_Aux.Copy_Item_Node,
         Node_Storage_Pool => SAL.Storage_Pools.Integer_Access_Type'Storage_Pool);

      Have_Play_Before : Item_Lists.List_Type;
      Item_I           : Item_Lists.Iterator_Type;

      use Item_Lists;
      use Song_Lists;
      use SAL.Config_Files;
      use SAL.Config_Files.Integer;

      Songs_I : Song_Lists.Iterator_Type := First (Songs);

   begin
      Fill_Have_Play_Before :
      loop
         exit Fill_Have_Play_Before when Is_Done (Songs_I);

         if Is_Present (Db, Current (Songs_I), "Play_Before") then
            Append
              (Have_Play_Before,
               (First_Song_Songs  => Songs_I,
                First_Song_Db     => Current (Songs_I),
                Second_Song_Id    => Read (Db, Current (Songs_I), "Play_Before"),
                Second_Song_Songs => Song_Lists.Null_Iterator));
         end if;

         Next (Songs_I);
      end loop Fill_Have_Play_Before;

      Item_I := First (Have_Play_Before);
      Find_Second_Song :
      loop
         exit Find_Second_Song when Is_Done (Item_I);

         declare
            Item : Item_Type renames Current (Item_I).all;
         begin
            Songs_I := First (Songs);
            Find_Song :
            loop
               exit Find_Song when Is_Done (Songs_I);
               if Item.Second_Song_Id = Integer'Value (Current (Current (Songs_I))) then
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
         exit Place_Second_Song when Is_Done (Item_I);

         declare
            Item : Item_Type renames Current (Item_I).all;
         begin
            if Song_Lists.Is_Null (Item.Second_Song_Songs) then
               --  not in Songs yet
               Insert_After (Songs, Item.First_Song_Songs, Find (Db, "Songs", Integer'Image (Item.Second_Song_Id)));
            else
               Splice_After
                 (Source => Songs,
                  First  => Item.Second_Song_Songs,
                  Last   => Item.Second_Song_Songs,
                  Dest   => Songs,
                  After  => Item.First_Song_Songs);
            end if;
         end;

         Next (Item_I);
      end loop Place_Second_Song;

   end Play_Before;

end SMM;
