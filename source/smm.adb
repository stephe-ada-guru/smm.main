--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008, 2009, 2011 - 2016 Stephen Leake.  All Rights Reserved.
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

with Ada.Calendar.Formatting;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada_Config;
with SAL.Gen_Randomize_Doubly_Linked_Lists;
package body SMM is

   function Find_Home return String
   is
      use Ada.Environment_Variables;
   begin
      if Exists ("SMM_HOME") then
         return Value ("SMM_HOME");
      elsif Exists ("HOME") then
         return Value ("HOME") & "/smm";
      elsif Exists ("APPDATA") then
         return Value ("APPDATA") & "/smm";
      else
         raise Playlist_Error with "must define either APPDATA or HOME environment variable";
      end if;
   end Find_Home;

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
   is
      Dir_Root : constant String := As_Directory (Root);
   begin
      if Dir_Root = Full_Name (Full_Name'First .. Full_Name'First + Dir_Root'Length - 1) then
         return Full_Name (Full_Name'First + Dir_Root'Length .. Full_Name'Last);
      else
         raise SAL.Programmer_Error with Full_Name & " not relative to root " & Dir_Root;
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

   function As_File (Path : in String) return String
   is
      Temp : constant String := Normalize (Path);
   begin
      if Path'Length = 0 then
         return Path;
      end if;

      if Temp (Temp'Last) = '/' then
         return Temp (Temp'First .. Temp'Last - 1);
      else
         return Temp;
      end if;
   end As_File;

   function To_String (Time : in SAL.Time_Conversions.Time_Type) return String
   is begin
      return Ada.Calendar.Formatting.Image (SAL.Time_Conversions.To_Calendar_Time (Time));
   exception
   when Constraint_Error =>
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("ERROR: calendar.formatting or time_conversions.to_calendar_time can't handle " &
           SAL.Time_Conversions.Time_Type'Image (Time));
      Ada.Text_IO.New_Line;
      return "0.0";
   end To_String;

   Last_Downloaded_Key : constant String := "Last_Downloaded";
   Prev_Downloaded_Key : constant String := "Prev_Downloaded";

   function Read_Time
     (Db  : in SAL.Config_Files.Configuration_Type;
      I   : in SAL.Config_Files.Iterator_Type;
      Key : in String)
     return SAL.Time_Conversions.Time_Type
   is
      use SAL.Config_Files;
      use SAL.Time_Conversions;
   begin
      if Is_Present (Db, I, Key)  then
         declare
            Temp : constant String := Read (Db, I, Key);
         begin
            if Temp'Length = Extended_ASIST_Time_String_Type'Last and then Temp (5) = '-' then
               --  ASIST string
               return To_TAI_Time (Temp, Absolute => True);
            else
               --  Time_type'image; old db, or unit test.
               return Time_Type'Value (Temp);
            end if;
         exception
         when others =>
            --  bad format in db file; report it, but keep going
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, "bad time format for " & Current (I) & "." & Key & ": " & Temp);
            return 0.0;
         end;
      else
         return 0.0;
      end if;
   end Read_Time;

   function Read_Last_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type
   is begin
      return Read_Time (Db, I, Last_Downloaded_Key);
   end Read_Last_Downloaded;

   function Read_Prev_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type
   is begin
      return Read_Time (Db, I, Prev_Downloaded_Key);
   end Read_Prev_Downloaded;

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

   type Time_List_Element_Type is record
      Last_Downloaded : SAL.Time_Conversions.Time_Type;
      Songs           : Song_Lists.List;
   end record;

   function Is_Equal (Left : in Time_List_Element_Type; Right : in Time_List_Element_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded = Right.Last_Downloaded;
   end Is_Equal;

   package Time_Lists is new Ada.Containers.Doubly_Linked_Lists (Time_List_Element_Type, Is_Equal);

   function Is_Less (Left : in Time_List_Element_Type; Right : in Time_List_Element_Type) return Boolean
   is
      use type SAL.Time_Conversions.Time_Type;
   begin
      return Left.Last_Downloaded < Right.Last_Downloaded;
   end Is_Less;

   package Time_Lists_Sorting is new Time_Lists.Generic_Sorting (Is_Less);

   procedure Insert
     (Db   : in     SAL.Config_Files.Configuration_Type;
      List : in out Time_Lists.List;
      Item : in     SAL.Config_Files.Iterator_Type)
   is
      use Time_Lists;
      Last_Downloaded         : constant SAL.Time_Conversions.Time_Type := Read_Last_Downloaded (Db, Item);
      Last_Downloaded_Element : constant Time_List_Element_Type         := (Last_Downloaded, Song_Lists.Empty_List);

      I : Cursor := List.Find (Last_Downloaded_Element);
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

   procedure Least_Recent_Songs
     (Db             : in     SAL.Config_Files.Configuration_Type;
      Category       : in     String;
      Songs          :    out Song_Lists.List;
      Song_Count     : in     Ada.Containers.Count_Type;
      New_Song_Count : in     Ada.Containers.Count_Type;
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

      procedure Randomize is new SAL.Gen_Randomize_Doubly_Linked_Lists (Song_Lists);

      use Ada.Containers;
      use type SAL.Time_Conversions.Time_Type;

      Min_Randomize_Count : constant Count_Type := 2 * Song_Count;
      Time_List           : Time_Lists.List;

      procedure Finish
      is begin
         Randomize (Songs, Seed);
         if Songs.Length > Song_Count then
            Song_Lists.Delete_Last (Songs, Songs.Length - Song_Count);
         end if;
         Play_Before (Db, Songs);
      end Finish;

      procedure Add_All (Time_List_I : in Time_Lists.Cursor)
      is
         use Song_Lists;
      begin
         if Verbosity > 0 then
            Ada.Text_IO.Put_Line ("adding songs from " & To_String (Time_Lists.Element (Time_List_I).Last_Downloaded));
         end if;

         Splice
           (Source => Time_List.Reference (Time_List_I).Songs,
            Target => Songs,
            Before => No_Element);
      end Add_All;

      Time_List_I : Time_Lists.Cursor;

      use Song_Lists;
      use Time_Lists;
   begin
      declare
         use SAL.Config_Files;
         All_Songs : SAL.Config_Files.Iterator_Type := First (Db, Songs_Key);
      begin
         loop
            --  Note that we can't exit on finding Count songs; there
            --  may be more later that were downloaded less recently.
            --  Db is not sorted.
            exit when Is_Null (All_Songs);

            if Category = Read (Db, All_Songs, Category_Key, Default => "vocal", Missing_Key => Ignore) then
               Insert (Db, Time_List, All_Songs);
            end if;

            Next (All_Songs);
         end loop;
      end;

      Time_List_I := Time_List.First;

      if Element (Time_List_I).Last_Downloaded = 0.0 then
         --  New songs

         if Time_List.Length = 1 then
            --  New db; all songs have zero Last_Downloaded
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
               Source : Song_Lists.List renames Element (Time_List_I).Songs;
               Last   : Song_Lists.Cursor := Source.First;
            begin
               for I in 1 .. New_Song_Count loop
                  Songs.Prepend (Element (Last));
                  Next (Last);
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
      Songs : in out Song_Lists.List)
   is
      type Item_Type is record
         First_Song_Songs  : Song_Lists.Cursor;
         First_Song_Db     : SAL.Config_Files.Iterator_Type;
         Second_Song_Id    : Integer;
         Second_Song_Songs : Song_Lists.Cursor;
      end record;

      package Item_Lists is new Ada.Containers.Doubly_Linked_Lists (Item_Type);

      Have_Play_Before : Item_Lists.List;
      Item_I           : Item_Lists.Cursor;

      use Item_Lists;
      use Song_Lists;
      use SAL.Config_Files;
      use Ada_Config;

      Songs_I : Song_Lists.Cursor := First (Songs);

   begin
      Fill_Have_Play_Before :
      loop
         exit Fill_Have_Play_Before when Songs_I = Song_Lists.No_Element;

         if Is_Present (Db, Element (Songs_I), "Play_Before") then
            declare
               Song_Id   : constant Integer := Integer'Value (Current (Element (Songs_I)));
               Before_Id : constant Integer := Read (Db, Element (Songs_I), "Play_Before");
            begin
               if Before_Id = Song_Id then
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line
                    ("db ERROR: " & Integer'Image (Song_Id) & ".Play_Before = " & Integer'Image (Before_Id));
                  Ada.Text_IO.New_Line;
               else
                  Append
                    (Have_Play_Before,
                     (First_Song_Songs  => Songs_I,
                      First_Song_Db     => Element (Songs_I),
                      Second_Song_Id    => Before_Id,
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
               if Item.Second_Song_Id = Integer'Value (Current (Element (Songs_I))) then
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
               Insert (Songs, Next (Item.First_Song_Songs), Find (Db, "Songs", Integer'Image (Item.Second_Song_Id)));
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

end SMM;
