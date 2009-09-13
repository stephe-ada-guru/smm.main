--  Abstract :
--
--  see spec
--
--  Copyright (C) 2008, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Gen.Alg.Find_Linear.Sorted;
with SAL.Poly.Lists.Double.Gen_Randomize;
with SAL.Time_Conversions.Config;
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

   function Relative_Name_Sans_Extension
     (Root      : in String;
      Full_Name : in String)
      return String
   is
   begin
      if Root = Full_Name (Full_Name'First .. Full_Name'First + Root'Length - 1) then
         declare
            Temp            : constant String  := Full_Name (Full_Name'First + Root'Length .. Full_Name'Last);
            Extension_First : constant Integer := Ada.Strings.Fixed.Index
              (Source  => Temp,
               Pattern => ".",
               Going   => Ada.Strings.Backward);
         begin
            if Extension_First = 0 then
               return Temp;
            else
               return Temp (Temp'First .. Extension_First - 1);
            end if;
         end;
      else
         raise SAL.Programmer_Error with Full_Name & " not relative to root " & Root;
      end if;
   end Relative_Name_Sans_Extension;

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
      Last_Downloaded : constant SAL.Time_Conversions.Time_Type :=
        SAL.Time_Conversions.Config.Read (Db, Item, Last_Downloaded_Key);

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
     (Db         : in     SAL.Config_Files.Configuration_Type;
      Category   : in     String;
      Songs      :    out Song_Lists.List_Type;
      Song_Count : in     Integer;
      Seed       : in     Integer                             := 0)
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
      --  gradually, not all in one playlist.
      --
      --  Algorithm:
      --
      --  First read all songs with matching category into Time_List,
      --  grouped by Last_Downloaded.
      --
      --  Then build a list of twice desired_file_count from least
      --  recent sections
      --
      --  Randomize list, return Song_Count songs from it.

      Time_List : Time_Lists.List_Type;
   begin
      declare
         use SAL.Config_Files;
         All_Songs : Iterator_Type := First (Db, Songs_Key);
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

      if Count (Time_Lists.Head (Time_List).Songs) >= 2 * Song_Count then
         Songs := Time_Lists.Head (Time_List).Songs;
         Randomize (Songs, Seed);
         Song_Lists.Truncate (Songs, Song_Count);
      else
         declare
            use Song_Lists;
            I : Time_Lists.Iterator_Type := Time_Lists.First (Time_List);
         begin
            loop
               exit when Count (Songs) >= 2 * Song_Count or
                 Time_Lists.Is_Null (I);

               declare
                  Source_Songs : List_Type renames Time_Lists.Current (I).Songs;
               begin
                  Splice_After
                    (Source => Source_Songs,
                     First  => First (Source_Songs),
                     Last   => Last (Source_Songs),
                     Dest   => Songs,
                     After  => Last (Songs));
               end;

               Time_Lists.Next (I);
            end loop;

            Randomize (Songs, Seed);
            Truncate (Songs, Song_Count);

         end;
      end if;

   end Least_Recent_Songs;

   procedure Read_Playlist
     (File_Name  : in     String;
      Target_Dir : in     String;
      Files      :    out String_Lists.List)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      if Verbosity > 1 then
         Put_Line ("processing playlist " & File_Name);
      end if;

      begin
         Open (File, In_File, File_Name);
      exception
      when Name_Error =>
         Put_Line ("playlist file " & File_Name & " cannot be opened");
         raise;
      end;

      --  special case; empty file
      if End_Of_Line (File) then
         if End_Of_File (File) then
            Close (File);

            Files := String_Lists.Empty_List;

            return;
         end if;
      end if;

      loop -- exit on End_Error
         declare
            use Ada.Directories;
            Name : constant String := Get_Line (File);
         begin
            if Containing_Directory (Name) /= Target_Dir then
               raise Playlist_Error with "found " & Name & "; expecting " & Target_Dir;
            end if;

            String_Lists.Append (Files, Ada.Characters.Handling.To_Lower (Simple_Name (Name)));
         end;
      end loop;
   exception
   when End_Error =>
      Close (File);
   end Read_Playlist;

end SMM;
