--  Abstract :
--
--  download files to a music player
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
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

with Ada.Real_Time;
with Ada.Directories;
with Ada.Text_IO;
with SAL.Aux.Definite_Private_Items;
with SAL.Aux.Indefinite_Private_Items;
with SAL.Config_Files;
with SAL.Gen.Alg.Count;
with SAL.Gen.Alg.Find_Linear.Sorted;
with SAL.Poly.Lists.Double.Gen_Randomize;
with SAL.Storage_Pools;
with SAL.Time_Conversions.Config;
procedure SMM.Download
  (Db          : in out SAL.Config_Files.Configuration_Type;
   Category    : in String;
   Destination : in String)
is
   package Song_Lists_Aux is new SAL.Aux.Definite_Private_Items (SAL.Config_Files.Iterator_Type);

   package Song_Lists is new SAL.Poly.Lists.Double
     (Item_Type         => SAL.Config_Files.Iterator_Type,
      Item_Node_Type    => SAL.Config_Files.Iterator_Type,
      To_Item_Node      => Song_Lists_Aux.To_Item_Node,
      Free_Item         => Song_Lists_Aux.Free_Item,
      Copy              => Song_Lists_Aux.Copy_Item_Node,
      Node_Storage_Pool => SAL.Storage_Pools.Integer_Access_Type'Storage_Pool);

   procedure Randomize is new Song_Lists.Gen_Randomize;

   package Song_Lists_Algorithms is new SAL.Gen.Alg
     (Item_Node_Type => SAL.Config_Files.Iterator_Type,
      Container_Type => Song_Lists.List_Type,
      Iterator_Type  => Song_Lists.Iterator_Type,
      Current        => Song_Lists.Current,
      First          => Song_Lists.First,
      Last           => Song_Lists.Last,
      None           => Song_Lists.None,
      Is_Null        => Song_Lists.Is_Null,
      Next_Procedure => Song_Lists.Next,
      Next_Function  => Song_Lists.Next);

   function Count is new Song_Lists_Algorithms.Count;

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
     (List : in out Time_Lists.List_Type;
      Item : in SAL.Config_Files.Iterator_Type)
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

   procedure Download (Songs : in out Song_Lists.List_Type; Desired_Song_Count : in Integer)
   is
      use Song_Lists;
      I           : Iterator_Type;
      Count       : Integer         := 0;
      Source_Root : constant String := SAL.Config_Files.Read (Db, Root_Key);

      Download_Time : constant String := SAL.Time_Conversions.Time_Type'Image
        (SAL.Time_Conversions.To_TAI_Time (Ada.Real_Time.Clock));
   begin
      Randomize (Songs);
      I := First (Songs);
      loop
         exit when Count >= Desired_Song_Count;
         declare
            Source : constant String := Source_Root & SAL.Config_Files.Read (Db, Current (I), File_Key);
            Target : constant String := Destination & Ada.Directories.Simple_Name (Source);
         begin
            if Verbosity > 0 then
               Ada.Text_IO.Put_Line ("downloading " & Source);
               Ada.Text_IO.Put_Line ("to          " & Target);
            else
               if Count mod 10 = 0 then
                  Ada.Text_IO.New_Line;
               end if;
               Ada.Text_IO.Put (".");
            end if;

            Ada.Directories.Copy_File
              (Source_Name => Source,
               Target_Name => Target);

            SAL.Config_Files.Write (Db, Current (I), Last_Downloaded_Key, Download_Time);

            Next (I);
            Count := Count + 1;
         end;
      end loop;


   end Download;

   Desired_Song_Count : constant := 30;

   Time_List : Time_Lists.List_Type;
begin
   --  When the database is new, we want to select random sets of songs.
   --
   --  When all songs have a non-zero Last_Downloaded, we want the
   --  least recently downloaded, but we still want to mix them up so
   --  the play order is not constant each time thru the database.
   --
   --  When a new album is added, we want it to be mixed in gradually,
   --  not all in one playlist.
   --
   --  First read all songs with matching category into Time_List.
   --
   --  Then build a list of twice desired_file_count from least recent sections
   --
   --  Randomize list, download desired_file_count songs from it.

   declare
      use SAL.Config_Files;
      Songs : Iterator_Type := First (Db, Songs_Key);
   begin
      loop
         exit when Is_Null (Songs);

         if Category = Read (Db, Songs, Category_Key, Default => "vocal", Missing_Key => Ignore) then
            Insert (Time_List, Songs);
         end if;

         Next (Songs);
      end loop;
   end;

   if Count (Time_Lists.Head (Time_List).Songs) >= 2 * Desired_Song_Count then
      --  Avoid copying a large list
      Download (Time_Lists.Head (Time_List).Songs, Desired_Song_Count);
   else
      declare
         use Song_Lists;
         I     : Time_Lists.Iterator_Type := Time_Lists.First (Time_List);
         Songs : List_Type;
      begin
         loop
            exit when Count (Songs) >= 2 * Desired_Song_Count;

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
         Download (Songs, Desired_Song_Count);
      end;
   end if;

end SMM.Download;
