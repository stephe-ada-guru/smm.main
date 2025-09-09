--  Abstract :
--
--  Compare lists of songs between DB and Spotify
--
--  Copyright (C) 2025 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.JSON;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
with SMM.Database;
with Spotify;
procedure SMM.Compare_Playlist.Spotify
  (DB              : in out SMM.Database.Database;
   Category        : in     String;
   Spotify_Missing : in     String)
--  Category is the DB string identifying the playlist.
--
--  Spotify_Missing is the name of a file containing JSON-encoded list
--  of songs either missing or on a different album or with other
--  differences in the Spotify playlist.
is
   --  From https://developer.spotify.com/dashboard/5c012586b1214e33b7308648efb228e1/Settings
   Client_Id                : constant String := "5c012586b1214e33b7308648efb228e1";
   Client_Secret            : constant String := "2a4f43acb73443b59dee9aabbbee9ab7";

   --  From https://open.spotify.com/playlist/7nfC9g7RtFQUWDGdsq1GYj
   Stephes_Best_Playlist_ID : constant String := "7nfC9g7RtFQUWDGdsq1GYj";

   --  From https://open.spotify.com/playlist/36qJfLXS0A9kCio5MESskP
   Kate_Protest_Playlist_ID : constant String := "36qJfLXS0A9kCio5MESskP";

   --  From https://open.spotify.com/playlist/1rk2PNFo8BlRnq6YmLNjEc
   Parenthood_Playlist_ID : constant String := "1rk2PNFo8BlRnq6YmLNjEc";

   Spotify_Playlist_ID : constant String :=
     (if Category = "best"
      then Stephes_Best_Playlist_ID
      elsif Category = "protest"
      then Kate_Protest_Playlist_ID
      elsif Category = "parenthood"
      then Parenthood_Playlist_ID
      else raise SAL.Parameter_Error with "expecting 'best', 'protest', 'parenthood'");

   Spotify_Session : Standard.Spotify.Session;

   function Image (Spotify_Session : in Standard.Spotify.Session; Item : in Standard.Spotify.Cursor) return String
   is begin
      return Spotify_Session.Album_Artist (Item) & ", " & Spotify_Session.Album (Item) & ", " &
        Spotify_Session.Title (Item);
   end Image;

   type Missing_Data is record
      DB_Name      : Song_Name;
      Spotify_Name : Song_Name; -- Null_Song_Name if missing
   end record;

   function Missing_Data_Key (Item : in Missing_Data) return Song_Name
   is (Item.Spotify_Name);

   package Missing_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Missing_Data,
      Key_Type     => Song_Name,
      Key          => Missing_Data_Key,
      Key_Compare  => Song_Name_Compare);

   Rename_Tree  : Missing_Trees.Tree;   --  Only items with non-null Spotify_Name
   Missing_Tree : Song_Name_Trees.Tree; --  Only items with null Spotify_Name

   procedure Read_Missing
   --  Read Spotify_Missing, store in Rename_Tree or Missing_Tree.
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.JSON;
      File      : File_Type;
      Text      : Unbounded_String;
      Full_Data : JSON_Value;
      Data      : JSON_Array;
      I         : Positive;
   begin
      Open (File, In_File, Spotify_Missing);
      loop
         exit when End_Of_File (File);
         declare
            Line : constant String := Get_Line (File);
         begin
            if Line'Last >= 2 and then Line (1 .. 2) = "//" then
               --  skip comment line
               null;
            else
               Text := @ & Line;
            end if;
         end;
      end loop;

      Full_Data := Read (Text, Filename => Spotify_Missing);
      case Full_Data.Kind is
      when JSON_Object_Type =>
         --  Nothing in missing file; Data also defaults to Null
         return;

      when JSON_Array_Type =>
         Data := Get (Full_Data);

      when others =>
         raise SAL.Programmer_Error with "full_data.kind: " & JSON_Value_Type'Image (Full_Data.Kind);
      end case;

      if Verbosity > 1 then
         Put_Line ("missing JSON:");
         Put_Line (Write (Full_Data));
         New_Line (2);
      end if;

      I := Array_First (Data);
      loop
         exit when not Array_Has_Element (Data, I);
         declare
            Misc_Item         : JSON_Value renames Array_Element (Data, I);
            DB_Name_Item      : JSON_Value renames Misc_Item.Get ("db");
            Spotify_Name_Item : JSON_Value renames Misc_Item.Get ("spotify");

            --  We want to ignore case when matching names; that is done in "=" and Compare.
            DB_Name           : constant Song_Name :=
              (Album_Artist    => Get (DB_Name_Item, "album_artist"),
               Album           => Get (DB_Name_Item, "album"),
               Title           => Get (DB_Name_Item, "title"));
            Spotify_Name      : constant Song_Name :=
              (if Spotify_Name_Item.Kind = JSON_Object_Type then
                 (Album_Artist => Get (Spotify_Name_Item, "album_artist"),
                  Album        => Get (Spotify_Name_Item, "album"),
                  Title        => Get (Spotify_Name_Item, "title"))
               else Null_Song_Name);
         begin
            if Spotify_Name = Null_Song_Name then
               Missing_Tree.Insert (DB_Name);
            else
               Rename_Tree.Insert (Missing_Data'(DB_Name, Spotify_Name));
            end if;
         end;

         I := Array_Next (Data, I);
      end loop;
   exception
   when Name_Error =>
      raise Name_Error with "file '" & Spotify_Missing & "' not found";
   end Read_Missing;

   Spotify_Tree : Song_Name_Trees.Tree;

begin
   Read_Missing;

   Standard.Spotify.Start_Session (Spotify_Session, Client_Id, Client_Secret);

   declare
      Playlist_Chunk  : constant Standard.Spotify.Playlist_Item_Count := Standard.Spotify.Playlist_Item_Count'Last;

      Playlist_Offset : Natural := 0;

      Spotify_I : Standard.Spotify.Cursor := Spotify_Session.Get_Playlist
        (Spotify_Playlist_ID,
         Offset => Playlist_Offset, Count => Playlist_Chunk);

      use Standard.Spotify;

   begin
      if Verbosity > 1 then
         Put_Line ("spotify list: ");
         loop
            exit when not Spotify_Session.Has_Element (Spotify_I);
            Put_Line (Image (Spotify_Session, Spotify_I));
            Spotify_Session.Next (Spotify_I);
         end loop;
         New_Line (2);

         Spotify_I := Spotify_Session.First;
      end if;

      Read_Spotify_Tree :
      loop
         if not Spotify_Session.Has_Element (Spotify_I) then
            --  Try to get more
            Playlist_Offset := @ + To_Integer (Spotify_I) - 1;

            if Verbosity > 0 then
               Put_Line ("get more playlist" & Playlist_Offset'Image);
            end if;

            Spotify_I := Spotify_Session.Get_Playlist
              (Spotify_Playlist_ID, Offset => Playlist_Offset, Count => Playlist_Chunk);
         end if;

         exit Read_Spotify_Tree when not Spotify_Session.Has_Element (Spotify_I);

         declare
            use Missing_Trees;

            Spotify_Name : constant Song_Name  :=
              (Album_Artist => +Spotify_Session.Album_Artist (Spotify_I),
               Album        => +Spotify_Session.Album (Spotify_I),
               Title        => +Spotify_Session.Title (Spotify_I));

            Rename : constant Missing_Trees.Cursor := Rename_Tree.Find (Spotify_Name);
         begin
            Spotify_Tree.Insert
              (if Has_Element (Rename) then Element (Rename).DB_Name
               else Spotify_Name);
         end;

         Spotify_Session.Next (Spotify_I);
      end loop Read_Spotify_Tree;
   end;

   Compare_To_DB (DB, Category, Spotify_Tree, "Spotify", Missing_Tree);

end SMM.Compare_Playlist.Spotify;
