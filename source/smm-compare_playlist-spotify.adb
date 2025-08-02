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

with Ada.Characters.Handling;
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

   Spotify_Session : Spotify.Session;

   function Image (Spotify_Session : in Spotify.Session; Item : in Spotify.Cursor) return String
   is begin
      return Spotify_Session.Album_Artist (Item) & ", " & Spotify_Session.Album (Item) & ", " &
        Spotify_Session.Title (Item);
   end Image;

   type Missing_Data is record
      ID            : SMM.Database.Song_ID;
      Spotify_Names : Song_Names; -- Null_Song_Names if missing
   end record;

   function Missing_Data_Key (Item : in Missing_Data) return SMM.Database.Song_ID
   is begin
      return Item.ID;
   end Missing_Data_Key;

   package Missing_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Missing_Data,
      Key_Type     => SMM.Database.Song_ID,
      Key          => Missing_Data_Key,
      Key_Compare  => Song_ID_Compare);

   Missing_Tree : Missing_Trees.Tree;

   procedure Read_Missing
   --  Read Spotify_Missing, store in Missing_Tree.
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
            use Ada.Characters.Handling;

            Misc_Item          : JSON_Value renames Array_Element (Data, I);
            DB_Names_Item      : JSON_Value renames Misc_Item.Get ("db");
            Spotify_Names_Item : JSON_Value renames Misc_Item.Get ("spotify");

            --  DB Find_Like uses SQL search, which is not case sensitive
            DB_Names           : constant Song_Names :=
              (Album_Artist    => +To_Lower (Get (DB_Names_Item, "album_artist")),
               Album           => +To_Lower (Get (DB_Names_Item, "album")),
               Title           => +To_Lower (Get (DB_Names_Item, "title")));
            Spotify_Names      : constant Song_Names :=
              (if Spotify_Names_Item.Kind = JSON_Object_Type then
                 (Album_Artist => +To_Lower (Get (Spotify_Names_Item, "album_artist")),
                  Album        => +To_Lower (Get (Spotify_Names_Item, "album")),
                  Title        => +To_Lower (Get (Spotify_Names_Item, "title")))
               else Null_Song_Names);

            DB_I : SMM.Database.Cursor := DB_Find (DB_Names);
         begin
            Multiple_DB_Match :
            loop
               if To_Lower (Image (DB_I)) = Image (DB_Names) then
                  Missing_Tree.Insert (Element => (DB_I.ID, Spotify_Names));
                  exit Multiple_DB_Match;
               else
                  --  Handle "the babysitter's here intro" vs "the babysitter's here".
                  DB_I.Next;

                  if not DB_I.Has_Element then
                     raise SAL.Initialization_Error with Image (DB_Names) & " not found in db";
                  end if;
               end if;
            end loop Multiple_DB_Match;
         end;

         I := Array_Next (Data, I);
      end loop;
   exception
   when Name_Error =>
      raise Name_Error with "file '" & Spotify_Missing & "' not found";
   end Read_Missing;

begin
   Read_Missing;

   Spotify.Start_Session (Spotify_Session, Client_Id, Client_Secret);

   declare
      function "=" (Left : in SMM.Database.Cursor; Right : in Spotify.Cursor) return Boolean
      is
         use Ada.Characters.Handling;

         DB_Artist : constant String := To_Lower (Left.Album_Artist);
         DB_Album  : constant String := To_Lower (Left.Album);
         DB_Title  : constant String := To_Lower (Left.Title);

         Spotify_Artist : constant String := To_Lower (Spotify_Session.Album_Artist (Right));
         Spotify_Album  : constant String := To_Lower (Spotify_Session.Album (Right));
         Spotify_Title  : constant String := To_Lower (Spotify_Session.Title (Right));
      begin
         return
           DB_Artist = Spotify_Artist and
           DB_Album = Spotify_Album and
           DB_Title = Spotify_Title;
      end "=";

      DB_I : SMM.Database.Cursor := DB.First;
      --  Iterates in song ID order; playlist is created in that order. We
      --  need album_artist, album, title to match Spotify playlist entry.
      --
      --  We assume songs are marked with Category in DB _before_ being
      --  added to Spotify list. FIXME: not true; added Superman to spotify best.

      Playlist_Chunk  : constant Spotify.Playlist_Item_Count := Spotify.Playlist_Item_Count'Last;
      Playlist_Offset : Natural                              := 0;

      Spotify_I : Spotify.Cursor := Spotify_Session.Get_Playlist
        (Spotify_Playlist_ID,
         Offset => Playlist_Offset, Count => Playlist_Chunk);

      use SMM.Database;
      use Spotify;
      Error_Count : Integer := 0;
      Total_Song_Count : Integer := 0;

      procedure Check_Missing
      is
         use Missing_Trees;

         Missing_J : constant Missing_Trees.Cursor := Missing_Tree.Find (DB_I.ID);
      begin
         if Has_Element (Missing_J) then
            declare
               Different_Names : Song_Names renames Element (Missing_J).Spotify_Names;
            begin
               if Different_Names = Null_Song_Names then
                  if Verbosity > 0 then
                     Put_Line (Total_Song_Count'Image & " missing ok: " & Image (DB_I));
                  end if;
               else
                  declare
                     use Ada.Characters.Handling;

                     Spotify_Names : constant Song_Names :=
                       (+To_Lower (Spotify_Session.Album_Artist (Spotify_I)),
                        +To_Lower (Spotify_Session.Album (Spotify_I)),
                        +To_Lower (Spotify_Session.Title (Spotify_I)));
                  begin
                     if Verbosity > 1 then
                        Put_Line ("checking spotify: " & Image (Spotify_Names));
                     end if;
                     if Different_Names = Spotify_Names then
                        if Verbosity > 0 then
                           Put_Line (Total_Song_Count'Image & " different ok: " & Image (DB_I));
                        end if;

                        Spotify_Session.Next (Spotify_I);
                     else
                        Put_Line ("Spotify missing: '" & Image (DB_I) & "'");
                        Put_Line ("after            '" & Image (Spotify_Names) & "'");
                        Put_Line ("marked as        '" & Image (Different_Names) & "'");
                        Error_Count := @ + 1;
                     end if;
                  end;
               end if;
            exception
            when Constraint_Error =>
               raise Some_Error with Image (DB_I) & " in missing.json but not in Spotify?";
            end;
         else
            Put_Line ("Spotify missing: " & Image (DB_I));
            if Error_Count = 0 and then Spotify_Session.Has_Element (Spotify_I) then
               --  Previous errors make this message meaningless.
               Put_Line ("Spotify at     : " & Image (Spotify_Session, Spotify_I));
            end if;
            Error_Count := @ + 1;
         end if;
      end Check_Missing;

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

      Main :
      loop
         exit Main when not Has_Element (DB_I);

         if not Spotify_Session.Has_Element (Spotify_I) then
            --  Try to get more
            Playlist_Offset := @ + To_Integer (Spotify_I) - 1;

            if Verbosity > 0 then
               Put_Line ("get more playlist" & Playlist_Offset'Image);
            end if;

            Spotify_I := Spotify_Session.Get_Playlist
              (Spotify_Playlist_ID, Offset => Playlist_Offset, Count => Playlist_Chunk);
         end if;

         if not Spotify_Session.Has_Element (Spotify_I) then
            --  Past end of Spotify playlist; remaining category items in DB_I are new
            if Verbosity > 1 then
               Put_Line ("spotify list done");
            end if;

            loop
               if Verbosity > 1 then
                  Put_Line ("checking db: " & Image (DB_I));
               end if;

               if DB_I.Category_Contains (Category) then
                  Total_Song_Count := @ + 1;
                  Check_Missing;
               end if;
               DB_I.Next;
               exit when not Has_Element (DB_I);
            end loop;
            exit Main;
         else
            if Verbosity > 1 then
               Put_Line ("checking db: " & Image (DB_I));
            end if;

            if DB_I.Category_Contains (Category) then
               Total_Song_Count := @ + 1;
               if DB_I = Spotify_I then
                  --  all ok
                  if Verbosity > 1 then
                     Put_Line ("checking spotify: " & Image (Spotify_Session, Spotify_I));
                  end if;
                  if Verbosity > 0 then
                     Put_Line (Total_Song_Count'Image & " ok: " & Image (DB_I));
                  end if;
                  DB_I.Next;
                  Spotify_Session.Next (Spotify_I);
               else
                  Check_Missing;
                  DB_I.Next;
               end if;
            else
               DB_I.Next;
            end if;
         end if;
      end loop Main;

      Put_Line
        ("compare DB Best to Spotify best done: songs/errors " & Total_Song_Count'Image & " /" & Error_Count'Image);
   end;

end SMM.Compare_Playlist;
