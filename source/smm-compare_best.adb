--  Abstract :
--
--  Compare list of best songs in DB and Spotify
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
procedure SMM.Compare_Best (DB : in out SMM.Database.Database; Spotify_Missing : in String)
--  Spotify_Missing is the name of a file containing JSON-encoded list
--  of songs either missing or on a different album in Spotify.
is
   --  From https://developer.spotify.com/dashboard/5c012586b1214e33b7308648efb228e1/Settings
   Client_Id                : constant String := "5c012586b1214e33b7308648efb228e1";
   Client_Secret            : constant String := "2a4f43acb73443b59dee9aabbbee9ab7";

   --  From https://open.spotify.com/playlist/7nfC9g7RtFQUWDGdsq1GYj
   Stephes_Best_Playlist_Id : constant String := "7nfC9g7RtFQUWDGdsq1GYj";

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
        renames Ada.Strings.Unbounded.To_String;

   Spotify_Session : Spotify.Session;

   type Song_Names is record
      Album_Artist : Ada.Strings.Unbounded.Unbounded_String;
      Album        : Ada.Strings.Unbounded.Unbounded_String;
      Title        : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   function "=" (Left, Right : in Song_Names) return Boolean
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return
        Left.Album_Artist = Right.Album_Artist and
        Left.Album = Right.Album and
        Left.Title = Right.Title;
   end "=";

   Null_Song_Names : constant Song_Names := (others => Ada.Strings.Unbounded.Null_Unbounded_String);

   function Image (Item : in Song_Names) return String
   is begin
      return -Item.Album_Artist & ", " & (-Item.Album) & ", " & (-Item.Title);
   end Image;

   function Image (Item : in SMM.Database.Cursor) return String
   is begin
      return Item.Album_Artist & ", " & Item.Album & ", " & Item.Title;
   end Image;

   function Image (Spotify_Session : in Spotify.Session; Item : in Spotify.Cursor) return String
   is begin
      return Spotify_Session.Album_Artist (Item) & ", " & Spotify_Session.Album (Item) & ", " &
        Spotify_Session.Title (Item);
   end Image;

   function DB_Find (Item : in Song_Names) return SMM.Database.Song_ID
   is
      use SMM.Database;
      I : Cursor := Find_Like
        (DB,
         Param           =>
           (Album_Artist => Item.Album_Artist,
            Album        => Item.Album,
            Title        => Item.Title,
            others       => Ada.Strings.Unbounded.Null_Unbounded_String),
         Order_By        => (1 => Album_Artist));
   begin
      if not I.Has_Element then
         raise SAL.Not_Found with "'" & Image (Item) & "' not found in DB";
      end if;

      return ID : constant Song_ID := I.ID do
         I.Next;
         if I.Has_Element then
            raise SAL.Programmer_Error with "song_names not unique: '" &
              (-Item.Album_Artist) & ", " &
              (-Item.Album) & ", " &
              (-Item.Title) & "'";
         end if;
      end return;
   end DB_Find;

   type Missing_Data is record
      ID            : SMM.Database.Song_ID;
      Spotify_Names : Song_Names; -- Null_Song_Names if missing
   end record;

   function Missing_Data_Key (Item : in Missing_Data) return SMM.Database.Song_ID
   is begin
      return Item.ID;
   end Missing_Data_Key;

   function Song_ID_Compare is new SAL.Gen_Compare_Integer (SMM.Database.Song_ID);

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
      Data      := Get (Full_Data);

      if Verbosity > 1 then
         Put_Line ("missing JSON:");
         Put_Line (Write (Full_Data));
         New_Line (2);
      end if;

      I := Array_First (Data);
      loop
         exit when not Array_Has_Element (Data, I);
         declare
            Misc_Item          : JSON_Value renames Array_Element (Data, I);
            DB_Names_Item      : JSON_Value renames Misc_Item.Get ("db");
            Spotify_Names_Item : JSON_Value renames Misc_Item.Get ("spotify");
            DB_Names           : constant Song_Names :=
              (Album_Artist    => Get (DB_Names_Item, "album_artist"),
               Album           => Get (DB_Names_Item, "album"),
               Title           => Get (DB_Names_Item, "title"));
            Spotify_Names      : constant Song_Names :=
              (if Spotify_Names_Item.Kind = JSON_Object_Type then
                 (Album_Artist => Get (Spotify_Names_Item, "album_artist"),
                  Album        => Get (Spotify_Names_Item, "album"),
                  Title        => Get (Spotify_Names_Item, "title"))
               else Null_Song_Names);

            DB_I : constant SMM.Database.Song_ID := DB_Find (DB_Names);
         begin
            Missing_Tree.Insert (Element => (DB_I, Spotify_Names));
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
         DB_Artist : constant String := Left.Album_Artist;
         DB_Album  : constant String := Left.Album;
         DB_Title  : constant String := Left.Title;

         Spotify_Artist : constant String := Spotify_Session.Album_Artist (Right);
         Spotify_Album  : constant String := Spotify_Session.Album (Right);
         Spotify_Title  : constant String := Spotify_Session.Title (Right);
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
      --  We assume songs are marked Best in DB _before_ being added to
      --  Spotify list.

      Spotify_I : Spotify.Cursor := Spotify_Session.Get_Playlist (Stephes_Best_Playlist_Id);

      use SMM.Database;
      use Spotify;
      Missing_Count : Integer := 0;

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
                     Put_Line ("missing ok: " & Image (DB_I));
                  end if;
               else
                  declare
                     Spotify_Names : constant Song_Names :=
                       (+Spotify_Session.Album_Artist (Spotify_I),
                        +Spotify_Session.Album (Spotify_I),
                        +Spotify_Session.Title (Spotify_I));
                  begin
                     if Verbosity > 1 then
                        Put_Line ("checking spotify: " & Image (Spotify_Names));
                     end if;
                     if Different_Names = Spotify_Names then
                        if Verbosity > 0 then
                           Put_Line ("different ok: " & Image (DB_I));
                        end if;

                        Spotify_Session.Next (Spotify_I);
                     else
                        Put_Line ("Spotify missing: '" & Image (DB_I) & "' after '" & Image (Spotify_Names) & "'");
                        Put_Line ("marked as '" & Image (Different_Names) & "'");
                     end if;
                  end;
               end if;
            end;
         else
            Put_Line ("Spotify missing: " & Image (DB_I));
            if Spotify_Session.Has_Element (Spotify_I) then
               Put_Line ("Spotify at     : " & Image (Spotify_Session, Spotify_I));
            end if;
            Missing_Count := @ + 1;
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
            --  remaining Best items in DB_I are new
            if Verbosity > 1 then
               Put_Line ("spotify list done");
            end if;

            loop
               if Verbosity > 1 then
                  Put_Line ("checking db: " & Image (DB_I));
               end if;

               if DB_I.Category_Contains ("best") then
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

            if DB_I.Category_Contains ("best") then
               if DB_I = Spotify_I then
                  --  all ok
                  if Verbosity > 1 then
                     Put_Line ("checking spotify: " & Image (Spotify_Session, Spotify_I));
                  end if;
                  if Verbosity > 0 then
                     Put_Line ("ok: " & Image (DB_I));
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

      Put_Line ("compare DB Best to Spotify best done: missing " & Missing_Count'Image);
   end;

end SMM.Compare_Best;
