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
with SMM.Database;
with Spotify;
procedure SMM.Compare_Best (DB : in out SMM.Database.Database)
is
   --  From https://developer.spotify.com/dashboard/5c012586b1214e33b7308648efb228e1/Settings
   Client_Id                : constant String := "5c012586b1214e33b7308648efb228e1";
   Client_Secret            : constant String := "2a4f43acb73443b59dee9aabbbee9ab7";

   --  From https://open.spotify.com/playlist/7nfC9g7RtFQUWDGdsq1GYj
   Stephes_Best_Playlist_Id : constant String := "7nfC9g7RtFQUWDGdsq1GYj";

   Spotify_Session : Spotify.Session;
begin
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
      --  need artist, album, title to match Spotify playlist entry.
      --
      --  We assume songs are marked Best in DB _before_ being added to
      --  Spotify list.

      Spotify_I : Spotify.Cursor := Spotify_Session.Get_Playlist (Stephes_Best_Playlist_Id);

      use SMM.Database;
      use Spotify;
   begin
      Main :
      loop
         exit Main when not Has_Element (DB_I);

         if not Spotify_Session.Has_Element (Spotify_I) then
            --  remaining items in DB_I are new
            Put_Line ("Spotify missing songs:");
            loop
               Put_Line (DB_I.Artist & ", " & DB_I.Album & ", " & DB_I.Title);
               DB_I.Next;
               exit when not Has_Element (DB_I);
            end loop;
            exit Main;
         else
            if not (DB_I = Spotify_I) then
               Put_Line ("Spotify missing song:" & DB_I.Artist & ", " & DB_I.Album & ", " & DB_I.Title);
               DB_I.Next;
            else
               DB_I.Next;
               Spotify_Session.Next (Spotify_I);
            end if;
         end if;

         Put_Line ("compare DB Best to Spotify best done");
      end loop Main;
   end;

end SMM.Compare_Best;
