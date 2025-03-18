--  Implement Spotify web API
--
--  References:
--
--  [1] https://developer.spotify.com/documentation/web-api
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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;
package Spotify is

   Session_Expired : exception;

   Some_Error : exception;
   --  Should improve code to use more specific exception, include an
   --  error message.

   type Session is new Ada.Finalization.Limited_Controlled with private;

   procedure Start_Session
     (Session       : in out Spotify.Session;
      Client_ID     : in     String;
      Client_Secret : in     String);
   --  Session expires after one hour. For now, we assume that's long enough.

   type Cursor is private;

   function To_Integer (Item : in Cursor) return Integer;
   --  For counting the number of items retrieved from a playlist.

   subtype Playlist_Item_Count is Integer range 1 .. 100;
   --  https://developer.spotify.com/documentation/web-api/reference/get-playlists-tracks
   --  says limit is 50, but 100 works, 110 fails.

   function Get_Playlist
     (Session     : in out Spotify.Session;
      Playlist_ID : in     String;
      Offset      : in     Natural;
      Count       : in     Playlist_Item_Count)
     return Cursor;
   --  Get up to Count items starting at Offset + 1 from Playlist_ID.

   function Has_Element (Session : in Spotify.Session; Position : in Cursor) return Boolean;
   function First (Session : in Spotify.Session) return Cursor;
   procedure Next (Session : in Spotify.Session; Position : in out Cursor);

   function Album (Session : in Spotify.Session; Position : in Cursor) return String;
   function Album_Artist (Session : in Spotify.Session; Position : in Cursor) return String;
   function Title (Session : in Spotify.Session; Position : in Cursor) return String;

private

   type Session is new Ada.Finalization.Limited_Controlled with record
      Credentials : Ada.Strings.Unbounded.Unbounded_String;
      Playlist    : GNATCOLL.JSON.JSON_Array;
   end record;

   type Cursor is record
      Index : Positive; --  into Session.Playlist
   end record;
end Spotify;
