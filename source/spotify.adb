--  Abstract:
--
--  Implement Spotify web API
--
--  Design:
--
--  AWS.Client adds may headers to messages by default, and is very
--  hard to debug. So we just spawn curl, making it very easy to
--  debug.
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

with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;
package body Spotify is

   use GNATCOLL.JSON;

   Debug_Flag : constant Boolean := True; -- FIXME: false or delete

   function Curl (Args : in GNAT.OS_Lib.Argument_List) return String
   --  Spawn 'curl' with Args, return result.
   is
      use GNAT.OS_Lib;

      Output_File_Name : constant String := "curl_output";
      Success          : Boolean;
      Return_Code      : Integer;
   begin
      if Debug_Flag then
         Ada.Text_IO.Put ("-> curl ");
         for S of Args loop
            Ada.Text_IO.Put (S.all & " | ");
         end loop;
         Ada.Text_IO.New_Line;
      end if;

      Spawn ("curl", Args, Output_File_Name, Success, Return_Code);

      if not Success then
         raise Some_Error with "spawn curl failed: " & Return_Code'Image;
      end if;

      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File, In_File, Output_File_Name);
         return Result : constant String := Get_Line (File) do
            Delete (File);
         end return;
      end;

   end Curl;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   procedure Start_Session
     (Session       : in out Spotify.Session;
      Client_ID     : in     String;
      Client_Secret : in     String)
   is
      Args : GNAT.OS_Lib.String_List_Access :=
        new GNAT.OS_Lib.String_List'
          (new String'("-s"),
           new String'("-X"),
           new String'("POST"),
           new String'("https://accounts.spotify.com/api/token"),
           new String'("-H"),
           new String'("Content-Type: application/x-www-form-urlencoded"),
           new String'("-d"),
           new String'("grant_type=client_credentials&client_id=" & Client_ID & "&client_secret=" & Client_Secret));

      Text : constant String := Curl (Args.all);
   begin
      declare
         Temp  : constant JSON_Value := Read (Text);
         Token : constant JSON_Value := Get (Temp, Field => "access_token");
      begin
         GNAT.OS_Lib.Free (Args);

         Session.Credentials := Get (Token);
      end;
   exception
   when Invalid_JSON_Stream =>
      Ada.Text_IO.Put_Line ("json parse fail:");
      Ada.Text_IO.Put_Line (Text);
      raise Some_Error;
   end Start_Session;

   function Get_Playlist
     (Session     : in out Spotify.Session;
      Playlist_ID : in     String)
     return Cursor
   is
      Args : GNAT.OS_Lib.String_List_Access :=
        new GNAT.OS_Lib.String_List'
          (new String'("-s"),
           new String'("https://api.spotify.com/v1/playlists/" & Playlist_ID),
           new String'("-H"),
           new String'("Authorization: Bearer  " & (-Session.Credentials)));

      Data : constant String := Curl (Args.all);
      Temp : JSON_Value;
   begin
      declare
         use Ada.Text_IO;
         use Ada.Exceptions;
      begin
         GNAT.OS_Lib.Free (Args);
         Temp := Read (Data);
      exception
      when E : Invalid_JSON_Stream =>
         Put_Line (Standard_Error, "data => ");
         Put_Line (Data);
         raise Some_Error with Exception_Message (E);
      end;

      --  First layer is an object containing playlist metadata.
      case Kind (Temp) is
      when JSON_Array_Type =>
         Session.Playlist := Temp.Get;

         return (Index => Array_First (Session.Playlist));

      end case;
   exception
         when others =>
         raise Some_Error;
         return (Index => Positive'First);
end Get_Playlist;

   function Has_Element (Session : in Spotify.Session; Position : in out Cursor) return Boolean
   is begin
      return Array_Has_Element (Session.Playlist, Position.Index);
   end Has_Element;

   procedure Next (Session : in Spotify.Session; Position : in out Cursor)
   is begin
      Position.Index := Array_Next (Session.Playlist, Position.Index);
   end Next;

   function Album (Session : in Spotify.Session; Position : in Cursor) return String
   is
      --  Song : JSON_Value renames Get (Session.Playlist, Position.Index);
   begin
      --  Need typical structure; navigaet to Album
      raise Some_Error with "not implemented";
      return "";
   end Album;

   function Album_Artist (Session : in Spotify.Session; Position : in Cursor) return String
   is begin
      raise Some_Error with "not implemented";
      return "";
   end Album_Artist;

   function Title (Session : in Spotify.Session; Position : in Cursor) return String
   is begin
      raise Some_Error with "not implemented";
      return "";
   end Title;

end Spotify;
