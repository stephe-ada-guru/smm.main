--  Abstract :
--
--  Stephe's Music Manager Server
--
--  Copyright (C) 2016 - 2018 Stephen Leake All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with AWS.Config.Set;
with AWS.Messages;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.Server.Log;
with AWS.Status;
with AWS.URL;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Config_Files.Integer;
with SMM.Database;
with SMM.Song_Lists;
package body SMM.Server is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;
   function "&"
     (Left  : in Ada.Strings.Unbounded.Unbounded_String;
      Right : in String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded."&";
   function Length (Item : in Ada.Strings.Unbounded.Unbounded_String) return Integer
     renames Ada.Strings.Unbounded.Length;

   Source_Root : Ada.Strings.Unbounded.Unbounded_String; -- Root of music files; does not end in /
   Server_Root : Ada.Strings.Unbounded.Unbounded_String; -- Root of server html, css files; does not end in /
   DB_Filename : Ada.Strings.Unbounded.Unbounded_String;

   function Decode_Plus (Item : in String) return String
   is begin
      return Result : String := Item do
         for I in Result'Range loop
            if Result (I) = '+' then
               Result (I) := ' ';
            end if;
         end loop;
      end return;
   end Decode_Plus;

   function Decode_Plus (Param : in AWS.Parameters.List) return AWS.Parameters.List
   is
      --  We have patched AWS.URL.Decode to not decode +, since that is not
      --  appropriate outside the query.
      Result : AWS.Parameters.List;
   begin
      for I in 1 .. Param.Count loop
         Result.Add (Param.Get_Name (I), Decode_Plus (Param.Get_Value (I)));
      end loop;
      return Result;
   end Decode_Plus;

   ----------
   --  Specific request handlers

   function Handle_Download (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use Ada.Containers;
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      use AWS.URL;
      use SMM.Database;
      use SMM.Song_Lists.Song_Lists;

      Category   : constant String     := Parameter (URI, "category");
      Count      : constant Count_Type := Count_Type'Value (Parameter (URI, "count"));
      New_Count  : constant Count_Type := Count_Type'Value (Parameter (URI, "new_count"));

      Seed_Param : constant String     := Parameter (URI, "seed"); -- only used in unit tests
      Seed       : constant Integer    :=
        (if Seed_Param'Length > 0 then Integer'Value (Seed_Param) else 0);

      Over_Select_Ratio_Param : constant String := Parameter (URI, "over_select_ratio");
      Over_Select_Ratio       : constant Float  :=
        (if Over_Select_Ratio_Param'Length > 0 then Float'Value (Over_Select_Ratio_Param) else 2.0);

      DB       : SMM.Database.Database;
      Songs    : List;
      Response : Unbounded_String;
   begin
      DB.Open (-DB_Filename);

      SMM.Song_Lists.Least_Recent_Songs
        (DB, Category, Songs,
         Song_Count        => Count,
         New_Song_Count    => New_Count,
         Over_Select_Ratio => Over_Select_Ratio,
         Seed              => Seed);

      for I of Songs loop
         Response := Response & Normalize (DB.Find_ID (I).File_Name) & ASCII.CR & ASCII.LF;
      end loop;

      return AWS.Response.Build ("text/plain", Response);
   exception
   when E : others =>
      return AWS.Response.Acknowledge
        (Status_Code  => AWS.Messages.S500,
         Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_Download;

   function Handle_Meta (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use AWS.URL;

      Source_Dir : constant String := -Source_Root & Path (URI);
      Response   : Unbounded_String;

      procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
      is begin
         Response := Response &
           Relative_Name (-Source_Root, Normalize (Full_Name (Dir_Ent))) & ASCII.CR & ASCII.LF;
      end Copy_Aux;
   begin
      Search
        (Directory => Source_Dir,
         Pattern   => "AlbumArt*.jpg",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      Search
        (Directory => Source_Dir,
         Pattern   => "liner_notes.pdf",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      return AWS.Response.Build ("text/plain", Response);
   exception
   when Ada.IO_Exceptions.Name_Error =>
      --  GNAT runtime sets message to "(unknown directory "")"; no file name!
      raise Ada.IO_Exceptions.Name_Error with "unknown directory '" & Path (URI) & "'";

   end Handle_Meta;

   function Handle_File (URI : in AWS.URL.Object; Name_In_Param : in Boolean) return AWS.Response.Data
   is
      use Ada.Directories;
      use Ada.Exceptions;
      use AWS.URL;
      use SMM.Database;

      Param : constant AWS.Parameters.List := AWS.URL.Parameters (URI);

      Filename : constant String := -Source_Root &
        (if Name_In_Param
         then Param.Get ("name")
         else Path (URI) & File (URI));

      Ext       : constant String := Extension (Filename);
      Mime_Type : constant String :=
        --  MIME types from https://www.iana.org/assignments/media-types/media-types.xhtml
        --  also GNAT/share/examples/aws/web_elements/mime.types
        (if    Ext = "jpg" then "image/jpeg"
         elsif Ext = "mp3" then "audio/mpeg"
         elsif Ext = "pdf" then "application/pdf"
         else "");

      DB              : SMM.Database.Database;
      Result          : AWS.Response.Data;
      I               : Cursor;
      Prev_Downloaded : Time_String;

   begin
      if Mime_Type'Length = 0 then
         return AWS.Response.Acknowledge
           (Status_Code  => AWS.Messages.S500,
            Message_Body => "<p>file extension '" & Ext & "' not supported.");
      end if;

      if Exists (Filename) then
         if Ext = "mp3" then
            DB.Open (-DB_Filename);

            --  Find does not want leading / on filename
            I := Find_File_Name (DB, Filename (Length (Source_Root) + 2 .. Filename'Last));

            if not I.Has_Element then
               return AWS.Response.Acknowledge
                 (Status_Code  => AWS.Messages.S500,
                  Message_Body => "file not found");
            end if;

            I.Write_Last_Downloaded (DB, SMM.Database.UTC_Image (Ada.Calendar.Clock));

            Prev_Downloaded := I.Prev_Downloaded;

            Finalize (DB);
         end if;

         Result := AWS.Response.File (Mime_Type, Filename);
         AWS.Response.Set.Add_Header (Result, "X-prev_downloaded", Prev_Downloaded);
         return Result;

      else
         return AWS.Response.Acknowledge
           (Status_Code  => AWS.Messages.S404,
            Message_Body => "<p>file '" & Filename & "' not found.");
      end if;
   exception
   when Name_Error =>
      --  Exists raises Name_Error if the directory does not exist
      return AWS.Response.Acknowledge
        (Status_Code  => AWS.Messages.S404,
         Message_Body => "<p>file '" & Filename & "' not found.");
   when E : others =>
      return AWS.Response.Acknowledge
        (Status_Code  => AWS.Messages.S500,
         Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_File;

   function Handle_Put_Notes (Request : in AWS.Status.Data; URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use Ada.Directories;
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use AWS.URL;

      Pathname : constant String := -Source_Root & Path (URI);
      Filename : constant String := Pathname & File (URI);
      Data     : constant String := -AWS.Status.Binary_Data (Request);
      First    : Integer         := Data'First;
      Last     : Integer;
      File     : File_Type;
   begin
      if not Exists (Pathname) then
         Create_Directory (Pathname);
      end if;

      if Exists (Filename) then
         Open (File, Append_File, Filename);
      else
         Create (File, Out_File, Filename);
      end if;
      loop
         Last := Index (Source => Data (First .. Data'Last), Pattern => ASCII.CR & ASCII.LF);
         exit when Last < Data'First;
         Put_Line (File, Data (First .. Last - 1));
         First := Last + 2;
      end loop;
      Close (File);

      return AWS.Response.Acknowledge (Status_Code  => AWS.Messages.S200);
   exception
   when E : others =>
      return AWS.Response.Acknowledge
        (Status_Code  => AWS.Messages.S500,
         Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_Put_Notes;

   function Handle_Search (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use Ada.Strings.Unbounded;
      use SMM.Database;

      Param : constant AWS.Parameters.List := Decode_Plus (AWS.URL.Parameters (URI));

      DB           : SMM.Database.Database;
      Search_Param : SMM.Database.Field_Values;

      function Search_Result (I : in Cursor) return String
      is
         --  FIXME: replace with template
      begin
         return
           "<div>" &
           "<a href=""file?name=/" & I.File_Name & """>" & I.Artist & " | " & I.Album & " | " & I.Title & "</a>" &
           "</div>";
      end Search_Result;

   begin
      if Param.Is_Empty then
         --  Return search page with no results.
         return AWS.Response.File ("text/html", -Server_Root & "/search_initial.html");

      else
         --  From search page, query looks like
         --  '?title=michael&artist=joni&album=miles'; all three fields always
         --  present, may be empty.
         --
         --  Support links from result pages etc; allow fields to be not present.

         DB.Open (-DB_Filename);

         for I in Fields loop
            declare
               Value : constant String := Param.Get (-Field_Image (I)); -- empty string if not present
            begin
               if Value'Length > 0 then
                  Search_Param (I) := +Value;
               end if;
            end;
         end loop;

         declare
            Response : Unbounded_String := +"<!DOCTYPE html>" & ASCII.LF & "<html><head></head><body>";
            I        : Cursor           := Find_Like (DB, Search_Param);
         begin
            if not I.Has_Element then
               return AWS.Response.Acknowledge
                 (Status_Code  => AWS.Messages.S404,
                  Message_Body => "'" & Image (Search_Param) & "' not found");
            end if;

            loop
               exit when not I.Has_Element;
               Response := Response & Search_Result (I) & ASCII.LF;

               I.Next;
            end loop;

            Response := Response & "</body></html>";
            return AWS.Response.Build ("text/html", Response);
         end;
      end if;
   end Handle_Search;

   ----------
   --  Top level

   function Handle_Request (Request : in AWS.Status.Data) return AWS.Response.Data
   is
      use Ada.Exceptions;
      use AWS.Response;
      use AWS.Status;
      use AWS.URL;
      URI : constant AWS.URL.Object := AWS.Status.URI (Request);
   begin
      case Method (Request) is
      when GET =>
         declare
            URI_File : constant String := File (URI);
         begin
            if URI_File = "download" then
               return Handle_Download (URI);

            elsif URI_File = "favicon.ico" then
               --  FIXME: this does not work; laptop chrome developer console reports
               --  "server responded with 404".
               return AWS.Response.File ("image/png", -Server_Root & "icon.png");

            elsif URI_File = "file" then
               return Handle_File (URI, Name_In_Param => True);

            elsif URI_File = "meta" then
               return Handle_Meta (URI);

            elsif URI_File = "search" then
               return Handle_Search (URI);

            else
               --  FIXME: delete after change android app

               --  It's a file request.
               return Handle_File (URI, Name_In_Param => False);
            end if;
         end;
      when PUT =>
         return Handle_Put_Notes (Request, URI);

      when others =>
         return Acknowledge
           (Status_Code  => AWS.Messages.S500,
            Message_Body => "unrecognized request " & Request_Method'Image (Method (Request)));
      end case;
   exception
   when E : others =>
      return Acknowledge
        (Status_Code  => AWS.Messages.S500,
         Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_Request;

   procedure Server
   is
      procedure Usage
      is
         use Ada.Text_IO;
      begin
         Put_Line ("usage: smm-server-driver <server config filename> [enable_log]");
         Put_Line ("enable_log : if present, enable web sever log");
         Put_Line ("config file:");
         Put_Line ("DB_Filename");
      end Usage;

      Config     : SAL.Config_Files.Configuration_Type;
      Ws         : AWS.Server.HTTP;
      Enable_Log : Boolean := False;

   begin
      declare
         use Ada.Command_Line;
         use SAL.Config_Files;
      begin
         case Argument_Count is
         when 1 | 2 =>
            Open (Config, Argument (1));

            Enable_Log := Argument_Count = 2;

            DB_Filename := +Read (Config, "DB_Filename", Missing_Key => Raise_Exception);

            Source_Root := +As_File
              (Ada.Directories.Full_Name (Read (Config, SMM.Root_Key, Missing_Key => Raise_Exception)));
            Server_Root := Source_Root & "/../server";

         when others =>
            Usage;
            Set_Exit_Status (Failure);
            raise SAL.Parameter_Error;
         end case;
      end;

      declare
         use Ada.Text_IO;
         use AWS.Config;
         use AWS.Config.Set;
         use SAL.Config_Files;
         use SAL.Config_Files.Integer;
         Obj : Object := Default_Config;
      begin
         Server_Name (Obj, "SMM Server");

         --  Get_Host_By_Name includes VMware IP addresses, which are not
         --  useful for sync from outside this box. But there doesn't
         --  seem to be a way to tell which ones those are from here. So
         --  we get the address to use from the config file.
         Server_Host (Obj, Read (Config, "Server_IP", Missing_Key => Raise_Exception));

         --  AWS default server port is AWS.Default.Server_Port (= 8080)
         Server_Port (Obj, Read (Config, "Server_Port", Default => 8080, Missing_Key => Ignore));

         Close (Config);

         if Enable_Log then
            declare
               use Ada.Directories;
               Log_Dir_Name : constant String := -Source_Root & "/remote_cache/";
            begin
               if not Exists (Log_Dir_Name) then
                  Create_Directory (Log_Dir_Name);
               end if;

               Log_File_Directory (Obj, Log_Dir_Name);
               Log_Filename_Prefix (Obj, "smm-server");
            end;
         else
            Put_Line ("not logging");
         end if;

         AWS.Server.Start
           (Ws,
            Callback => Handle_Request'Access,
            Config   => Obj);

         if Enable_Log then
            AWS.Server.Log.Start (Ws, Auto_Flush => True);

            Put_Line ("logging to   " & Log_File_Directory (Obj) & "smm-server.log");
         end if;
      end;

      declare
         use Ada.Text_IO;
         use AWS.Config;
         Ws_Config : constant Object := AWS.Server.Config (Ws);
      begin
         Put_Line ("listening on " & Server_Host (Ws_Config) & ":" & Integer'Image (Server_Port (Ws_Config)));
      end;

      AWS.Server.Wait;
   end Server;
end SMM.Server;
--  Local Variables:
--  ada-indent-comment-gnat: t
--  End:
