--  Abstract :
--
--  Stephe's Music Manager Server
--
--  Copyright (C) 2016 Stephen Leake All Rights Reserved.
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
with AWS.Response;
with AWS.Server.Log;
with AWS.Status;
with AWS.URL;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Config_Files;
package body SMM.Server is

   Source_Root : access String; -- does not end in /

   Db : SAL.Config_Files.Configuration_Type;

   function Handle_Request (Request : AWS.Status.Data) return AWS.Response.Data
   is
      use AWS.Response;
      use AWS.URL;
      URI : constant AWS.URL.Object := AWS.Status.URI (Request);
   begin
      if File (URI) = "download" then
         declare
            use Ada.Containers;
            use Ada.Strings.Unbounded;
            use SAL.Config_Files;
            use Song_Lists;

            Category   : constant String     := Parameter (URI, "category");
            Count      : constant Count_Type := Count_Type'Value (Parameter (URI, "count"));
            Seed_Param : constant String     := Parameter (URI, "seed"); -- only used in unit tests
            Seed       : constant Integer    :=
              (if Seed_Param'Length > 0 then Integer'Value (Seed_Param) else 0);
            Songs      : List;
            Response   : Unbounded_String;
         begin
            Least_Recent_Songs
              (Db, Category, Songs,
               Song_Count     => Count,
               New_Song_Count => Count_Type'Min (1, Count / 10),
               Seed           => Seed);

            for I of Songs loop
               Response :=  Response & Read (Db, I, File_Key) & ASCII.CR & ASCII.LF;
            end loop;
            --  FIXME: add liner_notes.pdf, AlbumArt*.jpg

            return Build ("text/plain", Response);
         end;
      elsif File (URI) = "meta" then
         declare
            use Ada.Directories;
            use Ada.Strings.Unbounded;
            Source_Dir : constant String := Source_Root.all & Path (URI);
            Response   : Unbounded_String;

            procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
            is begin
               Response := Response &
                 Relative_Name (Source_Root.all, Normalize (Full_Name (Dir_Ent))) & ASCII.CR & ASCII.LF;
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

            return Build ("text/plain", Response);
         end;
      else
         --  It's a file request.
         declare
            use Ada.Directories;
            Filename  : constant String := Source_Root.all & Path (URI) & File (URI);
            Ext       : constant String := Extension (Filename);
            Mime_Type : constant String :=
              --  MIME types from https://www.iana.org/assignments/media-types/media-types.xhtml
              --  also GNAT/share/examples/aws/web_elements/mime.types
              (if    Ext = "jpg" then "image/jpeg"
               elsif Ext = "mp3" then "audio/mpeg"
               elsif Ext = "pdf" then "application/pdf"
               else "");

         begin
            if Mime_Type'Length = 0 then
               return Acknowledge
                 (Status_Code  => AWS.Messages.S500,
                  Message_Body => "<p>file extension '" & Ext & "' not supported.");
            end if;

            if Exists (Filename) then
               if Ext = "mp3" then
                  --  Find does not want leading / on filename
                  Write_Last_Downloaded
                    (Db,
                     Find (Db, Filename (Source_Root.all'Length + 2 .. Filename'Last)),
                     SAL.Time_Conversions.To_TAI_Time (Ada.Calendar.Clock));
                  SAL.Config_Files.Flush (Db);
               end if;
               return File (Mime_Type, Filename);
            else
               return Acknowledge
                 (Status_Code  => AWS.Messages.S404,
                  Message_Body => "<p>file '" & Filename & "' not found.");
            end if;
         end;
      end if;
   exception
   when E : others =>
      declare
         use Ada.Exceptions;
      begin
         return Acknowledge
           (Status_Code  => AWS.Messages.S500,
            Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
      end;
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

            Open
              (Db,
               Read (Config, "DB_Filename", Missing_Key => Raise_Exception),
               Missing_File          => Raise_Exception,
               Duplicate_Key         => Raise_Exception,
               Read_Only             => False,
               Case_Insensitive_Keys => True);

            Source_Root := new String'(As_File (Read (Db, SMM.Root_Key, Missing_Key => Raise_Exception)));

         when others =>
            Usage;
            Set_Exit_Status (Failure);
            raise SAL.Parameter_Error;
         end case;
      end;

      declare
         use AWS.Config;
         use AWS.Config.Set;
         use SAL.Config_Files;
         Obj : Object := Default_Config;
      begin
         Server_Name (Obj, "SMM Server");

         --  Get_Host_By_Name includes VMware IP addresses, which are not
         --  useful for sync from outside this box. But there doesn't
         --  seem to be a way to tell which ones those are from here. So
         --  we get the address to use from the config file.
         Server_Host (Obj, Read (Config, "Server_IP", Missing_Key => Raise_Exception));

         AWS.Server.Start
           (Ws,
            Callback => Handle_Request'Access,
            Config   => Obj);
      end;

      if Enable_Log then
         AWS.Server.Log.Start (Ws);
      end if;

      declare
         use Ada.Text_IO;
         use AWS.Config;
         Ws_Config : constant Object := AWS.Server.Config (Ws);
      begin
         Put_Line ("listening on " & Server_Host (Ws_Config) & ":" & Integer'Image (Server_Port (Ws_Config)));
         if Enable_Log then
            Put_Line ("logging to   " & Log_File_Directory (Ws_Config) & Log_Filename_Prefix (Ws_Config));
         else
            Put_Line ("not logging");
         end if;
      end;

      AWS.Server.Wait;
   end Server;
end SMM.Server;
