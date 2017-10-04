--  Abstract :
--
--  Stephe's Music Manager Server
--
--  Copyright (C) 2016, 2017 Stephen Leake All Rights Reserved.
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
with SAL.Config_Files;
package body SMM.Server is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;
   function Length (Item : in Ada.Strings.Unbounded.Unbounded_String) return Integer
     renames Ada.Strings.Unbounded.Length;

   Source_Root : Ada.Strings.Unbounded.Unbounded_String; -- does not end in /
   Db_Filename : Ada.Strings.Unbounded.Unbounded_String;

   function Handle_Request (Request : AWS.Status.Data) return AWS.Response.Data
   is
      use Ada.Exceptions;
      use AWS.Response;
      use AWS.Status;
      use AWS.URL;
      URI : constant AWS.URL.Object := AWS.Status.URI (Request);
   begin
      case Method (Request) is
      when GET =>
         if File (URI) = "download" then
            declare
               use Ada.Containers;
               use Ada.Strings.Unbounded;
               use SAL.Config_Files;
               use Song_Lists;

               Category   : constant String     := Parameter (URI, "category");
               Count      : constant Count_Type := Count_Type'Value (Parameter (URI, "count"));
               New_Count  : constant Count_Type := Count_Type'Value (Parameter (URI, "new_count"));

               Seed_Param : constant String     := Parameter (URI, "seed"); -- only used in unit tests
               Seed       : constant Integer    :=
                 (if Seed_Param'Length > 0 then Integer'Value (Seed_Param) else 0);

               Over_Select_Ratio_Param : constant String := Parameter (URI, "over_select_ration");
               Over_Select_Ratio       : constant Float  :=
                 (if Over_Select_Ratio_Param'Length > 0 then Float'Value (Over_Select_Ratio_Param) else 2.0);

               Db       : SAL.Config_Files.Configuration_Type;
               Songs    : List;
               Response : Unbounded_String;
            begin
               Open
                 (Db,
                  -Db_Filename,
                  Missing_File          => Raise_Exception,
                  Duplicate_Key         => Raise_Exception,
                  Read_Only             => False,
                  Case_Insensitive_Keys => True);

               Least_Recent_Songs
                 (Db, Category, Songs,
                  Song_Count        => Count,
                  New_Song_Count    => New_Count,
                  Over_Select_Ratio => Over_Select_Ratio,
                  Seed              => Seed);

               for I of Songs loop
                  Response :=  Response & Normalize (Read (Db, I, File_Key)) & ASCII.CR & ASCII.LF;
               end loop;

               Close (Db);

               return Build ("text/plain", Response);
            exception
            when E : others =>
               if Is_Open (Db) then
                  Close (Db);
               end if;
               return Acknowledge
                 (Status_Code  => AWS.Messages.S500,
                  Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
            end;

         elsif File (URI) = "meta" then
            declare
               use Ada.Directories;
               use Ada.Strings.Unbounded;
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

               return Build ("text/plain", Response);
            exception
            when Ada.IO_Exceptions.Name_Error =>
               --  GNAT runtime sets message to "(unknown directory "")"; no file name!
               raise Ada.IO_Exceptions.Name_Error with "unknown directory '" & Path (URI) & "'";
            end;

         else
            --  It's a file request.
            declare
               use Ada.Directories;
               use SAL.Config_Files;

               Filename  : constant String := -Source_Root & Path (URI) & File (URI);
               Ext       : constant String := Extension (Filename);
               Mime_Type : constant String :=
                 --  MIME types from https://www.iana.org/assignments/media-types/media-types.xhtml
                 --  also GNAT/share/examples/aws/web_elements/mime.types
                 (if    Ext = "jpg" then "image/jpeg"
                  elsif Ext = "mp3" then "audio/mpeg"
                  elsif Ext = "pdf" then "application/pdf"
                  else "");

               Db : Configuration_Type;
               Result : AWS.Response.Data;
               I : SAL.Config_Files.Iterator_Type;
               Prev_Downloaded : SAL.Time_Conversions.Extended_ASIST_Time_String_Type;

            begin
               if Mime_Type'Length = 0 then
                  return Acknowledge
                    (Status_Code  => AWS.Messages.S500,
                     Message_Body => "<p>file extension '" & Ext & "' not supported.");
               end if;

               if Exists (Filename) then
                  if Ext = "mp3" then
                     Open
                       (Db,
                        -Db_Filename,
                        Missing_File          => Raise_Exception,
                        Duplicate_Key         => Raise_Exception,
                        Read_Only             => False,
                        Case_Insensitive_Keys => True);

                     --  Find does not want leading / on filename
                     I := Find (Db, Filename (Length (Source_Root) + 2 .. Filename'Last));

                     if I = Null_Iterator then
                        return Acknowledge
                          (Status_Code  => AWS.Messages.S500,
                           Message_Body => "file not found");
                     end if;

                     Write_Last_Downloaded (Db, I, SAL.Time_Conversions.To_TAI_Time (Ada.Calendar.Clock));

                     Prev_Downloaded := SAL.Time_Conversions.To_Extended_ASIST_String (Read_Prev_Downloaded (Db, I));

                     Close (Db);
                  end if;

                  Result := File (Mime_Type, Filename);
                  AWS.Response.Set.Add_Header (Result, "X-prev_downloaded", Prev_Downloaded);
                  return Result;

               else
                  return Acknowledge
                    (Status_Code  => AWS.Messages.S404,
                     Message_Body => "<p>file '" & Filename & "' not found.");
               end if;
            exception
            when Name_Error =>
               --  Exists raises Name_Error if the directory does not exist
               return Acknowledge
                 (Status_Code  => AWS.Messages.S404,
                  Message_Body => "<p>file '" & Filename & "' not found.");
            when E : others =>
               if Is_Open (Db) then
                  Close (Db);
               end if;
               return Acknowledge
                 (Status_Code  => AWS.Messages.S500,
                  Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
            end;
         end if;

      when PUT =>
         --  Notes
         declare
            use Ada.Directories;
            use Ada.Strings.Fixed;
            use Ada.Text_IO;
            Pathname : constant String := -Source_Root & Path (URI);
            Filename : constant String := Pathname & File (URI);
            Data     : constant String := -Binary_Data (Request);
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

            return Acknowledge (Status_Code  => AWS.Messages.S200);
         exception
         when E : others =>
            return Acknowledge
              (Status_Code  => AWS.Messages.S500,
               Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
         end;

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
         Db : SAL.Config_Files.Configuration_Type;
      begin
         case Argument_Count is
         when 1 | 2 =>
            Open (Config, Argument (1));

            Enable_Log := Argument_Count = 2;

            Db_Filename := +Read (Config, "DB_Filename", Missing_Key => Raise_Exception);

            Open
              (Db,
               Read (Config, "DB_Filename", Missing_Key => Raise_Exception),
               Missing_File          => Raise_Exception,
               Duplicate_Key         => Raise_Exception,
               Read_Only             => False,
               Case_Insensitive_Keys => True);

            Source_Root := +As_File (Read (Db, SMM.Root_Key, Missing_Key => Raise_Exception));

            Close (Db);

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
         Obj : Object := Default_Config;
      begin
         Server_Name (Obj, "SMM Server");

         --  Get_Host_By_Name includes VMware IP addresses, which are not
         --  useful for sync from outside this box. But there doesn't
         --  seem to be a way to tell which ones those are from here. So
         --  we get the address to use from the config file.
         Server_Host (Obj, Read (Config, "Server_IP", Missing_Key => Raise_Exception));

         if Enable_Log then
            Log_File_Directory (Obj, -Source_Root & "/remote_cache/");
            Log_Filename_Prefix (Obj, "smm-server");

            AWS.Server.Log.Start (Ws, Auto_Flush => True);

            Put_Line ("logging to   " & Log_File_Directory (Obj) & "smm-server.log");
         else
            Put_Line ("not logging");
         end if;

         AWS.Server.Start
           (Ws,
            Callback => Handle_Request'Access,
            Config   => Obj);
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
