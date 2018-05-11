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
with AWS.Containers.Tables;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.Server.Log;
with AWS.Status;
with AWS.URL;
with Ada.Calendar.Formatting;
with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Config_Files.Integer;
with SAL.Time_Conversions;
with SMM.Database;
with SMM.JPEG;
with SMM.Song_Lists;
package body SMM.Server is

   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;
   function "&"
     (Left  : in Ada.Strings.Unbounded.Unbounded_String;
      Right : in String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded."&";
   function "&"
     (Left  : in Ada.Strings.Unbounded.Unbounded_String;
      Right : in Character)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded."&";
   function Length (Item : in Ada.Strings.Unbounded.Unbounded_String) return Integer
     renames Ada.Strings.Unbounded.Length;

   Source_Root : Ada.Strings.Unbounded.Unbounded_String; -- Root of music files; does not end in /
   Server_Data : Ada.Strings.Unbounded.Unbounded_String;
   --  Relative to Source_Root; contains server html, css, js files; does not end in /

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

   function Meta_Files (Source_Dir : in String) return AWS.Containers.Tables.Table_Type
   is
      use Ada.Directories;
      use Ada.Strings.Unbounded;

      Result : AWS.Containers.Tables.Table_Type;

      procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
      is
         Path_Name : constant String := Relative_Name (-Source_Root, Normalize (Full_Name (Dir_Ent)));
      begin
         Result.Add
           (Name => Simple_Name (Path_Name),
            Value => Path_Name);
      end Copy_Aux;
   begin
      Search
        (Directory => -Source_Root & "/" & Source_Dir,
         Pattern   => "AlbumArt*.jpg",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      Search
        (Directory => -Source_Root & "/" & Source_Dir,
         Pattern   => "liner_notes.pdf",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      return Result;
   exception
   when Ada.IO_Exceptions.Name_Error =>
      --  GNAT runtime sets message to "(unknown directory "")"; no file name!
      raise Ada.IO_Exceptions.Name_Error with "unknown directory '" & Source_Dir & "'";
   end Meta_Files;

   function Server_Href (Relative_Resource : in String; Label : in String) return String
   is begin
      return "<a href=""/" & Relative_Resource & """>" & Label & "</a>";
   end Server_Href;

   function Server_Img
     (Relative_Resource : in String;
      Label             : in String;
      Width             : in Integer;
      Height            : in Integer)
     return String
   is
      Size : constant SMM.JPEG.Size_Type := SMM.JPEG.Size (-Source_Root & "/" & Relative_Resource);
   begin
      if Size.X <= Width and Size.Y <= Height then
         return "<img src=""/" & Relative_Resource & """ alt=""" & Label & """>";
      else
         return "<img src=""/" & Relative_Resource & """" &
           " onload=""Scale_Px(event," & Integer'Image (Width) & "," & Integer'Image (Height) & ")""" &
           " alt=""" & Label & """>";
      end if;
   end Server_Img;

   function Server_Img_Set
     (Root      : in String;
      Ext       : in String;
      Size_Low  : in String;
      Size_Med  : in String;
      Size_High : in String;
      Label     : in String)
     return String
   is begin
      return "<img src=""/" & Root & "-" & Size_Low & Ext & """" &
        "srcset=""/" & Root & "-" & Size_Med & Ext & " 2x, /" & Root & "-" & Size_High & Ext & " 3x""" &
        " alt=""" & Label & """>";
   end Server_Img_Set;

   function Days_Ago (Date : in Database.Time_String) return String
   is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      use SAL.Time_Conversions;
      Date_1 : constant Time := Value (Date);
      Today  : constant Time := Ada.Calendar.Clock;
   begin
      return Integer'Image (Integer ((Today - Date_1) / Seconds_Per_Day));
   end Days_Ago;

   ----------
   --  Specific request handlers, alphabetical

   function Handle_Append_Category (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use SMM.Database;

      URI_Param : constant AWS.Parameters.List := Decode_Plus (AWS.URL.Parameters (URI));
      DB        : SMM.Database.Database;
      SQL_Param : SMM.Database.Field_Values;

   begin
      if URI_Param.Is_Empty then
         return AWS.Response.Acknowledge
           (AWS.Messages.S400, "invalid query params: '" & AWS.URL.Parameters (URI) & "'");

      else
         --  From Emacs notes buffer page, query looks like
         --  'append_category?file=<file_name>&category=<data>'

         if not URI_Param.Exist ("file") then
            return AWS.Response.Acknowledge
              (AWS.Messages.S400, "missing 'file' param: '" & AWS.URL.Parameters (URI) & "'");
         end if;

         if not URI_Param.Exist ("category") then
            return AWS.Response.Acknowledge
              (AWS.Messages.S400, "missing 'category' param: '" & AWS.URL.Parameters (URI) & "'");
         end if;

         SQL_Param (Category) := +URI_Param.Get ("category");

         DB.Open (-DB_Filename);

         declare
            File_Name : constant String := URI_Param.Get ("file");
            I         : constant Cursor := DB.Find_File_Name (File_Name);
         begin
            if I.Has_Element then
               DB.Update (I, SQL_Param);
            else
               return AWS.Response.Acknowledge (AWS.Messages.S400, "file not in db: '" & File_Name & "'");
            end if;
         end;

         return AWS.Response.Acknowledge (AWS.Messages.S200, "file updated");
      end if;
   end Handle_Append_Category;

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

   function Handle_File (URI : in AWS.URL.Object; Name_In_Param : in Boolean) return AWS.Response.Data
   is
      --  If Name_In_Param, assume it's from Android app updating playlist;
      --  record download time.

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
        (if    Ext = "css" then "text/css"
         elsif Ext = "jpg" then "image/jpeg"
         elsif Ext = "js"  then "text/javascript"
         elsif Ext = "mp3" then "audio/mpeg"
         elsif Ext = "pdf" then "application/pdf"
         elsif Ext = "png" then "image/png"
         elsif Ext = "svg" then "image/svg+xml"
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
         Result := AWS.Response.File (Mime_Type, Filename);

         if Ext = "mp3" and Name_In_Param then
            DB.Open (-DB_Filename);

            --  Find does not want leading / on filename
            I := DB.Find_File_Name (Filename (Length (Source_Root) + 2 .. Filename'Last));

            if not I.Has_Element then
               return AWS.Response.Acknowledge
                 (Status_Code  => AWS.Messages.S500,
                  Message_Body => "file not found");
            end if;

            I.Write_Last_Downloaded (DB, SMM.Database.UTC_Image (Ada.Calendar.Clock));

            Prev_Downloaded := I.Prev_Downloaded;

            Finalize (DB);

            AWS.Response.Set.Add_Header (Result, "X-prev_downloaded", Prev_Downloaded);
         end if;

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
      use Ada.Characters.Handling;
      use Ada.Directories;
      use SMM.Database;

      URI_Param : constant AWS.Parameters.List := Decode_Plus (AWS.URL.Parameters (URI));
      DB        : SMM.Database.Database;
      SQL_Param : SMM.Database.Field_Values;

      function Search_Result (I : in Cursor) return String
      is
         Meta : constant AWS.Containers.Tables.Table_Type := Meta_Files (Containing_Directory (I.File_Name));

         Result : Unbounded_String := +"<tr>" &
           "<td><a href=""/" & AWS.URL.Encode (I.File_Name, SMM.File_Name_Encode_Set) &
           """>" & Server_Img_Set (-Server_Data & "/play_icon", ".png", "19x19", "76x76", "171x171", "play") &
           "</a></td>" &
           "<td><table class=""subtable""><tbody>" &
           "<tr><td class=""text"" style=""background-color: GhostWhite"">" & I.Artist & "</td></tr>" &
           "<tr><td class=""text"">" & Days_Ago (I.Last_Downloaded) & " / " &
           Days_Ago (I.Prev_Downloaded) & "</td></tr>" &
           "</tbody></table></td>" &
           "<td><table class=""subtable""><tbody>" &
           "<tr><td class=""text"">" & I.Album & "</td>" &
           "<tr><td class=""text"" style=""background-color: GhostWhite"">" & I.Title & "</td>" &
           "</tr></tbody></table></td>";
      begin
         Result := Result & "<td><div class=""album_art_list""><table>";
         for J in 1 .. Meta.Count loop
            if To_Lower (Extension (Meta.Get_Name (J))) = "jpg" then
               Result := Result & "<tr><td>" & Server_Img (Meta.Get_Value (J), "album art", 100, 100) & "</td></tr>";
            end if;
         end loop;
         Result := Result & ASCII.LF & "</table></div></td>";

         Result := Result & "<td>";
         for J in 1 .. Meta.Count loop
            if To_Lower (Meta.Get_Name (J)) = "liner_notes.pdf" then
               Result := Result & Server_Href
                 (Meta.Get_Value (J), Server_Img_Set
                    (-Server_Data & "/liner_notes_icon", ".png", "28x37", "112x148", "252x333", "liner notes"));
            end if;
         end loop;
         Result := Result & "</td>";

         Result := Result & "<td><div class=""categories_list text"" onclick=""EditCategory(event)"" id=""" &
           I.ID_String & """>" & I.Category & "</div></td>";

         Result := Result & "</tr>" & ASCII.LF;
         return -Result;
      end Search_Result;

      Required_Fields : constant array (Fields) of Boolean :=
        (Artist | Album | Title => True,
         others => False);

      function Valid_Query return Boolean
      is begin
         for Field in Fields loop
            if Required_Fields (Field) and then not URI_Param.Exist (-Field_Image (Field)) then
               return False;
            end if;
         end loop;
         return True;
      end Valid_Query;

   begin
      if URI_Param.Is_Empty then
         --  Return search page with no results.
         return AWS.Response.File ("text/html", (-Source_Root) & "/" & (-Server_Data) & "/search.html");

      elsif not Valid_Query then
         return AWS.Response.Acknowledge (AWS.Messages.S400, "invalid query params '" & AWS.URL.Parameters (URI) & "'");

      else
         --  From search page, query looks like
         --  '?title=michael&artist=joni&album=miles'; all three fields always
         --  present, may be empty.
         --
         --  Support links from result pages etc; allow fields to be not present.

         for I in Fields loop
            declare
               Value : constant String := URI_Param.Get (-Field_Image (I)); -- empty string if not present
            begin
               if Value'Length > 0 then
                  SQL_Param (I) := +Value;
               end if;
            end;
         end loop;

         declare
            Response : Unbounded_String := +"<!DOCTYPE html>" & ASCII.LF &
              "<html lang=""en"">" &
              "<meta http-equiv=""Content-Type"" content=""text/html; charset=utf-8"">" & ASCII.LF &
              "<head>" & ASCII.LF &
              "<script src=""/" & (-Server_Data) & "/songs.js""></script>" & ASCII.LF &
              "<script src=""/" & (-Server_Data) & "/debug_head.js""></script>" & ASCII.LF &
              "<link type=""text/css"" rel=""stylesheet"" href=""/" & (-Server_Data) & "/songs.css""/>" &
              "</head><body>" & ASCII.LF &
              "<table>" & ASCII.LF &
              "<thead><tr>" &
              "<th class=""text"">play</th>" &
              "<th><table style=""width: 100%;""><thead>" &
              "<tr><th class=""text"" style=""border-bottom: solid white;"">artist</th></tr>" &
              "<tr><th class=""text"" style=""width: 5em;"">last/prev downloaded</th>" &
              "</tr></thead></table></th>" &
              "<th><table style=""width: 100%;""><thead>" &
              "<tr><th class=""text"" style=""border-bottom: solid white;"">album</th></tr>" &
              "<tr><th class=""text"">title</th></tr>" &
              "</thead></table></th>" &
              "<th class=""text"">album art</th>" &
              "<th class=""text"" style=""width: 1em;"">liner notes</th>" &
              "<th class=""text"">categories</th>" &
              "</tr></thead>" & ASCII.LF &
              "<tbody>" & ASCII.LF &
              "<script src=""/" & (-Server_Data) & "/debug_body.js""></script>";
         begin
            DB.Open (-DB_Filename);

            declare
               I : Cursor := DB.Find_Like (SQL_Param);
            begin
               if not I.Has_Element then
                  return AWS.Response.Acknowledge
                    (Status_Code  => AWS.Messages.S404,
                     Message_Body => Image (SQL_Param) & " not found");
               end if;

               loop
                  exit when not I.Has_Element;
                  Response := Response & Search_Result (I) & ASCII.LF;

                  I.Next;
               end loop;
            end; --  Free cursor

            Response := Response & "</tbody>" & ASCII.LF & "</table>" & ASCII.LF & "</body></html>";
            return AWS.Response.Build ("text/html", Response);
         end;
      end if;
   end Handle_Search;

   function Handle_Update (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use SMM.Database;

      URI_Param : constant AWS.Parameters.List := Decode_Plus (AWS.URL.Parameters (URI));
      DB        : SMM.Database.Database;
      SQL_Param : SMM.Database.Field_Values;
      Key_Field : Unbounded_String;
      Ref       : Unbounded_String;
      Cancel    : Boolean                      := False;

      function Redirect_Search return AWS.Response.Data
      is
         use AWS.Response;
      begin
         return Result : Data do
            Set.Location     (Result, AWS.URL.Decode (-Ref));
            Set.Status_Code  (Result, AWS.Messages.S303);
            Set.Content_Type (Result, AWS.MIME.Text_HTML);
            Set.Message_Body (Result, "back to search");
         end return;
      end Redirect_Search;

   begin
      if URI_Param.Is_Empty then
         return AWS.Response.Acknowledge
           (AWS.Messages.S400, "invalid query params: '" & AWS.URL.Parameters (URI) & "'");

      else
         --  From Emacs notes buffer page, query looks like
         --  'update?file=<file_name>&<field>=<data>'
         --  only update field if present.

         --  From Web search results page, query looks like
         --
         --  'update?ref=<search uri>id=<id>&<field>=<data>'
         --
         --  Only update field if present. If a "cancel" param is present, don't update anything.

         if URI_Param.Exist ("id") then
            Key_Field := +"id";
         elsif URI_Param.Exist ("file") then
            Key_Field := +"file";
         else
            return AWS.Response.Acknowledge
              (AWS.Messages.S400, "missing 'id' or 'file' param: '" & AWS.URL.Parameters (URI) & "'");
         end if;

         for I in 1 .. URI_Param.Count loop
            declare
               Field_Name : String renames URI_Param.Get_Name (I);
            begin
               if Field_Name = "cancel" then
                  Cancel := True;
               elsif Field_Name = -Key_Field then
                  null;
               elsif Field_Name = "ref" then
                  Ref := +URI_Param.Get_Value (I);
               elsif Valid_Field (Field_Name) then
                  null;
               else
                  return AWS.Response.Acknowledge
                    (AWS.Messages.S400, "bad param name: '" & String'(Field_Name) & "'");
               end if;
            end;
         end loop;

         if Cancel then
            if Length (Ref) = 0 then
               return AWS.Response.Acknowledge (AWS.Messages.S200, "canceled");
            else
               return Redirect_Search;
            end if;
         end if;

         for I in Fields loop
            declare
               Value : constant String := URI_Param.Get (-Field_Image (I)); -- empty string if not present
            begin
               if Value'Length > 0 then
                  SQL_Param (I) := +Value;
               end if;
            end;
         end loop;

         DB.Open (-DB_Filename);

         declare
            I : constant Cursor :=
              (if -Key_Field = "id"
               then DB.Find_ID (Integer'Value (URI_Param.Get ("id")))
               else DB.Find_File_Name (URI_Param.Get ("file")));
         begin
            if I.Has_Element then
               DB.Update (I, SQL_Param);
            else
               return AWS.Response.Acknowledge
                 (AWS.Messages.S400, "not found in db: '" &
                    (if -Key_Field = "id"
                     then URI_Param.Get ("id")
                     else URI_Param.Get ("file"))
                    & "'");
            end if;
         end;

         if Length (Ref) = 0 then
            return AWS.Response.Acknowledge (AWS.Messages.S200, "updated");
         else
            return Redirect_Search;
         end if;
      end if;
   end Handle_Update;

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
               --  FIXME: does not show up in firefox address bar
               return AWS.Response.File ("image/png", "/" & (-Server_Data) & "/app_icon.png");

            elsif URI_File = "file" then
               return Handle_File (URI, Name_In_Param => True);

            elsif URI_File = "meta" then
               --  FIXME: move directory to query
               return Handle_Meta (URI);

            elsif URI_File = "search" then
               return Handle_Search (URI);

            else
               --  It's a file request; mp3, liner_notes, image
               return Handle_File (URI, Name_In_Param => False);
            end if;
         end;

      when PUT =>
         return Handle_Put_Notes (Request, URI);

      when POST =>
         declare
            URI_File : constant String := File (URI);
         begin
            if URI_File = "update" then
               return Handle_Update (URI);
            elsif URI_File = "append_category" then
               return Handle_Append_Category (URI);
            else
               return Acknowledge
                 (Status_Code  => AWS.Messages.S400, -- bad request
                  Message_Body => "unrecognized POST path '" & URI_File & "'");
            end if;
         end;

      when others =>
         return Acknowledge
           (Status_Code  => AWS.Messages.S400, -- bad request
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
            Server_Data := +Read (Config, "Server_Data", "server_data");

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
         Put_Line
           ("listening on " & Server_Host (Ws_Config) & ":" & Integer'Image (Server_Port (Ws_Config)) &
            " source_root: '" & (-Source_Root) & "' server_data: '" & (-Server_Data) & "'");
      end;

      AWS.Server.Wait;
   end Server;
end SMM.Server;
--  Local Variables:
--  ada-indent-comment-gnat: t
--  End:
