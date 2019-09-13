--  Abstract :
--
--  Stephe's Music Manager Server
--
--  Copyright (C) 2016 - 2019 Stephen Leake All Rights Reserved.
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
with AWS.Log;
with AWS.MIME;
with AWS.Messages;
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Config_Files.Integer;
with SAL.Time_Conversions;
with SMM.Database;
with SMM.JPEG;
with SMM.Song_Lists;
package body SMM.Server is

   Source_Root : Unbounded_String; -- Root of music files; does not end in /
   Server_Data : Unbounded_String;
   --  Relative to Source_Root; contains server html, css, js files; does not end in /

   DB_Filename : Unbounded_String;
   Enable_Log  : Boolean := False;
   Debug_Log   : AWS.Log.Object;

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
      Height            : in Integer;
      Class             : in String)
     return String
   is
      Size : constant SMM.JPEG.Size_Type := SMM.JPEG.Size (-Source_Root & "/" & Relative_Resource);
   begin
      if Size.X <= Width and Size.Y <= Height then
         return "<img src=""/" & Relative_Resource & """ alt=""" & Label & """ class=""" & Class & """>";
      else
         --  The size specified here is overwritten by Scale_Px. It limits the
         --  display size before Scale_Px runs, to avoid large-scale
         --  reformatting as the page loads.
         return "<img src=""/" & Relative_Resource & """" &
           " onload=""Scale_Px(event," & Integer'Image (Width) & "," & Integer'Image (Height) & ")""" &
           " alt=""" & Label & """ class=""" & Class & """ width=" & Integer'Image (Width) & """ height=" &
           Integer'Image (Height) & """>";
      end if;
   end Server_Img;

   function Server_Img_Set
     (Root  : in String;
      Ext   : in String;
      Label : in String;
      Class : in String := "")
     return String
   is
      Size_Low  : constant String := "desktop";
      Size_Med  : constant String := "tablet";
      Size_High : constant String := "phone";
   begin
      return "<img src=""/" & Root & "-" & Size_Low & Ext & """" &
        " srcset=""/" & Root & "-" & Size_Med & Ext & " 2x, /" & Root & "-" & Size_High & Ext & " 3x""" &
        " alt=""" & Label & """" & (if Class = "" then "" else " class=""" & Class & """") & ">";
   end Server_Img_Set;

   function Days_Ago (Date : in Database.Time_String) return String
   is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      use SAL.Time_Conversions;
      Date_1 : constant Time := Value (Date);
      Today  : constant Time := Ada.Calendar.Clock;
   begin
      if Date = SMM.Database.Default_Time_String then
         return "-";
      else
         return Integer'Image (Integer ((Today - Date_1) / Seconds_Per_Day));
      end if;
   end Days_Ago;

   ----------
   --  Specific request handlers, alphabetical

   function Handle_Download (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use Ada.Containers;
      use Ada.Exceptions;
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
        (if Over_Select_Ratio_Param'Length > 0 then Float'Value (Over_Select_Ratio_Param) else 1.1);
      --  Larger over_select_ratio causes more mixing of total song order,
      --  but it also makes it take longer to download new songs. 2.0 is too high.

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
         Seed              => Seed,
         Debug             => Enable_Log,
         Debug_Log         => Debug_Log);

      for I of Songs loop
         declare
            Cur : constant SMM.Database.Cursor := DB.Find_ID (I);
         begin
            if Cur.Has_Element then
               Response := Response & Normalize (Cur.File_Name) & ASCII.CR & ASCII.LF;
            else
               --  Must be a bad play before/after link. Need an error message protocol.
               null;
            end if;
         end;
      end loop;

      return AWS.Response.Build ("text/plain", Response);
   exception
   when E : others =>
      return AWS.Response.Acknowledge
        (Status_Code  => AWS.Messages.S500,
         Message_Body => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_Download;

   function Handle_Field (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use SMM.Database;

      URI_Param : constant AWS.Parameters.List := AWS.URL.Parameters (URI);
      DB        : SMM.Database.Database;
   begin
      --  From Emacs notes, query looks like:
      --
      --  field?id=<id>&field=<field-name>

      if URI_Param.Is_Empty then
         return AWS.Response.Acknowledge (AWS.Messages.S400, "no params; usage field?id=<id>&field=<field_name>");

      elsif URI_Param.Exist ("id") and URI_Param.Exist ("field") then
         DB.Open (-DB_Filename);

         declare
            I          : constant Cursor := Find_ID (DB, Integer'Value (URI_Param.Get ("id")));
            Field_Name : constant String := URI_Param.Get ("field");
         begin
            if not I.Has_Element then
               return AWS.Response.Acknowledge (AWS.Messages.S400, "id " & URI_Param.Get ("id") & " not found");
            end if;

            if Field_Name = "artist" then
               return AWS.Response.Build ("text/plain", I.Artist);
            elsif Field_Name = "album" then
               return AWS.Response.Build ("text/plain", I.Album);
            elsif Field_Name = "category" then
               return AWS.Response.Build ("text/plain", I.Category);
            elsif Field_Name = "title" then
               return AWS.Response.Build ("text/plain", I.Title);
            else
               return AWS.Response.Acknowledge (AWS.Messages.S400, "id " & URI_Param.Get ("id") & " not found");
            end if;
         end; --  Free cursor

      else
         return AWS.Response.Acknowledge (AWS.Messages.S400, "invalid query params '" & AWS.URL.Parameters (URI) & "'");
      end if;
   exception
   when Constraint_Error =>
      --  from Integer'Value (id)
      return AWS.Response.Acknowledge (AWS.Messages.S400, "invalid id '" & URI_Param.Get ("id") & "'");
   end Handle_Field;

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

   function Handle_ID (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use SMM.Database;

      URI_Param : constant AWS.Parameters.List := AWS.URL.Parameters (URI);
      DB        : SMM.Database.Database;
   begin
      if URI_Param.Is_Empty then
         return AWS.Response.Acknowledge (AWS.Messages.S400, "no params; usage id?file=<file_name>");

      else
         --  From Emacs notes buffer page, query looks like
         --  'id?file=<file_name>'

         if not URI_Param.Exist ("file") then
            return AWS.Response.Acknowledge
              (AWS.Messages.S400, "missing 'file' param: '" & AWS.URL.Parameters (URI) & "'");
         end if;

         DB.Open (-DB_Filename);

         declare
            File_Name : constant String := URI_Param.Get ("file");
            I         : constant Cursor := DB.Find_File_Name (File_Name);
         begin
            if I.Has_Element then
               return AWS.Response.Build ("text/plain", Integer'Image (I.ID));
            else
               return AWS.Response.Acknowledge (AWS.Messages.S400, "file not in db: '" & File_Name & "'");
            end if;
         end;
      end if;
   end Handle_ID;

   function Handle_Meta (URI : in AWS.URL.Object) return AWS.Response.Data
   is
      use Ada.Directories;
      use AWS.URL;

      Source_Dir : constant String := -Source_Root & Path (URI);
      Response   : Unbounded_String;
      Min_Size : File_Size := 0;

      Min_Jpg_Size : constant File_Size := 40_000; -- exclude tiny images

      procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
      is begin
         if Size (Dir_Ent) > Min_Size then
            Response := Response &
              Relative_Name (-Source_Root, Normalize (Full_Name (Dir_Ent))) & ASCII.CR & ASCII.LF;
         end if;
      end Copy_Aux;
   begin
      Min_Size := Min_Jpg_Size;
      Search
        (Directory => Source_Dir,
         Pattern   => "AlbumArt*.jpg",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      Min_Size := 0;
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
      use Ada.Strings.Fixed;
      use Ada.Strings;
      use SMM.Database;

      URI_Param : constant AWS.Parameters.List := Decode_Plus (AWS.URL.Parameters (URI));
      DB        : SMM.Database.Database;

      Search_Result_ID : constant String := "search_result";

      Response_1 : constant String := "<!DOCTYPE html>" & ASCII.LF &
        "<html lang=""en"">" &
        "<meta http-equiv=""Content-Type"" content=""text/html; charset=utf-8"">" & ASCII.LF &
        "<head>" & ASCII.LF &
        "<script src=""/" & (-Server_Data) & "/songs.js""></script>" & ASCII.LF &
        "<link type=""text/css"" rel=""stylesheet"" href=""/" & (-Server_Data) & "/songs.css""/>" & ASCII.LF &
        "</head>";

      Response_2 : constant String := "<div class=""tabbar"">" &
        "<button class=""tabbutton"" id=""general_search_button""" &
        " onclick=""SelectTab('general_search_button', 'general_search_tab', '" & Search_Result_ID &
        "')"">General Search</button>" &
        "<button class=""tabbutton"" id=""detailed_search_button""" &
        " onclick=""SelectTab('detailed_search_button', 'detailed_search_tab', '" & Search_Result_ID &
        "')"">Detailed Search</button>" &
        "</div>" & ASCII.LF &
        "<div class=""tabcontent"" id=""general_search_tab""><form action=""/search"" method=get>" &
        "<input type=submit value=""Search"">" &
        "<input type=search autofocus name=""search"" value=""" & URI_Param.Get ("search") & """>" &
        "</form></div>" & ASCII.LF &
        "<div class=""tabcontent"" id=""detailed_search_tab"">" &
        "<form action=""/search"" method=get><div class=""table"">" &
        "<div class=""row""><label>Title </label>" &
        "<input type=search name=""title"" value=""" & URI_Param.Get ("title") & """></div>" &
        "<div class=""row""><label>Artist </label>" &
        "<input type=search name=""artist"" value=""" & URI_Param.Get ("artist") & """></div>" &
        "<div class=""row""><label>Album </label>" &
        "<input type=search name=""album"" value=""" & URI_Param.Get ("album") & """></div>" &
        "<div class=""row""><label>Album Artist</label>" &
        "<input type=search name=""album_artist"" value=""" & URI_Param.Get ("album_artist") & """></div>" &
        "<div class=""row""><label>Composer</label>" &
        "<input type=search name=""composer"" value=""" & URI_Param.Get ("composer") & """></div>" &
        "<div class=""row""><label>Category </label>" &
        "<input type=search name=""category"" value=""" & URI_Param.Get ("category") & """></div>" &
        "</div><input type=submit value=""Search"">" &
        "</form></div><hr>" & ASCII.LF;

      Response      : Unbounded_String;
      Current_Album : Unbounded_String;
      Album_ID      : Integer := 0;

      function Search_Result (I : in Cursor) return String
      is
         Title_Row : constant Unbounded_String := +"<tr>" &
           "<td><a href=""/" & AWS.URL.Encode (I.File_Name, SMM.File_Name_Encode_Set) &
           """>" & Server_Img_Set (-Server_Data & "/play_icon", ".png", "play") &
           "</a></td>" &
           "<td class=""text"">" & I.Artist & "</td>" &
           "<td class=""text"">" & I.Composer & "</td>" &
           "<td class=""text"">" & I.Title & "</td>" &
           "<td class=""text"">" & Days_Ago (I.Last_Downloaded) & " / " & Days_Ago (I.Prev_Downloaded) & "</td>" &
           "<td><div class=""categories_list text"" onclick=""EditCategory(event)""" &
           " tabindex=""0"" onkeydown=""EditCategory(event)""" &
           " id=""" & I.ID_String & """>" & I.Category & "</div></td>" &
           "<td class=""text"">" &
           (if I.Play_Before /= Null_ID then "v"
            elsif I.Play_After /= Null_ID then "^"
            else "") & "</td>" &
           "</tr>" & ASCII.LF;

         Result : Unbounded_String;
      begin
         if I.Album'Length > 0 and then I.Album = -Current_Album then
            return -Title_Row;

         else
            if Length (Current_Album) > 0 then
               --  Terminate previous album title table and album item
               Result := Result & "</tbody></table></li><hr>";
            end if;

            Album_ID := Album_ID + 1;

            if I.Album'Length = 0 then
               Current_Album := +"no album";
            else
               Current_Album := +I.Album;
            end if;

            declare
               Album_Item : Unbounded_String := +"<li id=""album_" & Trim (Integer'Image (Album_ID), Both) &
                 """ class=""album_li"">" &
                 "<div class=""album_row""><a class=""text"" href=""search?album=" & AWS.URL.Encode
                   (I.Album, SMM.File_Name_Encode_Set) & """>" & I.Album & "</a>" &
                 "<div class=""text"">" & I.Album_Artist &
                 (if I.Year /= No_Year then Integer'Image (I.Year) else "") & "</div>";

               Meta : constant AWS.Containers.Tables.Table_Type := Meta_Files (Containing_Directory (I.File_Name));
            begin
               for J in 1 .. Meta.Count loop
                  if To_Lower (Extension (Meta.Get_Name (J))) = "jpg" then
                     Album_Item := Album_Item & Server_Img
                       (Meta.Get_Value (J), "album art", 100, 100, Class => "album_art_item");
                  end if;
               end loop;

               for J in 1 .. Meta.Count loop
                  if To_Lower (Meta.Get_Name (J)) = "liner_notes.pdf" then
                     Album_Item := Album_Item & Server_Href
                       (Meta.Get_Value (J), Server_Img_Set
                          (-Server_Data & "/liner_notes_icon", ".png", "liner notes",
                           Class => "album_art_item"));
                  end if;
               end loop;
               Album_Item := Album_Item & "</div>" & ASCII.LF;

               Result := Result & Album_Item & "<table><tbody>" & Title_Row;
               return -Result;
            end;
         end if;
      end Search_Result;

      function To_SQL_Param (Param : in AWS.Parameters.List) return SMM.Database.Field_Values
      is begin
         return Result : SMM.Database.Field_Values do
            for I in Fields loop
               declare
                  Value : constant String := Param.Get (-Field_Image (I)); -- empty string if not present
               begin
                  if Value'Length > 0 then
                     Result (I) := +Value;
                  end if;
               end;
            end loop;
         end return;
      end To_SQL_Param;

   begin
      --  From search page, query looks like one of:
      --
      --  ?search=michael+joni+miles
      --
      --  ?title=michael&artist=joni&album=miles&category=
      --
      --  ?album=miles

      if URI_Param.Is_Empty then
         --  Return search page with no results.
         Response := +Response_1 & "<body onload=""InitTabs()"">" & Response_2 & "</body></html>";
         return AWS.Response.Build ("text/html", Response);

      elsif URI_Param.Exist ("search") or
        URI_Param.Exist ("title") or URI_Param.Exist ("artist") or URI_Param.Exist ("album") or
        URI_Param.Exist ("album_artist") or URI_Param.Exist ("category")
      then
         DB.Open (-DB_Filename);

         declare
            I                : Cursor;
            Button           : Unbounded_String;
            Tab              : Unbounded_String;
         begin
            if URI_Param.Exist ("search") then
               --  General search
               I      := DB.Find_Like (URI_Param.Get ("search"), Order_By => (Album, Track));
               Button := +"general_search_button";
               Tab    := +"general_search_tab";
            else
               --  Detailed search
               I      := DB.Find_Like (To_SQL_Param (URI_Param), Order_By => (Album, Track));
               Button := +"detailed_search_button";
               Tab    := +"detailed_search_tab";
            end if;

            Response := +Response_1 &
                "<body onload=""SelectTab('" & Button & "', '" & Tab & "', '" & Search_Result_ID & "')"">" &
              Response_2;

            if not I.Has_Element then
               Response := Response  & "<p>no matching entries found</p></body></html>";
               return AWS.Response.Build ("text/html", Response);
            end if;

            Response := Response & "<div id=""" & Search_Result_ID & """ class=""" & Search_Result_ID & """><ul>";
            loop
               exit when not I.Has_Element;
               Response := Response & Search_Result (I) & ASCII.LF;

               I.Next;
            end loop;
         end; --  Free cursor

         --  Terminate last album title table and album item
         Response := Response & "</tbody></table></li><hr>";

         --  Terminate album list, search result scroll, body, doc.
         Response := Response & "</ul></div></body></html>";

         return AWS.Response.Build ("text/html", Response);

      else
         return AWS.Response.Acknowledge (AWS.Messages.S400, "invalid query params '" & AWS.URL.Parameters (URI) & "'");
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
         --  'update?id=<id>&<field>=<data>'
         --  only update field if present.

         --  From Web search results page, query looks like
         --
         --  'update?ref=<search uri>&id=<id>&<field>=<data>'
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
               return AWS.Response.File ("image/x-icon", (-Source_Root) & "/" & (-Server_Data) & "/app.ico");

            elsif URI_File = "field" then
               return Handle_Field (URI);

            elsif URI_File = "file" then
               --  record download time in db
               return Handle_File (URI, Name_In_Param => True);

            elsif URI_File = "id" then
               return Handle_ID (URI);

            elsif URI_File = "meta" then
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

      Config : SAL.Config_Files.Configuration_Type;
      Ws     : AWS.Server.HTTP;
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
         --
         --  In GNAT GPL 2018, AWS uses GNAT.Sockets, and Bind does not support
         --  IPv6. Sigh.
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

               AWS.Log.Start
                 (Debug_Log,
                  File_Directory  => Log_Dir_Name,
                  Filename_Prefix => "smm-server-debug",
                  Auto_Flush      => True);
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

            Put_Line ("logging to   " & Log_File_Directory (Obj) & "smm-server.log, smm-server-debug.log");
         end if;
      end;

      declare
         use Ada.Text_IO;
         use AWS.Config;
         Ws_Config : constant Object := AWS.Server.Config (Ws);
      begin
         Put_Line
           ("listening on " & Server_Host (Ws_Config) & ":" & Integer'Image (Server_Port (Ws_Config)) &
              " db : '" & (-DB_Filename) & "' source_root: '" & (-Source_Root) & "' server_data: '" &
              (-Server_Data) & "'");
      end;

      AWS.Server.Wait;
   end Server;
end SMM.Server;
--  Local Variables:
--  ada-indent-comment-gnat: t
--  End:
