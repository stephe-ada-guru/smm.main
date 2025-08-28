--  Abstract :
--
--  Stephe's Music Manager Server
--
--  Implement a CGI script.
--
--  Copyright (C) 2016 - 2020, 2022, 2023, 2025 Stephen Leake All Rights Reserved.
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

with Ada.Calendar.Formatting;
with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;
with SAL.Config_Files;
with SAL.Gen_Definite_Doubly_Linked_Lists;
with SAL.Time_Conversions;
with SAL.Web_Utils;
with SMM.Database;
with SMM.JPEG;
with SMM.Song_Lists;
package body SMM.Server is

   subtype API_Versions is Integer range 1 .. 2;
   --  1 - API not specified in GET. Client always downloads all songs. 'download' => list of filenames
   --  2 - API specified in GET. Client only downloads new songs (all others previously downloaded).
   --      'get_new_songs_list => list of "Album_Artist", "album", "title", "filename"

   package String_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Ada.Strings.Unbounded.Unbounded_String);

   Source_Root : Unbounded_String; -- Absolute root of music files; does not end in /
   Server_Data : Unbounded_String;
   --  Absolute; contains server html, css, js files; does not end in /

   DB_Filename : Unbounded_String;

   function Meta_Files (Source_Dir : in String) return String_Lists.List
   is
      use Ada.Directories;

      Result : String_Lists.List;

      procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
      is
         Path_Name : constant String := Relative_Name (-Source_Root, Normalize (Full_Name (Dir_Ent)));
      begin
         Result.Append (+Path_Name);
      end Copy_Aux;
   begin
      Search
        (Directory => -Source_Root & "/" & Source_Dir,
         Pattern   => "*.jpg",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      Search
        (Directory => -Source_Root & "/" & Source_Dir,
         Pattern   => "*.png",
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

   function Handle_Get_New_Songs_List
     (Parameters : in SAL.Web_Utils.Parameter_Lists.Map;
      API        : in API_Versions)
     return String
   is
      --  Return an HTTP response that contains a list of least recently
      --  heard songs; client will build a playlist and maybe download the
      --  actual song files.
      use Ada.Containers;
      use Ada.Exceptions;
      use SAL.Web_Utils;
      use SMM.Database;
      use SMM.Song_Lists.Song_Lists;

      Category          : constant String     := Parameters.Element ("category");
      Count             : constant Count_Type := Count_Type'Value (Parameters.Element ("count"));
      New_Count         : constant Count_Type := Count_Type'Value (Parameters.Element ("new_count"));
      Record_Downloaded : constant Boolean    :=
        (if API > 1
         then Boolean'Value (Parameters.Element ("record_downloaded"))
         else False);

      Seed_Param : constant String     := Parameters.Element ("seed"); -- only used in unit tests
      Seed       : constant Integer    :=
        (if Seed_Param'Length > 0 then Integer'Value (Seed_Param) else 0);

      Over_Select_Ratio_Param : constant String := Parameters.Element ("over_select_ratio");
      Over_Select_Ratio       : constant Float  :=
        (if Over_Select_Ratio_Param'Length > 0 then Float'Value (Over_Select_Ratio_Param) else 1.1);
      --  Larger over_select_ratio causes more mixing of total song order,
      --  but it also makes it take longer to download new songs. 2.0 is too high.

      DB       : SMM.Database.Database;
      Songs    : List;
      Response : Unbounded_String;

      Need_Separator : Boolean := False;
   begin
      DB.Open (-DB_Filename);

      SMM.Song_Lists.Least_Recent_Songs
        (DB, Category, Songs,
         Song_Count        => Count,
         New_Song_Count    => New_Count,
         Over_Select_Ratio => Over_Select_Ratio,
         Seed              => Seed);

      for I of Songs loop
         declare
            Cur : constant SMM.Database.Cursor := DB.Find_ID (I);
         begin
            if Cur.Has_Element then
               if Need_Separator then
                  Response := Response & New_Line;
               else
                  Need_Separator := True;
               end if;

               Response := Response & Normalize (Cur.File_Name);

               if Record_Downloaded then
                  Cur.Write_Last_Downloaded (DB, SMM.Database.UTC_Image (Ada.Calendar.Clock));
               end if;

            else
               --  Must be a bad play before/after link. Need an error message protocol.
               null;
            end if;
         end;
      end loop;

      return HTTP_Response (S200, Content_Text_Plain, -Response);
   exception
   when E : others =>
      return HTML_Response
        (Status_Code => S500,
         Content => "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_Get_New_Songs_List;

   function Handle_Field
     (URI_Param : in SAL.Web_Utils.Parameter_Lists.Map;
      Query     : in String)
     return String
   --  Query is only used in error messages.
   is
      use SMM.Database;
      use SAL.Web_Utils;

      DB : SMM.Database.Database;
   begin
      --  From Emacs notes, query looks like:
      --
      --  field?id=<id>&field=<field-name>

      if URI_Param.Is_Empty then
         return HTML_Response (S400, "no params; usage field?id=<id>&field=<field_name>");

      elsif Exist (Map => URI_Param, Key => "id") and Exist (URI_Param, "field") then
         DB.Open (-DB_Filename);

         declare
            I          : constant Cursor := Find_ID (DB, Integer'Value (Get (URI_Param, "id")));
            Field_Name : constant String := Get (URI_Param, "field");
         begin
            if not I.Has_Element then
               return HTML_Response (S400, "id " & Get (URI_Param, "id") & " not found");
            end if;

            if Field_Name = "artist" then
               return HTTP_Response (S200, Content_Text_Plain, I.Artist);
            elsif Field_Name = "album" then
               return HTTP_Response (S200, Content_Text_Plain, I.Album);
            elsif Field_Name = "category" then
               return HTTP_Response (S200, Content_Text_Plain, I.Category);
            elsif Field_Name = "title" then
               return HTTP_Response (S200, Content_Text_Plain, I.Title);
            else
               return HTML_Response (S400, "id " & Get (URI_Param, "id") & " not found");
            end if;
         end; --  Free cursor

      else
         return HTML_Response (S400, "invalid query params '" & Query & "'");
      end if;
   exception
   when Constraint_Error =>
      --  from Integer'Value (id)
      return HTML_Response (S400, "invalid id '" & Get (URI_Param, "id") & "'");
   end Handle_Field;

   function Handle_ID
     (URI_Param : in SAL.Web_Utils.Parameter_Lists.Map;
      Query     : in String)
     return String
   --  Query is only used in error messages.
   is
      use SMM.Database;
      use SAL.Web_Utils;

      DB : SMM.Database.Database;
   begin
      if URI_Param.Is_Empty then
         return HTML_Response (S400, "no params; usage id?file=<file_name>");

      else
         --  From Emacs notes buffer page, query looks like
         --  'id?file=<file_name>'

         if not Exist (URI_Param, "file") then
            return HTML_Response (S400, "missing 'file' param: '" & Query & "'");
         end if;

         DB.Open (-DB_Filename);

         declare
            File_Name : constant String := Get (URI_Param, "file");
            I         : constant Cursor := DB.Find_File_Name (File_Name);
         begin
            if I.Has_Element then
               return HTTP_Response (S200, Content_Text_Plain, Integer'Image (I.ID));
            else
               return HTML_Response (S400, "file not in db: '" & File_Name & "'");
            end if;
         end;
      end if;
   end Handle_ID;

   function Handle_Meta (Path : in String) return String
   is
      use Ada.Directories;
      use SAL.Web_Utils;

      Source_Dir : constant String := -Source_Root & Path;
      Response   : Unbounded_String;
      Min_Size   : File_Size       := 0;

      Min_Jpg_Size : constant File_Size := 40_000; -- exclude tiny images, icons

      Need_Separator : Boolean := False;

      procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
      is begin
         if Size (Dir_Ent) > Min_Size then
            if Need_Separator then
               Response := Response & New_Line;
            else
               Need_Separator := True;
            end if;
            Response := Response &
              Relative_Name (-Source_Root, Normalize (Full_Name (Dir_Ent)));
         end if;
      end Copy_Aux;
   begin
      Min_Size := Min_Jpg_Size;
      Search
        (Directory => Source_Dir,
         Pattern   => "*.jpg",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      Min_Size := 0;
      Search
        (Directory => Source_Dir,
         Pattern   => "liner_notes.pdf",
         Filter    => (Ordinary_File => True, others => False),
         Process   => Copy_Aux'Access);

      return HTTP_Response (S200, Content_Text_Plain, -Response);
   exception
   when Ada.IO_Exceptions.Name_Error =>
      --  GNAT runtime sets message to "(unknown directory "")"; no file name!
      raise Ada.IO_Exceptions.Name_Error with "unknown directory '" & Path & "'";

   end Handle_Meta;

   function Handle_Put_Notes (Data : in String; Path : in String) return String
   is
      use Ada.Directories;
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      use SAL.Web_Utils;

      Filename : constant String := (-Server_Data) & Path;
      Pathname : constant String := Ada.Directories.Containing_Directory (Filename);
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

      return HTTP_Response (S200, Content_Text_Plain, "");
   exception
   when E : others =>
      return HTML_Response (S400, "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_Put_Notes;

   function Handle_Search
     (URI_Param : in SAL.Web_Utils.Parameter_Lists.Map;
      Query     : in String)
     return String
   --  Query is only used in error messages.
   is
      use Ada.Characters.Handling;
      use Ada.Directories;
      use Ada.Strings.Fixed;
      use Ada.Strings;
      use SAL.Web_Utils;

      DB : SMM.Database.Database;

      Search_Result_ID : constant String := "search_result";

      Response_1 : constant String := "<!DOCTYPE html>" & New_Line &
        "<html lang=""en"">" &
        "<meta http-equiv=""Content-Type"" content=""text/html; charset=utf-8"">" & New_Line &
        "<head>" & New_Line &
        "<script src=""/" & (-Server_Data) & "/songs.js""></script>" & New_Line &
        "<title>Stephe's music</title>" &
        "<link type=""text/css"" rel=""stylesheet"" href=""/" & (-Server_Data) & "/songs.css""/>" & New_Line &
        "</head>";

      Response_2 : constant String := "<div class=""tabbar"">" &
        "<button class=""tabbutton"" id=""general_search_button""" &
        " onclick=""SelectTab('general_search_button', 'general_search_tab', '" & Search_Result_ID &
        "')"">General Search</button>" &
        "<button class=""tabbutton"" id=""detailed_search_button""" &
        " onclick=""SelectTab('detailed_search_button', 'detailed_search_tab', '" & Search_Result_ID &
        "')"">Detailed Search</button>" &
        "</div>" & New_Line &
        "<div class=""tabcontent"" id=""general_search_tab""><form action=""/search"" method=get>" &
        "<input type=submit value=""Search"">" &
        "<input type=search autofocus name=""search"" value=""" & Get (URI_Param, "search") & """>" &
        "</form></div>" & New_Line &
        "<div class=""tabcontent"" id=""detailed_search_tab"">" &
        "<form action=""/search"" method=get><div class=""table"">" &
        "<div class=""row""><label>Title </label>" &
        "<input type=search name=""title"" value=""" & Get (URI_Param, "title") & """></div>" &
        "<div class=""row""><label>Artist </label>" &
        "<input type=search name=""artist"" value=""" & Get (URI_Param, "artist") & """></div>" &
        "<div class=""row""><label>Album </label>" &
        "<input type=search name=""album"" value=""" & Get (URI_Param, "album") & """></div>" &
        "<div class=""row""><label>Album Artist</label>" &
        "<input type=search name=""album_artist"" value=""" & Get (URI_Param, "album_artist") & """></div>" &
        "<div class=""row""><label>Composer</label>" &
        "<input type=search name=""composer"" value=""" & Get (URI_Param, "composer") & """></div>" &
        "<div class=""row""><label>Category </label>" &
        "<input type=search name=""category"" value=""" & Get (URI_Param, "category") & """></div>" &
        "</div><input type=submit value=""Search"">" &
        "</form></div><hr>" & New_Line;

      Response      : Unbounded_String;
      Current_Album : Unbounded_String;
      Album_ID      : Integer := 0;

      function Search_Result (I : in SMM.Database.Cursor) return String
      is
         use SMM.Database;

         Title_Row : constant Unbounded_String := +"<tr>" &
           "<td><a href=""/" & HTTP_Encode (I.File_Name) &
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
           "</tr>" & New_Line;

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
                 "<div class=""album_row""><a class=""text"" href=""search?album=" & HTTP_Encode
                   (I.Album) & """>" & I.Album & "</a>" &
                 "<div class=""text"">" & I.Album_Artist &
                 (if I.Year /= No_Year then Integer'Image (I.Year) else "") & "</div>";

               Meta : constant String_Lists.List := Meta_Files (Containing_Directory (I.File_Name));
            begin
               for File of Meta loop
                  if To_Lower (Extension (-File)) = "jpg" then
                     Album_Item := Album_Item & Server_Img
                       (-File, "album art", 100, 100, Class => "album_art_item");
                  end if;
               end loop;

               for File of Meta loop
                  if To_Lower (-File) = "liner_notes.pdf" then
                     Album_Item := Album_Item & SAL.Web_Utils.Local_Href
                       (-File, Server_Img_Set
                          (-Server_Data & "/liner_notes_icon", ".png", "liner notes",
                           Class => "album_art_item"));
                  end if;
               end loop;
               Album_Item := Album_Item & "</div>" & New_Line;

               Result := Result & Album_Item & "<table><tbody>" & Title_Row;
               return -Result;
            end;
         end if;
      end Search_Result;

      function To_SQL_Param (Param : in Parameter_Lists.Map) return SMM.Database.Field_Values
      is
         use Parameter_Lists;
      begin
         return Result : SMM.Database.Field_Values do
            for I in SMM.Database.Fields loop
               declare
                  Cur : constant Cursor := Param.Find (-SMM.Database.Field_Image (I));
               begin
                  if Cur /= No_Element then
                     Result (I) := +Element (Cur);
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
         return HTTP_Response (S200, Content_Text_HTML, -Response);

      elsif Exist (URI_Param, "search") or
        Exist (URI_Param, "title") or Exist (URI_Param, "artist") or Exist (URI_Param, "album") or
        Exist (URI_Param, "album_artist") or Exist (URI_Param, "category")
      then
         DB.Open (-DB_Filename);

         declare
            use SMM.Database;
            I                : Cursor;
            Button           : Unbounded_String;
            Tab              : Unbounded_String;
         begin
            if Exist (URI_Param, "search") then
               --  General search
               I      := DB.Find_Like (Decode_Plus (Get (URI_Param, "search")), Order_By => (Album, Track));
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
               return HTTP_Response (S200, Content_Text_HTML, -Response);
            end if;

            Response := Response & "<div id=""" & Search_Result_ID & """ class=""" & Search_Result_ID & """><ul>";
            loop
               exit when not I.Has_Element;
               Response := Response & Search_Result (I) & New_Line;

               I.Next;
            end loop;
         end; --  Free cursor

         --  Terminate last album title table and album item
         Response := Response & "</tbody></table></li><hr>";

         --  Terminate album list, search result scroll, body, doc.
         Response := Response & "</ul></div></body></html>";

         return HTTP_Response (S200, Content_Text_HTML, -Response);

      else
         return HTML_Response (S400, "invalid query params '" & Query & "'");
      end if;
   end Handle_Search;

   function Handle_Update
     (URI_Param : in SAL.Web_Utils.Parameter_Lists.Map;
      Query     : in String)
     return String
   is
      use SMM.Database;
      use SAL.Web_Utils;

      DB        : SMM.Database.Database;
      SQL_Param : SMM.Database.Field_Values;
      Key_Field : Unbounded_String;
      Ref       : Unbounded_String;
      Cancel    : Boolean := False;

      function Redirect_Search return String
      is
         use Header_Lists;
         Headers : Header_Lists.List;
      begin
         Headers.Append ((Status, +"back to search", S303));
         Headers.Append ((Location, Ref));
         Headers.Append ((Content_Type, +Content_Text_HTML));
         return HTTP_Response (Headers, "back to search");
      end Redirect_Search;
   begin
      if URI_Param.Is_Empty then
         return HTML_Response (S400, "invalid query params: '" & Query & "'");

      else
         --  From Emacs notes buffer page, query looks like
         --  'update?id=<id>&<field>=<data>'
         --  only update field if present.

         --  From Web search results page, query looks like
         --
         --  'update?ref=<search uri>&id=<id>&<field>=<data>'
         --
         --  Only update field if present. If a "cancel" param is present, don't update anything.

         if Exist (URI_Param, "id") then
            Key_Field := +"id";
         elsif Exist (URI_Param, "file") then
            Key_Field := +"file";
         else
            return HTML_Response (S400, "missing 'id' or 'file' param: '" & Query & "'");
         end if;

         for I in URI_Param.Iterate loop
            declare
               Field_Name : String renames Parameter_Lists.Key (I);
            begin
               if Field_Name = "cancel" then
                  Cancel := True;
               elsif Field_Name = -Key_Field then
                  null;
               elsif Field_Name = "ref" then
                  Ref := +Parameter_Lists.Element (I);
               elsif Valid_Field (Field_Name) then
                  null;
               else
                  return HTML_Response (S400, "bad param name: '" & Field_Name & "'");
               end if;
            end;
         end loop;

         if Cancel then
            if Length (Ref) = 0 then
               return HTML_Response (S200, "canceled");
            else
               return Redirect_Search;
            end if;
         end if;

         for I in Fields loop
            declare
               Value : constant String := Get (URI_Param, -Field_Image (I)); -- empty string if not present
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
               then DB.Find_ID (Integer'Value (Get (URI_Param, "id")))
               else DB.Find_File_Name (Get (URI_Param, "file")));
         begin
            if I.Has_Element then
               DB.Update (I, SQL_Param);
            else
               return HTML_Response
                 (S400, "not found in db: '" &
                    (if -Key_Field = "id"
                     then Get (URI_Param, "id")
                     else Get (URI_Param, "file"))
                    & "'");
            end if;
         end;

         if Length (Ref) = 0 then
            return HTML_Response (S200, "updated");
         else
            return Redirect_Search;
         end if;
      end if;
   end Handle_Update;

   ----------
   --  Top level

   function Handle_Request return String
   is
      use Ada.Exceptions;
      use SAL.Web_Utils;

      --  Handle a Common Gateway Interface request
      --  https://datatracker.ietf.org/doc/html/rfc3875
      --
      --  The full URI sent by the client looks like:
      --  https:/<host>/cgi-bin/smm-server_driver.exe/<path>?<query>

      Path   : constant String         := Ada.Environment_Variables.Value ("PATH_INFO");
      Query  : constant String         := Ada.Environment_Variables.Value ("QUERY_STRING");
      Method : constant Request_Method := Request_Method'Value (Ada.Environment_Variables.Value ("REQUEST_METHOD"));
   begin
      case Method is
      when GET =>
         declare
            URI_File   : constant String       := Ada.Directories.Simple_Name (Path);
            Parameters : constant Parameter_Lists.Map := Parse_Parameters (Query);
            API_String : constant String       := Get (Parameters, "API");
            API        : constant API_Versions :=
              (if API_String'Length = 0 then 1
               else API_Versions'Value (API_String));
         begin
            --  Simple file requests (mp3/m4a, liner_notes, image) are handled by
            --  the parent server, not here.

            if URI_File = "download" then
               --  API 1
               return Handle_Get_New_Songs_List (Parameters, API);

            elsif URI_File = "get_new_songs_list" then
               --  API 2
               return Handle_Get_New_Songs_List (Parameters, API);

            --  elsif URI_File = "favicon.ico" then
            --     --  FIXME: parent server handles this? it's a simple file request
            --     --  Rename app.ico to favicon.ico?
            --     return HTTP_response (S200, Content_Image_Icon, (-Server_Data) & "/app.ico");

            elsif URI_File = "field" then
               return Handle_Field (Parameters, Query);

            elsif URI_File = "id" then
               return Handle_ID (Parameters, Query);

            elsif URI_File = "meta" then
               --  Path looks like <album_artist>/<album>/meta
               return Handle_Meta (Path (Path'First .. Path'Last - 5));

            elsif URI_File = "search" then
               return Handle_Search (Parameters, Query);

            else
               return HTML_Response (S400, "invalid GET query '" & URI_File & "'");
            end if;
         end;

      when PUT =>
         declare
            Content_Length : constant Integer := Integer'Value (Ada.Environment_Variables.Value ("CONTENT_LENGTH"));
            Content : String (1 .. Content_Length);
         begin
            String'Read (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Input), Content);
            return Handle_Put_Notes (Content, Path);
         end;

      when POST =>
         declare
            URI_File : constant String := Ada.Directories.Simple_Name (Path);
         begin
            if URI_File = "update" then
               return Handle_Update (Parse_Parameters (Query), Query);
            else
               return HTML_Response (S400, "unrecognized POST path '" & URI_File & "'");
            end if;
         end;

      when others =>
         return HTML_Response (S400, "unrecognized request " & Request_Method'Image (Method));
      end case;
   exception
   when E : others =>
      return HTML_Response (S500, "exception " & Exception_Name (E) & ": " & Exception_Message (E));
   end Handle_Request;

   procedure Server
   is
      procedure Usage
      is
         use Ada.Text_IO;
      begin
         Put_Line ("usage: smm-server-driver [server config filename]");
         Put_Line ("config file defaults to /home/stephe/smm/music_server.config");
         Put_Line ("config file contains absolute paths:");
         Put_Line ("DB_Filename : database ");
         Put_Line ("Root : music files ");
         Put_Line ("Server_Data : other files (css, js, notes etc)");
      end Usage;
   begin
      declare
         use Ada.Command_Line;
         use SAL.Config_Files;

         Default : constant String := "/home/stephe/smm/music_server.Config";
         Config  : SAL.Config_Files.Configuration_Type;
      begin
         case Argument_Count is
         when 0 =>
            Open (Config, Default);

         when 1 =>
            Open (Config, Argument (1));

         when others =>
            Usage;
            Set_Exit_Status (Failure);
            raise SAL.Parameter_Error;

         end case;
         DB_Filename := +Read (Config, "DB_Filename", Missing_Key => Raise_Exception);

         Source_Root := +As_File
           (Ada.Directories.Full_Name (Read (Config, "Root", Missing_Key => Raise_Exception)));

         Server_Data := +Read (Config, "Server_Data", "server_data");

         Close (Config);
      end;
      declare
         Result : constant String := Handle_Request;
      begin
         String'Write (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output), Result);
      end;
   end Server;
end SMM.Server;
--  Local Variables:
--  ada-indent-comment-gnat: t
--  End:
