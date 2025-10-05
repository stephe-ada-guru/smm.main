with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Web_Utils; use SAL.Web_Utils;
with SMM.Server; use SMM.Server;
procedure Debug_Web_Server
is

   --  URI_File   : constant String       := "get_new_songs_list";
   Parameters : constant Parameter_Lists.Map := Parse_Parameters
     ("API=2&category=instrumental&count=88&new_count=29&over_select_ratio=1.1&record_downloaded=true");
   API_String : constant String       := Get (Parameters, "API");
   API        : constant API_Versions :=
     (if API_String'Length = 0 then 1
      else API_Versions'Value (API_String));
begin
   DB_Filename := Ada.Strings.Unbounded.To_Unbounded_String ("/Projects/music_server_data/smm.db");
   declare
      Response : constant String := Handle_Get_New_Songs_List (Parameters, API);
   begin
      Put_Line (Response);
   end;
end Debug_Web_Server;
