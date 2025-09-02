with Ada.Text_IO; use Ada.Text_IO;
with SAL.Web_Utils; use SAL.Web_Utils;
with SMM.Database;
procedure Debug
is
   Query : constant String := "search=Two+Worlds";
   URI_Param : constant Parameter_Lists.Map := Parse_Parameters (Query);
   DB : SMM.Database.Database;

begin

   Put_Line ("URI_Param:");
   for I in URI_Param.Iterate loop
      Put_Line
        (" '" & Parameter_Lists.Key (I) & ", " & Parameter_Lists.Element (I) & "'");
   end loop;

   DB.Open ("/Projects/music_server_data/smm.db");
   declare
      use SMM.Database;
      I : Cursor := DB.Find_Like (Decode_Plus (Get (URI_Param, "search")), Order_By => (Album, Track));
   begin
      if not I.Has_Element then
         Put_Line ("no matching entries found");
      else
         loop
            exit when not I.Has_Element;
            Put_Line (I.Artist & " " & I.Title);
            I.Next;
         end loop;
      end if;
   end;
end Debug;
