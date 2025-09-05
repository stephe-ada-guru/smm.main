with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Gen_Definite_Doubly_Linked_Lists;
with SAL.Web_Utils; use SAL.Web_Utils;
with SMM.Database;
procedure Debug
is
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Query : constant String := "title=&artist=&album=25+Disc+1&album_artist=Patty+Larkin&composer=&category=";
   URI_Param : constant Parameter_Lists.Map := Parse_Parameters (Query);

   DB : SMM.Database.Database;
begin
   Put_Line ("URI_Param:");
   for I in URI_Param.Iterate loop
      Put_Line
        (Parameter_Lists.Key (I) & " => " & Parameter_Lists.Element (I));
   end loop;

   DB.Open ("/Projects/music_server_data/smm.db");
   declare
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
                     Result (I) := +Decode_Plus (Element (Cur));
                  end if;
               end;
            end loop;
         end return;
      end To_SQL_Param;

      use SMM.Database;

      Search : constant SMM.Database.Field_Values := To_SQL_Param (URI_Param);
      I : constant Cursor := DB.Find_Like (Search, Order_By => (Album, Track));
   begin
      Put_Line ("Search param:");
      for F in Fields loop
         Put_Line (F'Img & " => " & (-Search (F)));
      end loop;

      if Has_Element (I) then
         Put_Line ("file " & I.File_Name);

         declare
            package String_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Ada.Strings.Unbounded.Unbounded_String);
            function Meta_Files (Source_Dir : in String) return String_Lists.List
            is
               use Ada.Directories;

               Result : String_Lists.List;

               Source_Root : constant Ada.Strings.Unbounded.Unbounded_String := +"/Projects/Music";

               procedure Copy_Aux (Dir_Ent : in Directory_Entry_Type)
               is
                  Path_Name : constant String := Relative_Name (-Source_Root, Normalize (Full_Name (Dir_Ent)));
               begin
                  Result.Append (+Path_Name);
               end Copy_Aux;
            begin
               Ada.Directories.Search
                 (Directory => -Source_Root & "/" & Source_Dir,
                  Pattern   => "*.jpg",
                  Filter    => (Ordinary_File => True, others => False),
                  Process   => Copy_Aux'Access);

               Ada.Directories.Search
                 (Directory => -Source_Root & "/" & Source_Dir,
                  Pattern   => "*.png",
                  Filter    => (Ordinary_File => True, others => False),
                  Process   => Copy_Aux'Access);

               Ada.Directories.Search
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

            Meta : constant String_Lists.List := Meta_Files (Ada.Directories.Containing_Directory (I.File_Name));
         begin
            Put_Line ("Meta files:");
            for File of Meta loop
               Put_Line (-File);
            end loop;
         end;

      else
         Put_Line ("not found");
      end if;
   end;

end Debug;
