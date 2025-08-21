with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;
with SMM.Database;
procedure Debug
is
   DB_File_Name : constant String := Argument (1);
   Category     : constant String := Argument (2);
   DB           : SMM.Database.Database;
   DB_I         : SMM.Database.Cursor;
   --  Count        : Integer         := 0;
begin
   DB.Open (DB_File_Name);
   DB_I := SMM.Database.First_By_Last_Downloaded (DB); -- oldest date

   loop
--      exit when Count > 10;
      exit when not DB_I.Has_Element;
      if DB_I.Category_Contains (Category) and
        (not DB_I.Category_Contains ("dont_play")) and
        (not DB_I.Play_After_Is_Present) -- only play this when Play_Before is included.
      then
         Ada.Text_IO.Put_Line (DB_I.ID'Image & " " & DB_I.Last_Downloaded & " " & DB_I.File_Name);
      end if;
      DB_I.Next;
   end loop;

   --  DB_I := SMM.Database.Last_By_Last_Downloaded (DB); -- newest date
   --  Count := 0;
   --  loop
   --     exit when Count > 10;
   --     if DB_I.Category_Contains (Category) and
   --       (not DB_I.Category_Contains ("dont_play")) and
   --       (not DB_I.Play_After_Is_Present) -- only play this when Play_Before is included.
   --     then
   --        Count := @ + 1;
   --        Ada.Text_IO.Put_Line ("newest:" & DB_I.ID'Image & " " & DB_I.Last_Downloaded & " " & DB_I.File_Name);
   --     end if;
   --     DB_I.Next;
   --  end loop;
end Debug;
