--  Abstract :
--
--  Show all ID3 tags in a file

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
procedure SMM.M4a.Show
is
   File_Name : constant String := Ada.Command_Line.Argument (1);

   Frames : SMM.Metadata.Frame_Lists.List;
   Artist_ID : SMM.Metadata.ID_String;
begin
   SMM.M4a.Metadata (File_Name, Frames, Artist_ID);

   for Frame of Frames loop
      Put_Line (Frame.ID & " '" & (-Frame.Data) & "'");
   end loop;
end SMM.M4a.Show;
