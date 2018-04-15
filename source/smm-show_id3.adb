--  Abstract :
--
--  Show all ID3 tags in a file

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with SMM.ID3;
procedure SMM.Show_ID3
is
   File_Name : constant String := Ada.Command_Line.Argument (1);
   File : SMM.ID3.File;

   Frames : SMM.ID3.Frame_Lists.List;
begin
   File.Open (File_Name);

   Frames := File.All_Frames;

   for Frame of Frames loop
      Put_Line (Frame.ID & " '" & (-Frame.Data) & "'");
   end loop;

   File.Close;
end SMM.Show_ID3;
