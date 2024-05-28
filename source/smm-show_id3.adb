--  Abstract :
--
--  Show all ID3 tags in a file

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with SMM.ID3;
with SMM.Metadata;
procedure SMM.Show_ID3
is
   File_Name : constant String := Ada.Command_Line.Argument (1);

   Frames    : SMM.Metadata.Frame_Lists.List;
   Artist_ID : SMM.Metadata.ID_String;
begin
   if Ada.Command_Line.Argument_Count > 1 then
      SMM.ID3.Ignore_Flags := True;
   end if;

   SMM.ID3.Metadata (File_Name, Frames, Artist_ID);

   for Frame of Frames loop
      Put_Line (Frame.ID & " '" & (-Frame.Data) & "'");
   end loop;

end SMM.Show_ID3;
