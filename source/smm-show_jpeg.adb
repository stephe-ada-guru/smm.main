--  Abstract :
--
--  Show pixel size of a jpeg file

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with SMM.JPEG; use SMM.JPEG;
procedure SMM.Show_JPEG
is
   File_Name : constant String := Ada.Command_Line.Argument (1);
   File : SMM.JPEG.File;

begin
   File.Open (File_Name);

   Put_Line ("size '" & Image (Size (File)) & "'");

   File.Close;
end SMM.Show_JPEG;
