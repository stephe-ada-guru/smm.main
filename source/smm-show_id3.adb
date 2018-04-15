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

   Tags : SMM.ID3.Tag_Lists.List;
begin
   File.Open (File_Name);

   Tags := File.All_Tags;

   for Tag of Tags loop
      Put_Line ("ID '" & Tag.ID & "'");
      --  some tag values are binary, so don't print any here
      --  FIXME: whitelist of tags known to be text and short?
   end loop;

   File.Close;
end SMM.Show_ID3;
