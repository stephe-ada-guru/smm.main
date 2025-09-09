with Ada.Text_IO; use Ada.Text_IO;
with HTML_Parse; use HTML_Parse;
with HTML_Utils; use HTML_Utils;
procedure Debug
is
   HTML : HTML_Tree;
   Node : P_Body_Node;
begin
   HTML_Parse.Verbosity := 2;

   HTML_Utils.Parse_File ("debug.html", HTML);
   Node := Find_Node (HTML.Root, Target_Kind => li);
   Put_Line ("album_artist: '" & (-Concat_Text (Node)) & "'");

   Node := Find_Node
     (First_Child (Node), Target_Kind => li, Target_Class => "album", Level => 1);
   Put_Line ("album: '" & (-Concat_Text (Node)) & "'");

   Node := Find_Node
     (Next_Sibling (Node), Target_Kind => li, Level => 2);
   Put_Line ("Title: '" & (-Concat_Text (Node)) & "'");

end Debug;
