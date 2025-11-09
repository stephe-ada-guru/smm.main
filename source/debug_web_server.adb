with Ada.Command_Line; use Ada.Command_Line;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with SMM.Server; use SMM.Server;
procedure Debug_Web_Server
is
   Method : constant String := Argument (1);
   Path   : constant String := Argument (2);
   Query  : constant String := Argument (3);
begin
   if Path'Length > 0 then
      Set ("PATH_INFO", Path);
   end if;

   Set ("QUERY_STRING", Query);
   Set ("REQUEST_METHOD", Method);

   Put_Line (Handle_Request);
end Debug_Web_Server;
