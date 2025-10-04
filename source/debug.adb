with Ada.Text_IO; use Ada.Text_IO;
with SMM; use SMM;
procedure Debug
is
   Spotify : constant Song_Name :=
     (+"David Wilcox",
      +"Blaze",
      +"Single Candle");

   DB : constant Song_Name :=
     (+"David Wilcox",
      +"blaze",
      +"Single Candle");

begin
   Put_Line (Boolean'Image (Spotify = DB));
end Debug;
