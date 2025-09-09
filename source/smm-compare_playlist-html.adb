--  Abstract :
--
--  Compare lists of songs between DB and Spotify
--
--  Copyright (C) 2025 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with Ada.Text_IO;
with HTML_Parse;
with HTML_Utils;
with SMM.Database;
procedure SMM.Compare_Playlist.HTML
  (DB            : in out SMM.Database.Database;
   Category      : in     String;
   HTML_Filename : in     String)
--  Category is the DB string identifying the playlist.
--
--  HTML_Filename is the name of the HTML file containing the equivalent list.
is
   HTML_Tree : Song_Name_Trees.Tree;

   procedure Read_HTML_Tree
   is
      use HTML_Utils, HTML_Parse;
      HTML              : HTML_Parse.HTML_Tree;
      Album_Artist_Node : HTML_Parse.P_Body_Node;
      Album_Node        : HTML_Parse.P_Body_Node;
      Title_Node        : HTML_Parse.P_Body_Node;
   begin
      if Verbosity >= 1 then
         Ada.Text_IO.Put_Line ("parse html file");
      end if;

      HTML_Utils.Parse_File (HTML_Filename, HTML);

      --  The HTML file is a list of {<album_artist> {<album> {<title>}}}

      if Verbosity >= 1 then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("build html tree");
      end if;

      Album_Artist_Node := Find_Node
        (HTML.Root, Target_Kind => li, Target_Class => "album_artist", Level => 0);

      Artist_Loop :
      loop
         exit Artist_Loop when Album_Artist_Node = null;

         if Verbosity >= 2 then
            Ada.Text_IO.Put_Line ("album_artist: " & (-Concat_Text (Album_Artist_Node)));
         end if;

         Album_Node := Find_Node
           (First_Child (Album_Artist_Node), Target_Kind => li, Target_Class => "album", Level => 1);

         Album_Loop :
         loop
            exit Album_Loop when Album_Node = null;

            if Verbosity >= 2 then
               Ada.Text_IO.Put_Line ("album: " & (-Concat_Text (Album_Node)));
            end if;

            Title_Node := Find_Node (Next_Sibling (Album_Node), Target_Kind => li, Level => 2);

            Title_Loop :
            loop
               exit Title_Loop when Title_Node = null;
               declare
                  Song : constant Song_Name :=
                    (Album_Artist => Concat_Text (Album_Artist_Node),
                     Album => Concat_Text (Album_Node),
                     Title => Concat_Text (Title_Node));
               begin
                  if Verbosity >= 2 then
                     Ada.Text_IO.Put_Line ("title: " & (-Concat_Text (Title_Node)));
                  end if;

                  HTML_Tree.Insert (Song);
               exception
               when SAL.Duplicate_Key =>
                  Ada.Text_IO.Put_Line ("Duplicate song in html: " & Image (Song));
               end;

               Title_Node := Find_Node (Next_Sibling (Title_Node), Target_Kind => li, Level => 2);
            end loop Title_Loop;

            Album_Node := Find_Node (Next_Sibling (Album_Node), Target_Kind => li, Target_Class => "album", Level => 1);
         end loop Album_Loop;

         Album_Artist_Node := Find_Node
           (Next_Sibling (Album_Artist_Node), Target_Kind => li, Target_Class => "album_artist", Level => 0);
      end loop Artist_Loop;
   end Read_HTML_Tree;

begin
   Read_HTML_Tree;
   Compare_To_DB
     (DB,
      Category  => Category,
      Tree      => HTML_Tree,
      Tree_Name => "HTML",
      Missing   => Song_Name_Trees.Empty_Tree);
end SMM.Compare_Playlist.HTML;
