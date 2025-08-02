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

with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with HTML_Parse;
with HTML_Utils;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
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
   DB_Tree   : Song_Name_Trees.Tree;

   procedure Read_HTML_Tree
   is
      use HTML_Utils;
      HTML              : HTML_Parse.HTML_Tree;
      Album_Artist_Node : HTML_Parse.P_Body_Node;
      Title_Node        : HTML_Parse.P_Body_Node;

      function Get_Album_Artist (Root : in HTML_Parse.P_Body_Node) return String
      is
         --  Root is:
         --  <li>name<ul>... ; return name
         --  <li><a href="">name</a><ul>... ; return name
         --  <li><a href="">name_1</a>.<a href="">name_2</a><ul>... ; return name_1/name_2

         Node : constant P_Body_Node := First_Child (Root);
      begin
         case Kind (Node) is
         when a =>
            declare
               Name_1      : constant String      := -Text (First_Child (Node));
               Sep_Node    : constant P_Body_Node := Next_Sibling (Node);
               Name_2_Node : P_Body_Node;
            begin
               if Sep_Node /= null and then Kind (Sep_Node) = body_text then
                  Name_2_Node := Next_Sibling (Sep_Node);
                  if Name_2_Node /= null and then Kind (Name_2_Node) = a then
                     return Name_1 & (-Text (Sep_Node, Trim_Blank => False)) & (-Text (First_Child (Name_2_Node)));
                  else
                     return Name_1;
                  end if;
               else
                  return Name_1;
               end if;
            end;

         when body_text =>
            return -Text (Node);

         when others =>
            raise Programmer_Error;
         end case;
      end Get_Album_Artist;

   begin
      HTML_Utils.Parse_File (HTML_Filename, HTML);

      --  The HTML file is a list of <album_artist>, each containing a list
      --  of songs, with no albums.

      Album_Artist_Node := Find_Node (HTML.Root, Target_Kind => li);
      Artist_Loop :
      loop
         declare
            HTML_Album_Artist : constant String := ;
         begin
            HTML_Tree.Insert
(Song_Name'(Album_Artist => Get_Album_Artist (Album_Artist_Node),

            if DB_Album_Artist /= HTML_Album_Artist then
               declare
                  Next_HTML : constant P_Body_Node := Find_Node (Next_Sibling (Album_Artist_Node), Target_Kind => li);

                  Cur_DB_Row          : constant Positive := DB_I.Row;
                  Cur_DB_Album_Artist : constant String   := Get_Album_Artist (DB_I);
                  Next_DB_Row         : Positive;
               begin
                  Next_Album_Artist (DB_I);

                  if Next_HTML /= null and then Cur_DB_Album_Artist = Get_Album_Artist (Next_HTML) then
                     Put_Line
                       ("new in html: '" & Get_Album_Artist (Album_Artist_Node) & "' (before " &
                          Cur_DB_Album_Artist & ")");
                     Album_Artist_Node := Next_HTML;
                     DB_I.Set_Row (Cur_DB_Row);

                  elsif Get_Album_Artist (DB_I) = Get_Album_Artist (Album_Artist_Node) then
                     Next_DB_Row := DB_I.Row;
                     DB_I.Set_Row (Cur_DB_Row);
                     loop
                        Put_Line
                          ("new in db: '" & Get_Album_Artist (DB_I) &
                             "' album '" & DB_I.Album & "'" &
                             "' title '" & DB_I.Title & "' (before " &
                             Get_Album_Artist (Album_Artist_Node) & ")");
                        DB_I.Next;
                        exit when DB_I.Row = Next_DB_Row;
                     end loop;
                  else
                     DB_I.Set_Row (Cur_DB_Row);
                     Put_Line
                       ("giving up: db '" & Cur_DB_Album_Artist & "' html '" &
                          Get_Album_Artist (Album_Artist_Node) & "'");
                     exit Artist_Loop;
                  end if;
               end;
            else
               if Debug then
                  Put_Line ("ok: '" & DB_Album_Artist & "' = '" & HTML_Album_Artist & "'");
               end if;
            end if;
         end;

         Title_Node := Find_Node (First_Child (Album_Artist_Node), Target_Kind => li);

         if Title_Node /= null then
            Title_Loop :
            loop
               if Get_Album_Artist (DB_I) = Get_Album_Artist (Album_Artist_Node) then
                  declare
                     DB_Title   : constant String := DB_I.Title;
                     HTML_Title : constant String := -Concat_Text (Title_Node);
                  begin
                     if DB_Title = HTML_Title then
                        if Debug then
                           Put_Line ("ok: '" & DB_Title & "' = '" & HTML_Title & "'");
                        end if;
                        DB_I.Next;
                        Title_Node := Find_Node (Next_Sibling (Title_Node), Target_Kind => li);
                     else
                        declare
                           --  We can't keep both "current" and "new" db cursors here, so we have
                           --  to be careful when we call db_i.next.
                           Prev_DB_I_Title : constant String := DB_I.Title;
                           Prev_DB_I_Album : constant String := DB_I.Album;
                           Next_Title_Node : constant P_Body_Node := Find_Node
                             (Next_Sibling (Title_Node), Target_Kind => li);
                        begin
                           if DB_I.Title = -Concat_Text (Next_Title_Node) then
                              Put_Line
                                ("extra in html: '" & Get_Album_Artist (Album_Artist_Node) &
                                   "' title '" & (-Concat_Text (Title_Node)) & "'");
                              Put_Line ("db at: " & Get_Album_Artist (DB_I) & "/" & Db_Title);
                              Title_Node := Next_Title_Node;
                              goto Next;
                           end if;

                           DB_I.Next;
                           if DB_I.Title = -Concat_Text (Title_Node) then
                              Put_Line
                                ("new in db: '" & Get_Album_Artist (DB_I) &
                                   "' album '" & Prev_DB_I_Album &
                                   "' title '" & Prev_DB_I_Title & "'");
                              Put_Line
                                ("html at: " & Get_Album_Artist (Album_Artist_Node) &
                                   "/" & (-Concat_Text (Title_Node)));

                           else
                              Put_Line ("mismatch title");
                              Put_Line ("db at  : " & Get_Album_Artist (DB_I) & "/" & DB_I.Title);
                              Put_Line ("html at: " & Get_Album_Artist (Album_Artist_Node) & "/"
                                          (-Concat_Text (Title_Node)) & "'");
                              Title_Node := Next_Title_Node;
                           end if;
                           <<Next>>
                        end;
                     end if;
                  end;
                  exit Title_Loop when (not DB_I.Has_Element) and Title_Node = null;

                  if not DB_I.Has_Element then
                     --  extra titles in html
                     loop
                        Put_Line ("extra html title '" & (-Concat_Text (Title_Node)) & "'");
                        Title_Node := Find_Node (Next_Sibling (Title_Node), Target_Kind => li);
                        exit when Title_Node = null;
                     end loop;
                     exit Title_Loop;
                  end if;

                  if Title_Node = null then
                     --  extra titles in db
                     if Get_Album_Artist (DB_I) = Get_Album_Artist (Album_Artist_Node) then
                        loop
                           Put_Line
                             ("new in db: '" & Get_Album_Artist (DB_I) &
                                "' album '" & DB_I.Album & "'" &
                                "' title '" & DB_I.Title & "'");
                           DB_I.Next;
                           exit Title_Loop when not DB_I.Has_Element;
                           exit Title_Loop when Get_Album_Artist (DB_I) /= Get_Album_Artist (Album_Artist_Node);
                        end loop;
                     end if;
                     exit Title_Loop;
                  end if;
               else
                  Put_Line
                    ("mismatch album artist: db '" & Get_Album_Artist (DB_I) & "' html '" &
                       Get_Album_Artist (Album_Artist_Node) & "'");
                  exit Title_Loop;
               end if;
            end loop Title_Loop;
         end if;
         Album_Artist_Node := Find_Node (Next_Sibling (Album_Artist_Node), Target_Kind => li);
         exit Artist_Loop when not DB_I.Has_Element;
         exit Artist_Loop when Album_Artist_Node = null;
      end loop Artist_Loop;
   end Read_HTML_Tree;

begin
   Read_HTML_Tree;
   Read_DB_Tree (DB, DB_Tree);
   Compare_Trees (DB_Tree, "DB", HTML_Tree, "HTML");
end SMM.Compare_Playlist.Spotify;
