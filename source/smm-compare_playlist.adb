--  Abstract :
--
--  See spec.
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
package body SMM.Compare_Playlist is

   function Image (Item : in SMM.Database.Cursor) return String
   is begin
      return Item.Album_Artist & ", " & Item.Album & ", " & Item.Title;
   end Image;

   procedure DB_Next (DB_I : in out SMM.Database.Cursor; Category : in String)
   --  Increment DB_I to next item containing Category
   is
      use SMM.Database;
   begin
      loop
         Next (DB_I);
         exit when not Has_Element (DB_I);
         exit when DB_I.Category_Contains (Category);
      end loop;
   end DB_Next;

   procedure Compare_To_DB
     (DB        : in SMM.Database.Database;
      Category  : in String;
      Tree      : in Song_Name_Trees.Tree;
      Tree_Name : in String;
      Missing   : in Song_Name_Trees.Tree)
   is
      use Song_Name_Trees, SMM.Database, Ada.Text_IO;

      DB_I          : SMM.Database.Cursor    := First_By_Name (DB);
      Tree_Iterator : constant Iterator      := Iterate (Tree);
      Tree_I        : Song_Name_Trees.Cursor := First (Tree_Iterator);

      Next_DB_I   : SMM.Database.Cursor := First_By_Name (DB); -- Independent of DB_I
      Next_Tree_I : Song_Name_Trees.Cursor;

      Error_Count : Integer := 0;
   begin
      if Verbosity >= 2 then
         Put_Line ("db best list, sorted:");
         loop
            exit when not Has_Element (DB_I);
            Put_Line (Image (DB_I.Song_Name));
            DB_Next (DB_I, Category);
         end loop;
         DB_I := First_By_Name (DB);

         New_Line;
         Put_Line (Tree_Name & " best list, sorted:");
         for Song of Tree loop
            Put_Line (Image (Song));
         end loop;
         New_Line;
      end if;

      loop
         exit when not Has_Element (DB_I) or not Has_Element (Tree_I);
         exit when Max_Errors > 0 and then Error_Count >= Max_Errors;

         if Verbosity >= 2 then
            Put_Line ("db at  : " & Image (DB_I.Song_Name));
            Put_Line (Tree_Name & " at: " & Image (Element (Tree_I)));
         end if;

         if DB_I.Song_Name = Element (Tree_I) then
            DB_Next (DB_I, Category);
            DB_Next (Next_DB_I, Category);
            Tree_I := Next (Tree_Iterator, Tree_I);
         else
            --  Could be new, deleted, or missing on either side. We assume there
            --  is only one consecutive difference.

            DB_Next (Next_DB_I, Category);
            Next_Tree_I := Next (Tree_Iterator, Tree_I);

            if Has_Element (Next_DB_I) and then Next_DB_I.Song_Name = Element (Tree_I) then
               --  New in DB:
               --       DB  Tree
               --  prev A   A
               --  I    new B
               --  next B
               --
               --  Deleted or missing in Tree:
               --       DB   Tree
               --  prev A    A
               --  I    B    C
               --  next C
               if Contains (Missing, DB_I.Song_Name) then
                  --  Ignore
                  null;

               elsif DB_I.Last_Downloaded /= Default_Time_String then
                  Error_Count := @ + 1;
                  Put_Line ("deleted or missing in " & Tree_Name & ": " & Image (DB_I.Song_Name));
               else
                  Error_Count := @ + 1;
                  Put_Line ("either new in db or deleted in " & Tree_Name & ": " & Image (DB_I.Song_Name));
               end if;

               DB_Next (DB_I, Category);

            elsif Has_Element (Next_Tree_I) and then DB_I.Song_Name = Element (Next_Tree_I) then
               --  New in Tree:
               --       DB  Tree
               --  prev A   A
               --  I    B   new
               --  next     B
               --
               --  Deleted in DB:
               --       DB   Tree
               --  prev A    A
               --  I    C    B
               --  next      C
               Error_Count := @ + 1;
               Put_Line ("either new in " & Tree_Name & " or deleted in db: " & Image (Element (Tree_I)));
               Tree_I := Next_Tree_I;
            else
               --  Probably just spelled differently:
               --  db at  : Abby Newton, Castles, Kirks, and Caves, A Hero Never Dies/Willie's Auld Trews
               --  HTML at: Abby Newton,                          , A Hero Never Dies / Willies Auld Trews
               Error_Count := @ + 1;

               Put_Line ("db at  : " & Image (DB_I.Song_Name));
               Put_Line (Tree_Name & " at: " & Image (Element (Tree_I)));
               Put_Line ("spelled differently?");

               DB_Next (DB_I, Category);
               DB_Next (Next_DB_I, Category);
               Tree_I := Next (Tree_Iterator, Tree_I);
            end if;
         end if;
      end loop;

      if Max_Errors > 0 and then Error_Count >= Max_Errors then
         Put_Line ("stopped at max errors");
      else
         if Has_Element (DB_I) then
            Put_Line ("extra db items:");
            loop
               exit when not Has_Element (DB_I);
               exit when Max_Errors > 0 and then Error_Count >= Max_Errors;
               Put_Line (Image (DB_I));
               DB_Next (DB_I, Category);
            end loop;
         end if;

         if Has_Element (Tree_I) then
            Put_Line ("extra " & Tree_Name & " items:");
            loop
               exit when not Has_Element (Tree_I);
               exit when Max_Errors > 0 and then Error_Count >= Max_Errors;
               Put_Line (Image (Element (Tree_I)));
               Tree_I := Next (Tree_Iterator, Tree_I);
            end loop;
         end if;
      end if;
   end Compare_To_DB;

end SMM.Compare_Playlist;
