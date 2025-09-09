--  Abstract :
--
--  Utils for html parsing.
--
--  Copyright (C) 2018, 2025 Stephen Leake All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);
with Ada.Strings.Unbounded;
with HTML_Parse;
package HTML_Utils is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String renames
     Ada.Strings.Unbounded.To_String;

   procedure Parse_File (File_Name : in String; Tree : out HTML_Parse.HTML_Tree);

   function Concat_Text
     (Root : in HTML_Parse.P_Body_Node)
     return Ada.Strings.Unbounded.Unbounded_String;
   --  All contained text

   function Find_Node
     (Node         : in HTML_Parse.P_Body_Node;
      Target_Kind  : in HTML_Parse.HTML_kind;
      Target_Id    : in String  := "";
      Target_Class : in String  := "";
      Level        : in Integer := 0)
     return HTML_Parse.P_Body_Node;
   --  Search siblings of Node and their children for a node with Target_*. If not found,
   --  result is null.

end HTML_Utils;
