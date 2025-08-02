--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019 Stephen Leake All Rights Reserved.
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
with Ada.Text_IO;
with GNATCOLL.Mmap;
package body HTML_Utils is

   procedure Parse_File (File_Name : in String; Tree : out HTML_Parse.HTML_Tree)
   is
      use GNATCOLL.Mmap;
      use Ada.Strings.Unbounded;
      File   : Mapped_File   := Open_Read (File_Name);
      Region : Mapped_Region := Read (File);
   begin
      HTML_Parse.Parse (To_Unbounded_String ("file://") & File_Name, String (Data (Region)(1 .. Last (Region))), Tree);
      Free (Region);
      Close (File);
   end Parse_File;

   function Concat_Text
     (Root : in HTML_Parse.P_Body_Node)
     return Ada.Strings.Unbounded.Unbounded_String
   is
      use Ada.Strings.Unbounded;
      use HTML_Parse;

      Result     : Unbounded_String;
      Need_Space : Boolean     := False;
      Node       : P_Body_Node :=
        (if Root = null then Root else
           (case Kind (Root) is
            when Body_bracketing_tag => First_Child (Root),
            when others => Root));
   begin
      loop
         exit when Node = null;

         case Kind (Node) is
         when body_text =>
            declare
               Txt : constant Unbounded_String := Text (Node);
            begin
               if Length (Txt) > 0 then
                  Result := Result & (if Need_Space then " " else "") & Txt;
                  Need_Space := True;
               end if;
            end;

         when a | Span =>
            declare
               Txt : constant Unbounded_String := Concat_Text (Node);
            begin
               if Length (Txt) > 0 then
                  Result := Result & (if Need_Space then " " else "") & Txt;
                  Need_Space := True;
               end if;
            end;

         when others =>
            null;
         end case;

         Node := Next_Sibling (Node);
      end loop;
      return Result;
   end Concat_Text;

   function Find_Node
     (Node         : in HTML_Parse.P_Body_Node;
      Target_Kind  : in HTML_Parse.HTML_kind;
      Target_Id    : in String  := "";
      Target_Class : in String  := "";
      Level        : in Integer := 0)
     return HTML_Parse.P_Body_Node
   is
      use HTML_Parse;
      Result : P_Body_Node := Node;

      Child_Result : P_Body_Node;
   begin
      loop
         if Verbosity > 0 then
            Ada.Text_IO.Put_Line (Integer'Image (Level) & ' ' & Image (Result));
         end if;
         exit when Result = null;
         exit when Kind (Result) = Target_Kind and
           (Target_Id'Length = 0 or Target_Id = Id (Result)) and
           (Target_Class'Length = 0 or Target_Class = Class (Result));

         if Kind (Result) in Body_bracketing_tag and then First_Child (Result) /= null then
            Child_Result := Find_Node (First_Child (Result), Target_Kind, Target_Id, Target_Class, Level + 1);
            if Child_Result /= null then
               return Child_Result;
            end if;
         end if;

         Result := Next_Sibling (Result);
      end loop;
      return Result;
   end Find_Node;

end HTML_Utils;
