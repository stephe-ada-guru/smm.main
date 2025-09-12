--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2019, 2025 Stephen Leake All Rights Reserved.
--
--  copied from https://sourceforge.net/projects/wasabee/ zrt_dev branch
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body HTML_Parse is

   function "-" (Source : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
   function "+" (Source : String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   generic
      type Enum is (<>);
   procedure Gen_To_Enum_proc (key : String; variable : in out Enum);

   procedure Gen_To_Enum_proc (key : String; variable : in out Enum) is
      s : String := key;
      old : Enum;
   begin
      for i in s'Range loop
         if s (i) = '-' then s (i) := '_'; end if;
      end loop;
      old := variable;
      variable := Enum'Value (s);
   exception
   when Constraint_Error =>
      variable := old;  --  keep existing value in variable
   end Gen_To_Enum_proc;

   procedure To_Encoding is new Gen_To_Enum_proc (Encoding_choice);

   procedure Parse_encoding (content_str : String; encoding : in out Encoding_choice) is
      n : constant Natural := Ada.Strings.Fixed.Index (content_str, "charset=");
   begin
      if n > 0 then
         To_Encoding (content_str (n + 8 .. content_str'Last), encoding);
      end if;
   end Parse_encoding;

   --  List of Attributes used in parsing HTML element.
   --  This is an absolutely dumb alphabetical list; its purpose
   --  is only to save tedious string comparisons.

   type HTML_Attribute is
     (Charset, Class, Content,
      href,
      id,
      name,
      src,
      Zzz_Unknown_Attribute);

   function Get_Attribute (s : String) return HTML_Attribute
   is begin
      return HTML_Attribute'Value (s);
   exception
   when Constraint_Error =>
      return Zzz_Unknown_Attribute;
   end Get_Attribute;

   type Location_type is (nowhere, in_head, in_body);

   function Identify_tag (s : String) return HTML_kind is
      kind : HTML_kind;
   begin
      if s = "!--" then
         kind := comment;
      elsif Ada.Characters.Handling.To_Upper (s) = "BODY" then  --  Ada keyword
         kind := b0dy;
      else
         begin
            kind := HTML_kind'Value (s);
         exception
         when Constraint_Error =>
            kind := Unknown_Tag;
         end;
      end if;
      if Verbosity > 0 then
         Ada.Text_IO.Put ("Tag: [" & s & ',' & HTML_kind'Image (kind) & ']');
      end if;
      return kind;
   end Identify_tag;

   Alpha_attrib_set : constant array (Character) of Boolean :=
     ('a' .. 'z' | 'A' .. 'Z' | '-' | ':' => True, others => False);

   Alphanum_tag_set : constant array (Character) of Boolean :=
     ('a' .. 'z' |
        'A' .. 'Z' |
        '0' .. '9' |
        '!' | '-' |  -- This is for spotting comments: "<!--"
        ':'          -- This is only for <g:plusone> (Google's "+1" button) !
        => True,
      others => False);

   Bracketing_tag_set : constant array (HTML_kind) of Boolean :=
     (Univ_Bracketing_tag |
        Head_bracketing_tag |
        Body_bracketing_tag
        => True,
      others => False);

   Formatting : constant array (Character) of Boolean :=
     (Character'Val (9) | Character'Val (10) | Character'Val (13) => True,
      others => False);

   function All_Blank (S : in String) return Boolean
   is begin
      for C of S loop
         if C /= ' ' and not Formatting (C) then return False; end if;
      end loop;
      return True;
   end All_Blank;

   function Trim_Blanks (S : String; Trim_Space : in Boolean; Trim_Formatting : in Boolean) return String
   is
      T : String (S'Range);
      J : Integer := T'First - 1;
   begin
      for I in S'Range loop
         if (Trim_Space and S (I) = ' ') or
           (Trim_Formatting and Formatting (S (I)))
         then
               null;  --  Skip
         else
            J := J + 1;
            T (J) := S (I);
         end if;
      end loop;
      return T (T'First .. J);
   end Trim_Blanks;

   function Simplify_URL (s : String) return String is
      s2 : constant String (s'First .. s'Last + 2) := s & "XX";
      i : Positive := s'First;
      t : String (s'Range) := s;
      j : Natural := t'First - 1;
   begin
      if s'Length >= 7 and then s (s'First .. s'First + 6) = "file://" then
         return s;
      end if;
      while i <= s'Last loop
         if s2 (i .. i + 1) = "./" then
            i := i + 2; -- skip the "./"
         elsif s2 (i .. i + 2) = "../" then
            if j < t'First or else t (j) /= '/' then
               null; -- "../" is misplaced
            else
               j := j - 1; -- "delete" the '/' in t
               while j >= t'First and then t (j) /= '/' loop
                  j := j - 1;
               end loop;
               i := i + 3; -- skip the "../"
            end if;
         else
            j := j + 1;
            t (j) := s (i);
            i := i + 1;
         end if;
      end loop;
      return t (t'First .. j);
   end Simplify_URL;

   Build_URL_Error : exception;

   function Build_URL (complete_URL, partial_URL : String) return String
   is
      use Ada.Strings.Fixed;
      first_slash : Integer := complete_URL'Last + 1;   -- first /
      last_slash : Integer := complete_URL'First - 1;  -- last /
      protocol   : Integer := complete_URL'First - 1;  -- //
   begin
      if partial_URL'Length = 0 then
         return complete_URL; -- partial URL is empty: well, use the complete one...
      elsif Index (partial_URL, "://") > 0 then
         return partial_URL;  -- partial URL is actually a complete one: use it!
      end if;
      for i in complete_URL'Range loop
         if complete_URL (i) = '/' then
            if last_slash = i - 1 and protocol < complete_URL'First then
               protocol := last_slash; -- capture the first //
            end if;
            last_slash := i;
         end if;
      end loop;
      if protocol < complete_URL'First then
         raise Build_URL_Error with
           "In complete_URL, the string ""//"" is missing, something like ""http://"". " &
           "complete_URL=" & complete_URL;
      end if;
      if Index (partial_URL, "//") = partial_URL'First then
         --  case [1]: absolute-with-website path given as the partial URL
         return complete_URL (complete_URL'First .. protocol - 1) & partial_URL;
      end if;
      for i in reverse protocol + 2 .. complete_URL'Last loop
         if complete_URL (i) = '/' then
            first_slash := i;
         end if;
      end loop;
      case partial_URL (partial_URL'First) is
      when '/' =>
         --  case [2]: absolute path given as the partial URL
         return complete_URL (complete_URL'First .. first_slash - 1) & partial_URL;
      when '#' =>
         --  case [3]: partial URL is just an anchor
         return complete_URL & partial_URL;
      when others =>
         --  case [4]: relative path
         return complete_URL (complete_URL'First .. last_slash) & partial_URL;
      end case;
   end Build_URL;

   function Build_URL (ho : HTML_Tree; partial_URL : String) return String
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if ho.base_URL = "" then
         return Build_URL (-ho.own_URL, partial_URL);
      else
         return Build_URL (-ho.base_URL, partial_URL);
      end if;
   end Build_URL;

   overriding
   procedure Finalize (ho : in out HTML_Tree)
   is
      procedure Delete_body (bn : in out P_Body_Node)
      is
         procedure Dispose is new Ada.Unchecked_Deallocation (Body_Node, P_Body_Node);
      begin
         if bn = null then
            return;
         end if;
         case bn.Kind is
         when Body_text_or_singleton_tag =>
            null;
         when Body_bracketing_tag =>
            Delete_body (bn.first_child);
         end case;
         Delete_body (bn.next);
         Dispose (bn);
      end Delete_body;
   begin
      Delete_body (ho.the_body);
   end Finalize;

   --  Translate &nbsp; &lt; &gt; &amp; and so on.
   --  http://dev.w3.org/html5/html-author/charref
   --  http://www.w3.org/TR/html4/sgml/entities.html

   type Entity is
     (nbsp, thinsp, -- spaces
      lt, gt, amp, quot,
      ndash
     );

   --  FIXME: use correct Unicode value, return utf-8
   Conversion : constant array (Entity) of Character :=
     (nbsp     => ' ',  --  non-breaking space
      thinsp   => ' ', --  thin space
      lt       => '<',
      gt       => '>',
      amp      => '&',
      quot     => '"',
      ndash    => '-');

   function Convert_Entity (E : String) return String
   is
      N : Positive;
   begin
      if E'Length >= 2 and then E (E'First) = '#' then
         --  &#entity_number;
         if E (E'First + 1) = 'x' then
            --  hexadecimal
            N := Integer'Value ("16#" & E (E'First + 2 .. E'Last) & '#');
         else
            --  decimal
            N := Integer'Value (E (E'First + 1 .. E'Last));
         end if;
         return (1 => Character'Val (N));
      else
         return (1 => Conversion (Entity'Value (E)));
      end if;
   exception
   when Constraint_Error =>
      --  No clue - give back entity itself
      return '&' & E & ';';
   end Convert_Entity;

   ----------
   --  Public functions

   procedure Parse
     (URL     : in     Ada.Strings.Unbounded.Unbounded_String;
      Content : in     String;
      Ho      : in out HTML_Tree)
   is
      truncated, syntax : exception;

      location : Location_type := nowhere;

      type p_p_Body_node is access all P_Body_Node;
      Current_Body_Pointer : p_p_Body_node := Ho.the_body'Access;
      --  Assigning to Current_body_pointer builds tree

      preformatted_level : Natural := 0;

      curs : Integer := Content'First;

      function Seek_tag_name return String is
         curs_0 : constant Integer := curs;
      begin
         while
           curs <= Content'Last and then
           Alphanum_tag_set (Content (curs)) and then
           Content (curs_0 .. curs - 1) /= "!--"  -- an HTML comment can be followed by letters without spaces.
         loop
            curs := curs + 1;
         end loop;
         return Content (curs_0 .. curs - 1);
      end Seek_tag_name;

      function Seek_Attribute_name return String is
         curs_0 : constant Integer := curs;
      begin
         while curs <= Content'Last and then Alpha_attrib_set (Content (curs)) loop
            curs := curs + 1;
         end loop;
         return Content (curs_0 .. curs - 1);
      end Seek_Attribute_name;

      function Seek_Attribute_value return String is
         curs_0 : Integer;
      begin
         if curs > Content'Last or else Content (curs) /= '=' then
            return "";
         end if;
         curs := curs + 1;
         if curs > Content'Last then
            return "";
         end if;
         if Content (curs) = '"' then     --  bracketed with "..." (the correct way)
            curs := curs + 1;
            curs_0 := curs;
            while curs <= Content'Last and then Content (curs) /= '"' loop
               curs := curs + 1;
            end loop;
            return Content (curs_0 .. curs - 1);
         elsif Content (curs) = ''' then  --  bracketed with '...' (wrong but it happens...)
            curs := curs + 1;
            curs_0 := curs;
            while curs <= Content'Last and then Content (curs) /= ''' loop
               curs := curs + 1;
            end loop;
            return Content (curs_0 .. curs - 1);
         else
            curs_0 := curs;
            while curs <= Content'Last and then not (Content (curs) = '>' or Content (curs) = ' ') loop
               curs := curs + 1;
            end loop;
            curs := curs - 1; -- Go back to last Attribute character
            return Content (curs_0 .. curs);
         end if;
      end Seek_Attribute_value;

      --  Process a tag pair ( <TAG>...</TAG> ) or a singleton tag ( <TAG> or <TAG /> ).
      --  Conventions:
      --   Enter : curs is on the first character after tag name
      --   Exit  : curs is on the '>' of the closing tag
      procedure Process_tag
        (tag_name : String;
         level    : Natural := 0)
      is
         Kind     : HTML_kind;
         New_Node : P_Body_Node := null;
         --
         --
         --   Enter : curs is on the first character after tag name
         --   Exit  : curs is on the '>' of the opening or singleton tag
         procedure Process_tag_Attributes
         is
            use Ada.Strings.Unbounded;
         begin
            loop
               if curs > Content'Last then
                  raise truncated;
               end if;
               exit when Content (curs) = '>';
               if Alpha_attrib_set (Content (curs)) then
                  declare
                     Attribute_Name  : constant String         := Seek_Attribute_name;
                     Attribute       : constant HTML_Attribute := Get_Attribute (Attribute_Name);
                     Attribute_Value : constant String         := Seek_Attribute_value;
                  begin
                     if Kind = Meta then
                        case Attribute is
                        when Charset =>
                           Parse_encoding ("charset=" & Attribute_Value, Ho.encoding);
                        when HTML_Parse.Content =>
                           Parse_encoding (Attribute_Value, Ho.encoding);
                        when others =>
                           if New_Node = null then
                              Ho.Meta.Append ((+Attribute_Name, +Attribute_Value));
                           else
                              New_Node.Meta.Append ((+Attribute_Name, +Attribute_Value));
                           end if;
                        end case;

                     elsif Kind = Link or Kind = Unknown_Tag then
                        --  ignore for now
                        null;

                     else
                        case Attribute is
                        when id =>
                           New_Node.Id := +Attribute_Value;

                        when Class =>
                           if New_Node = null then
                              Ho.Class := +Attribute_Value;
                           else
                              New_Node.Class := +Attribute_Value;
                           end if;

                        when name =>
                           if Kind = a then
                              --  name is how anchor was defined up to HTML 4.01
                              New_Node.Id := +Attribute_Value;
                           else
                              New_Node.Attributes.Insert (Attribute_Name, Attribute_Value);
                           end if;

                        when src =>
                           if Kind = img then
                              New_Node.src_URL := +Simplify_URL (Build_URL (Ho, Attribute_Value));
                           else
                              New_Node.Attributes.Insert (Attribute_Name, Attribute_Value);
                           end if;

                        when href =>
                           case Kind is
                           when a =>
                              New_Node.URL := +Simplify_URL (Build_URL (Ho, Attribute_Value));
                           when base =>
                              Ho.base_URL := +Attribute_Value;
                           when others =>
                              New_Node.Attributes.Insert (Attribute_Name, Attribute_Value);
                           end case;

                        when Charset | HTML_Parse.Content | Zzz_Unknown_Attribute =>
                           if New_Node = null then
                              Ho.Attributes.Insert (Attribute_Name, Attribute_Value);
                           else
                              New_Node.Attributes.Insert (Attribute_Name, Attribute_Value);
                           end if;
                        end case;
                     end if;
                  exception
                  when E : Constraint_Error =>
                     raise Constraint_Error with "attribute '" & Attribute_Name & "' = '" &
                       Attribute_Value & "' : " & Ada.Exceptions.Exception_Message (E);
                  end;
               end if;
               curs := curs + 1;
            end loop;
         end Process_tag_Attributes;

         --  Can be no child at all. We stop when bumping into a closing tag.
         --   Enter : curs is on the '>' of the opening tag
         --   Exit  : curs is on the '<' of any closing tag (i.e. we have a beginning of "</")
         procedure Process_children (take_text : Boolean := True)
         is
            curs_0, curs_1 : Integer;
            new_text_node  : P_Body_Node;
            is_closing_tag : Boolean;
         begin
            curs := curs + 1;
            loop
               if curs > Content'Last then
                  raise truncated;
               end if;
               curs_0 := curs;

               while curs <= Content'Last and then Content (curs) /= '<' loop
                  curs := curs + 1;
               end loop;
               if curs > curs_0 then -- OK, we have a bit of text
                  declare
                     ze_text : String renames Content (curs_0 .. curs - 1);
                  begin
                     case location is
                     when in_head =>
                        case Kind is
                        when title =>
                           Ho.Title := +ze_text;
                        when others =>
                           null;
                        end case;
                     when in_body =>
                        if take_text then
                           new_text_node := new Body_Node (body_text);
                           new_text_node.Content := +ze_text;

                           Current_Body_Pointer.all := new_text_node;
                           Current_Body_Pointer := new_text_node.next'Access; -- ready for next sibling
                        end if;
                     when nowhere =>
                        null; -- text neither in HEAD nor BODY region -> trash
                     end case;
                  end;
               end if;
               --  We have the end of the stream, or the '<' of a tag at this point.
               if curs >= Content'Last then
                  raise truncated;
               end if;
               is_closing_tag := Content (curs .. curs + 1) = "</";
               if is_closing_tag then
                  curs_1 := curs;
                  curs := curs + 2;
               else
                  curs := curs + 1;
               end if;
               declare
                  new_tag_name : constant String := Seek_tag_name;
               begin
                  if new_tag_name'Length /= 0 then
                     if is_closing_tag then
                        if Identify_tag (new_tag_name) /= Kind then
                           --  Stray close tag; ignore
                           null;
                        elsif Identify_tag (new_tag_name) in Body_singleton_tag then
                           --  ill-written singleton tag, e.g. </br>
                           Process_tag (new_tag_name, level + 1);  --  After Process_tag, we are on a '>'.
                        else  --  Start of a closing tag -> end of children list -> we leave the loop
                           curs := curs_1;
                           exit;
                        end if;
                     else
                        --  A new sibling
                        Process_tag (new_tag_name, level + 1);  --  After Process_tag, we are on a '>'.
                     end if;
                     curs := curs + 1;  --  '>' is skipped now.
                  else
                     raise syntax; -- No tag ID at all - we must have some UFO...
                  end if;
               end;
            end loop;
         end Process_children;

         procedure Process_body_tag is
         begin
            New_Node := new Body_Node (Kind);
            Current_Body_Pointer.all := New_Node;
            Process_tag_Attributes;
            case Kind is
            when pre =>
               preformatted_level := preformatted_level + 1;
            when others =>
               null;
            end case;
            if Kind in Body_bracketing_tag then
               Current_Body_Pointer := New_Node.first_child'Access; -- ready for first child
               Process_children;
            end if;
            if Kind = pre then
               preformatted_level := preformatted_level - 1;
            end if;

            Current_Body_Pointer := New_Node.next'Access; -- ready for next sibling
         end Process_body_tag;

         procedure Process_closing_tag
         is
            curs_0 : Integer;
         begin
            --  We are on the '<' of the "</TAG>"
            curs := curs + 2;
            curs_0 := curs;
            declare
               new_tag_name : constant String := Seek_tag_name;
            begin
               if Verbosity > 0 then
                  Ada.Text_IO.Put ("Closing ");
               end if;
               if Identify_tag (new_tag_name) = Kind then -- Closing tag name is correct
                  loop
                     exit when curs > Content'Last or else Content (curs) = '>';
                     curs := curs + 1;
                  end loop;
               else
                  if Verbosity > 0 then
                     Ada.Text_IO.Put (" (Wrong closing tag, should be " & Kind'Img & ") ");
                  end if;
                  --  Typically: "<li>" without "</li>".
                  curs := curs_0 - 3;
                  --  We go back to the 'x' of "x</TAG>".
               end if;
               if Verbosity > 0 then
                  Ada.Text_IO.Put_Line (" @ curs =" & curs'Img);
               end if;
            end;
         end Process_closing_tag;
         --
      begin -- Process_tag
         Kind := Identify_tag (tag_name);
         if Verbosity > 0 then
            Ada.Text_IO.Put_Line (" @ curs =" & curs'Img);
         end if;
         if Kind = Unknown_Tag then
            Process_tag_Attributes;
            Process_children;
            Process_closing_tag;
            return;
         elsif Kind = comment then
            loop
               if curs > Content'Last - 2 then
                  raise truncated with "End of HTML string reached within a comment";
               end if;
               exit when Content (curs .. curs + 2) = "-->";  --  End of comment
               curs := curs + 1;
            end loop;
            curs := curs + 2; -- Skip to the '>' of the "-->".
            return;
         elsif Kind = script then
            declare
               target : constant String := "</script>";
            begin
               loop
                  if curs > Content'Last - target'Length then
                     raise truncated with "End of HTML string reached within a script";
                  end if;
                  exit when Content (curs .. curs + target'Length - 1) = target;
                  curs := curs + 1;
               end loop;
               curs := curs + target'Length - 1; -- Skip to the '>' of the closing tag.
               return;
            end;
         end if;

         case level is
         when 0 =>      -- nesting level 0; we know the tag is HTML
            Process_tag_Attributes;
            Process_children;
         when 1 =>      -- nesting level 1
            case Kind is
            when head =>
               location := in_head;
               if Verbosity > 0 then
                  Ada.Text_IO.Put_Line ("*** HTML Region is now HEAD");
               end if;
               Process_tag_Attributes;
               Process_children;
            when b0dy =>
               location := in_body;
               if Verbosity > 0 then
                  Ada.Text_IO.Put_Line ("*** HTML Region is now BODY");
               end if;
               Process_body_tag;
            when others =>
               location := nowhere;
               if Verbosity > 0 then
                  Ada.Text_IO.Put_Line ("*** HTML Region is now NOWHERE");
               end if;
               Process_tag_Attributes;
               Process_children;
            end case;
         when others => -- nesting level 2,3,4,...
            case location is
            when in_head =>
               Process_tag_Attributes;
               if Bracketing_tag_set (Kind) then
                  Process_children; -- e.g., text of title, style.
               end if;
            when in_body =>
               if Kind in Body_kind then
                  Process_body_tag;
               else  --  No body-specific tag
                  Process_tag_Attributes;
                  if Bracketing_tag_set (Kind) then
                     Process_children (take_text => Kind /= script);
                  end if;
               end if;
            when nowhere =>
               null;
            end case;
         end case;
         if Bracketing_tag_set (Kind) then
            Process_closing_tag;
         end if;
      end Process_tag;

      html_str : String (1 .. 6);
   begin
      Ho.own_URL := URL;
      --  Skip to "<html"
      loop
         if curs + 5 > Content'Last then
            return; -- No HTML found in this string.
         end if;
         if Content (curs) = '<' then
            if Content (curs + 1 .. curs + 3) = "!--" then  --  Comment
               loop
                  curs := curs + 1;
                  if curs + 5 > Content'Last then
                     return; -- No HTML found in this string.
                  end if;
                  exit when Content (curs .. curs + 2) = "-->";  --  End of comment
               end loop;
            else
               html_str := Ada.Characters.Handling.To_Upper (Content (curs .. curs + 5));
               exit when
                 html_str = "<HTML " or else
                 html_str = "<HTML" & ASCII.LF or else
                 html_str = "<HTML" & ASCII.CR or else
                 html_str = "<HTML>";
            end if;
         end if;
         curs := curs + 1;
      end loop;
      curs := curs + 5;
      Process_tag ("html");

   exception
   when truncated | syntax =>
      --  An incomplete page; return partial tree
      null;
   end Parse;

   function Image (Node : in HTML_Parse.P_Body_Node) return String
   is
      use Ada.Strings.Unbounded;
   begin
      if Node = null then
         return "null";
      else
         case Node.Kind is
         when body_text =>
            return "body_text: [" & Trim_Blanks (-Node.Content, Trim_Space => False, Trim_Formatting => True) & ']';

         when Body_singleton_tag =>
            return '<' & HTML_kind'Image (Node.Kind) &
              (if Length (Node.Id) > 0 then " id=""" & (-Node.Id) & """" else "") &
              (if Length (Node.Class) > 0 then " class = """ & (-Node.Class) & """" else "") &
              "/>";

         when Body_bracketing_tag =>
            return '<' & Body_kind'Image (Node.Kind) &
              (if Length (Node.Id) > 0 then " id=""" & (-Node.Id) & """" else "") &
              (if Length (Node.Class) > 0 then " class = """ & (-Node.Class) & """" else "") &
              '>';
         end case;
      end if;

   end Image;

   procedure Dump_body (File : in Ada.Text_IO.File_Type; Bn : P_Body_Node; level : Natural := 0)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
   begin
      if Bn = null then
         return;
      end if;
      case Bn.Kind is
      when body_text =>
         if not All_Blank (-Bn.Content) then
            Put (File, level * "|  ");
            Put_Line (File, "body_text: [" & (-Text (Bn)) & ']');
         end if;

      when Body_singleton_tag =>
         Put (File, level * "|  ");
         Put_Line (File, Image (Bn));

      when Body_bracketing_tag =>
         Put (File, level * "|  ");
         Put_Line (File, Image (Bn));
         Dump_body (File, Bn.first_child, level + 1);

      end case;
      Dump_body (File, Bn.next, level);
   end Dump_body;

   procedure Dump (ho : HTML_Tree)
   is begin
      Dump_body (Ada.Text_IO.Standard_Output, ho.the_body);
   end Dump;

   procedure Dump (ho : HTML_Tree; File_Name : String)
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      File : File_Type;

   begin
      Create (File, Out_File, File_Name);

      Put_Line (File, "Title: " & (-ho.Title));
      Dump_body (File, ho.the_body);
      Close (File);
   end Dump;

   function Translate_Character_Entities (S : String) return String
   is
      A : Integer := 0;
   begin
      for I in S'Range loop
         case S (I) is
         when '&' =>
            A := I;
         when ';' =>
            if A > 0 then -- &...; pair found
               return S (S'First .. A - 1) &
                 Convert_Entity (S (A + 1 .. I - 1)) &
                 Translate_Character_Entities (S (I + 1 .. S'Last));
            end if;
         when others =>
            null;
         end case;
      end loop;
      --  No &...; pair found
      return S;
   end Translate_Character_Entities;

   function Root (o : in HTML_Tree) return P_Body_Node
   is begin
      return o.the_body;
   end Root;

   function Title (O : in HTML_Tree) return String
   is begin
      return -O.Title;
   end Title;

   function Kind (Node : in P_Body_Node) return HTML_kind
   is begin
      return Node.Kind;
   end Kind;

   function Id (Node : in P_Body_Node) return String
   is begin
      return -Node.Id;
   end Id;

   function Class (Node : in P_Body_Node) return String
   is begin
      return -Node.Class;
   end Class;

   function Attribute (Node : in P_Body_Node; Name : in String) return String
   is
      use String_Maps;
      I : constant Cursor := Node.Attributes.Find (Name);
   begin
      if Has_Element (I) then
         return Element (I);
      else
         return "";
      end if;
   end Attribute;

   function First_Child (Node : in P_Body_Node) return P_Body_Node
   is begin
      return Node.first_child;
   end First_Child;

   function Next_Sibling (Node : in P_Body_Node) return P_Body_Node
   is begin
      return Node.next;
   end Next_Sibling;

   function Text
     (Node            : in P_Body_Node;
      Trim_Space      : in Boolean := False;
      Trim_Formatting : in Boolean := True)
     return Ada.Strings.Unbounded.Unbounded_String
   is begin
      return +Translate_Character_Entities
        (if Trim_Space or Trim_Formatting
         then Trim_Blanks (-Node.Content, Trim_Space, Trim_Formatting)
         else -Node.Content);
   end Text;

end HTML_Parse;
