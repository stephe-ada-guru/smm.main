--  Abstract :
--
--  Parse HTML into an object tree.
--
--  Copyright (C) 2017, 2025 Stephen Leake All Rights Reserved.
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
package HTML_Parse is

   Verbosity : Integer := 0;

   type HTML_Tree is new Ada.Finalization.Limited_Controlled with private;

   procedure Parse
     (URL     : in     Ada.Strings.Unbounded.Unbounded_String;
      Content : in     String;
      Ho      : in out HTML_Tree);
   --  URL is used to resolve links in Content.

   procedure Dump (ho : HTML_Tree);
   procedure Dump (ho : HTML_Tree; File_Name : String);

   type HTML_kind is
     (
      comment,
      html,

      --  Bracketing tags that can be anywhere in the HTML code: <TAG>
      --  something... </TAG>
      Unknown_Tag,
      script,

      -----------------
      -- HEAD region --
      -----------------
      head_text, -- Text, not a tag !

      --  HEAD region singleton tags: <TAG> or <TAG /> (XHTML)
      base,
      basefont, -- deprecated, but we need to know it is a singleton
      Link,
      Meta,

      --  HEAD region bracketing tags: <TAG> something... </TAG>
      head,
      style,
      title,

      -----------------
      -- BODY region --
      -----------------
      body_text, -- Text, not a tag !

      --  BODY region singleton tags: <TAG> or <TAG /> (XHTML)
      img,
      area, br, col, hr,
      input, Param,

      --  BODY region bracketing tags: <TAG> something... </TAG>
      a,
      b0dy,
      b, i, u, strike, s,
      strong, em, dfn, var,
      big, small,
      sup, sub,
      code, samp, kbd, tt,
      del, ins, abbr, acronym, cite, blockquote,
      article, aside, figure, figcaption,
      address, nav, q, dl, dt, dd,
      details, summary,
      font, pre,
      h1, h2, h3, h4, h5, h6,
      p, div, Span,
      ul, ol, li,        -- lists
      table, tr, th, td  -- tables
     );

   subtype Univ_Bracketing_tag is HTML_kind range Unknown_Tag .. script;

   subtype Head_kind is HTML_kind range head_text .. HTML_kind'Pred (body_text);
   subtype Head_tag is Head_kind range Head_kind'Succ (head_text) .. Head_kind'Last; -- all but text
   subtype Head_singleton_tag is Head_kind range Head_kind'Succ (head_text) .. Meta;
   subtype Head_bracketing_tag is Head_tag range Head_tag'Succ (Head_singleton_tag'Last) .. Head_tag'Last;

   subtype Body_kind is HTML_kind range body_text .. HTML_kind'Last;
   subtype Body_tag is Body_kind range Body_kind'Succ (body_text) .. Body_kind'Last; -- all but text
   subtype Body_singleton_tag is Body_tag range Body_tag'First .. Param;
   subtype Body_text_or_singleton_tag is Body_kind range Body_kind'First .. Body_singleton_tag'Last;
   subtype Body_singleton_tag_no_img is Body_tag range Body_kind'Succ (img) .. Body_singleton_tag'Last;
   subtype Body_bracketing_tag is Body_tag range Body_tag'Succ (Body_singleton_tag'Last) .. Body_tag'Last;
   subtype Body_bracketing_tag_no_a is Body_tag range b0dy .. Body_tag'Last;

   type Body_Node (Kind : Body_kind) is private;
   type P_Body_Node is access Body_Node;

   function Image (Node : in P_Body_Node) return String;

   function Root (o : in HTML_Tree) return P_Body_Node;
   function Title (O : in HTML_Tree) return String;

   function Kind (Node : in P_Body_Node) return HTML_kind;
   function Id (Node : in P_Body_Node) return String;
   function Class (Node : in P_Body_Node) return String;

   function Attribute (Node : in P_Body_Node; Name : in String) return String;
   --  null string if attribute not in Node.

   function First_Child (Node : in P_Body_Node) return P_Body_Node;
   function Next_Sibling (Node : in P_Body_Node) return P_Body_Node;
   function Text
     (Node            : in P_Body_Node;
      Trim_Space      : in Boolean := False;
      Trim_Formatting : in Boolean := True)
     return Ada.Strings.Unbounded.Unbounded_String;

private

   type String_Pair is record
      Name  : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package String_Pair_Lists is new Ada.Containers.Doubly_Linked_Lists (String_Pair);

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Body_Node (kind : Body_kind) is tagged
   record
      Id         : Ada.Strings.Unbounded.Unbounded_String;
      Class      : Ada.Strings.Unbounded.Unbounded_String;
      Attributes : String_Maps.Map;
      Meta       : String_Pair_Lists.List;
      next       : aliased P_Body_Node := null; --  Next sibling
      case Kind is
      --  Text or singleton tags
      when body_text  =>
         Content : Ada.Strings.Unbounded.Unbounded_String;
      when area | br | col => null;
      when hr         =>
         hr_height     : Natural := 2;
      when img =>
         src_URL       : Ada.Strings.Unbounded.Unbounded_String; -- complete URL
      when input | Param => null;

         --  Bracketing tags: <TAG> something (the children)... </TAG>
      when Body_bracketing_tag =>
         first_child : aliased P_Body_Node := null;
         case Kind is
         when a =>
            URL : Ada.Strings.Unbounded.Unbounded_String; -- complete URL
         when ul | ol =>
            item_count : Natural; -- useful for getting the maximum marker width
         when others =>
            null;
         end case;
      end case;
   end record;

   type Encoding_choice is (utf_8, iso_8859_1, iso_8859_2, windows_1252);
   --  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

   procedure Parse_encoding (content_str : String; encoding : in out Encoding_choice);

   type HTML_Tree is new Ada.Finalization.Limited_Controlled with record
      Title      : Ada.Strings.Unbounded.Unbounded_String;
      Class      : Ada.Strings.Unbounded.Unbounded_String;
      Attributes : String_Maps.Map;
      Meta       : String_Pair_Lists.List;
      the_body   : aliased P_Body_Node := null;
      own_URL    : Ada.Strings.Unbounded.Unbounded_String; -- source URL
      base_URL   : Ada.Strings.Unbounded.Unbounded_String; -- given in <base> tag
      encoding   : Encoding_choice     := utf_8;
   end record;

   overriding
   procedure Finalize (ho : in out HTML_Tree);

end HTML_Parse;
