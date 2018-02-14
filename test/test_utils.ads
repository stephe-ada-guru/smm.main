--  Abstract :
--
--  Utilities for unit tests
--
--  Copyright (C) 2007, 2009, 2016, 2018 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Unbounded;
with SMM.Database;
with SMM.ID3;
package Test_Utils is
   pragma Elaborate_Body; -- Ada.Text_IO

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Delete_File (Name : in String);

   procedure Cleanup;
   --  Delete tmp/

   procedure Create_Empty_DB (DB_File_Name : in String);
   --  If DB_File_Name exists, deletes it. Then spawns sqlite3, creates
   --  schema.

   procedure Create_Test_File (Path : in String);
   --  File contains one line, containing "body: " & Path.
   --  Path is relative to current process directory.

   procedure Insert
     (DB              : in SMM.Database.Database;
      ID              : in Integer;
      File_Name       : in String;
      Last_Downloaded : in Duration;
      Category        : in String := "vocal");
   --  Insert a database element; other fields are null or "none".

   function "&" (Left, Right : in SMM.ID3.Tag) return SMM.ID3.Tag_Lists.List;
   function "&" (List : in SMM.ID3.Tag_Lists.List; Item : in SMM.ID3.Tag) return SMM.ID3.Tag_Lists.List;
   --  For SMM.ID3.Create

   procedure Check_Exists (Path : in String; Expected_Exists : in Boolean);
   --  Assertion.
   --  Path is relative to current process directory.

end Test_Utils;
