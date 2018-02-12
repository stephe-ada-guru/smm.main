--  Abstract :
--
--  see spec
--
--  Copyright (C) 2007 - 2009, 2015 - 2016, 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
package body Test_Utils is

   procedure Cleanup
   is begin
      Ada.Directories.Delete_Tree ("tmp");
   exception
   when Ada.Text_IO.Name_Error =>
      --  already deleted
      null;
   end Cleanup;

   procedure Create_Empty_DB (DB_File_Name : in String)
   is
      Success : Boolean;
      Args : constant GNAT.OS_Lib.Argument_List :=
        (1 => new String'("-init"),
         2 => new String'("../source/create_schema.sql"),
         3 => new String'(DB_File_Name),
         4 => new String'(".quit"));
   begin
      if Ada.Directories.Exists (DB_File_Name) then
         Ada.Directories.Delete_File (DB_File_Name);
      end if;

      GNAT.OS_Lib.Spawn ("sqlite3", Args, Success);
      if not Success then
         raise Program_Error with "sqlite3 failed to create db";
      end if;
   end Create_Empty_DB;

   procedure Create_Test_File (Path : in String)
   is
      use Ada.Text_IO;
      Temp : File_Type;
   begin
      Create (Temp, Out_File, Path);
      Put_Line (Temp, "body: " & Path);
      Close (Temp);
   exception
   when Name_Error =>
      raise Name_Error with "'" & Path & "' cannot be created";
   end Create_Test_File;

   procedure Insert
     (DB              : in SMM.Database.Database;
      ID              : in Integer;
      File_Name       : in String;
      Last_Downloaded : in Duration;
      Category        : in String := "vocal")
   is
      One_Second_Time_String : constant SMM.Database.Time_String := "1958-01-01 00:00:01";
   begin
      DB.Insert
        (ID              => ID,
         File_Name       => File_Name,
         Category        => Category,
         Artist          => "none",
         Album           => "none",
         Title           => "none",
         Last_Downloaded =>
           (if Last_Downloaded = 0.0
            then SMM.Database.Default_Time_String
            else One_Second_Time_String));
   end Insert;

   function "&" (Left, Right : in SMM.ID3.Tag) return SMM.ID3.Tag_Lists.List
   is begin
      return Result : SMM.ID3.Tag_Lists.List do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   function "&" (List : in SMM.ID3.Tag_Lists.List; Item : in SMM.ID3.Tag) return SMM.ID3.Tag_Lists.List
   is begin
      return Result : SMM.ID3.Tag_Lists.List := List do
         Result.Append (Item);
      end return;
   end "&";

   procedure Check_Exists (Path : in String; Expected_Exists : in Boolean)
   is
      use Ada.Directories;
   begin
      if Expected_Exists then
         AUnit.Assertions.Assert (Exists (Path), Path & " should NOT have been deleted");
      else
         AUnit.Assertions.Assert (not Exists (Path), Path & " SHOULD have been deleted");
      end if;
   end Check_Exists;

end Test_Utils;
