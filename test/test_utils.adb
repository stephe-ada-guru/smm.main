--  Abstract :
--
--  see spec
--
--  Copyright (C) 2007 - 2009, 2015 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Text_IO;
package body Test_Utils is

   procedure Cleanup
   is begin
      Ada.Directories.Delete_Tree ("tmp");
   exception
   when E : Ada.Text_IO.Name_Error =>
      --  already deleted
      --  FIXME: debugging
      --  null;
      Ada.Text_IO.Put_Line ("cleanup: name_error: " & Ada.Exceptions.Exception_Message (E));
   end Cleanup;

   procedure Create_Test_File (Path : in String)
   is
      use Ada.Text_IO;
      Temp : File_Type;
   begin
      Create (Temp, Out_File, Path);
      Put_Line (Temp, "dummy");
      Close (Temp);
   exception
   when Name_Error =>
      raise Name_Error with "'" & Path & "' cannot be created";
   end Create_Test_File;

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
