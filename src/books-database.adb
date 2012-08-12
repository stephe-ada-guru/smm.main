--  Abstract :
--
--  see spec
--
--  Copyright (C) 2002, 2003, 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Sqlite;
package body Books.Database is

   --  Subprogram bodies (alphabetical order)

   procedure Checked_Execute
     (T         : in out Table'Class;
      Statement : in     String;
      Params    : in     GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
   is begin
      --  FIXME: close cursor?
      GNATCOLL.SQL.Exec.Fetch (T.Cursor, T.DB.Connection, Statement, Params);

      --  FIXME: exceptions? Errors?
   end Checked_Execute;

   function Field
     (T           : in Table;
      Field_Index : in GNATCOLL.SQL.Exec.Field_Index)
     return String
   is begin
      if T.Cursor.Has_Row then
         raise No_Data;
      elsif T.Cursor.Is_Null (Field_Index) then
         raise Null_Field;
      else
         return T.Cursor.Value (Field_Index);
      end if;
   end Field;

   overriding procedure Finalize (T : in out Table)
   is begin
      --  We only finalize a table when we are about to close the
      --  application, when all memory will be recovered anyway. So we
      --  don't bother freeing the strings (this avoids using a named
      --  type).
      --  if T.Update_Statement /= null then
      --     Free (T.Update_Statement);
      --  end if;
      --  if T.Insert_Statement /= null then
      --     Free (T.Insert_Statement);
      --  end if;
      --  if T.Delete_Statement /= null then
      --     Free (T.Delete_Statement);
      --  end if;
      --  if T.All_By_ID_Statement /= null then
      --     Free (T.All_By_ID_Statement);
      --  end if;
      null; --  FIXME: delete this procedure!
   end Finalize;

   overriding procedure Finalize (DB : in out Database)
   is begin
      Ada.Text_IO.Put ("Disconnecting from database ... ");
      --  We ignore all errors, since we wouldn't be able to do
      --  anything about them at this point.
      GNATCOLL.SQL.Exec.Free (DB.Connection);
      Ada.Text_IO.Put_Line ("done.");
   exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Database disconnect: exception " & Ada.Exceptions.Exception_Message (E));
   end Finalize;

   procedure Find
     (T         : in out Table'Class;
      Statement : access constant String;
      Params    : in     GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
   is
   begin
      --  FIXME: close cursor?
      T.Find_Statement := Statement;
      Checked_Execute (T, T.Find_Statement.all, Params);
      Next (T);
   end Find;

   procedure Find_All_By_ID (T : in out Table'Class)
   is begin
      Find (T, T.All_By_ID_Statement);
   end Find_All_By_ID;

   procedure Free (Pointer : in out Database_Access)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (Database'Class, Database_Access);
   begin
      Deallocate (Pointer);
   end Free;

   procedure Free (Pointer : in out Table_Access)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (Table'Class, Table_Access);
   begin
      Deallocate (Pointer);
   end Free;

   function Image (ID : in ID_Type) return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Result : String (1 .. 5);
   begin
      Move
        (Source  => Trim (ID_Type'Image (ID), Left),
         Target  => Result,
         Justify => Right,
         Pad     => '0');
      return Result;
   end Image;

   overriding procedure Initialize (DB : in out Database)
   is
      use GNATCOLL.SQL.Exec;
      use SAL.Config_Files;

      Db_File : constant String := Read (DB.Config.all, "Database_File", "~/.books/books.db");
      Descrip : constant Database_Description := GNATCOLL.SQL.Sqlite.Setup (Db_File);
   begin

      if Descrip = null then
         raise SAL.Config_File_Error with Db_File & " database file not found or not valid";
      end if;

      DB.Connection := GNATCOLL.SQL.Exec.Build_Connection (Descrip);

      Ada.Text_IO.Put_Line ("Connected to database " & Db_File);
   end Initialize;

   procedure Next (T : in out Table'Class)
   is begin
      T.Cursor.Next;
   end Next;

   function Valid (T : in Table'Class) return Boolean
   is begin
      return T.Cursor.Has_Row;
   end Valid;

   function Valid_Field
     (T     : in Table'Class;
      Field : in GNATCOLL.SQL.Exec.Field_Index)
     return Boolean
   is begin
      return T.Cursor.Has_Row and then not T.Cursor.Is_Null (Field);
   end Valid_Field;

end Books.Database;
