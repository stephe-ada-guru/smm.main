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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Exec.Aux;
with GNATCOLL.SQL.Sqlite;
with SAL.File_Names;
package body Books.Database is

   --  Subprogram bodies (alphabetical order)

   procedure Checked_Execute
     (T         : in out Table'Class;
      Statement : in     String;
      Params    : in     GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
   is begin
      GNATCOLL.SQL.Exec.Execute (T.DB.Connection, Statement, Params);

      if T.DB.Connection.Success then
         GNATCOLL.SQL.Exec.Commit (T.DB.Connection);
      else
         declare
            Msg : constant String := T.DB.Connection.Error;
         begin
            --  GNATCOLL SQLite has obscure behavior with respect to
            --  cursors and rollback; if a cursor is active, it prevents
            --  Rollback from working (SQLite reports "database locked").
            --  It doesn't prevent a successful INSERT. So we have to
            --  Finalize any cursor before calling Rollback. Another
            --  GNATCOLL quirk makes Finalize (Cursor) not visible, so we
            --  use our own GNATCOLL.SQL.Exec.Aux
            GNATCOLL.SQL.Exec.Aux.Finalize (T.Cursor);
            GNATCOLL.SQL.Exec.Rollback (T.DB.Connection);

            raise Entry_Error with Msg;
         end;
      end if;
   end Checked_Execute;

   function Field
     (T           : in Table;
      Field_Index : in GNATCOLL.SQL.Exec.Field_Index)
     return String
   is begin
      if not T.Cursor.Has_Row then
         raise No_Data;
      elsif T.Cursor.Is_Null (Field_Index) then
         raise Null_Field with Ada.Tags.Expanded_Name (Table'Class (T)'Tag) & " "
           & GNATCOLL.SQL.Exec.Field_Index'Image (Field_Index);
      else
         return T.Cursor.Value (Field_Index);
      end if;
   end Field;

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
      Statement : in     String;
      Params    : in     GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
   is begin
      GNATCOLL.SQL.Exec.Fetch (T.Cursor, T.DB.Connection, Statement, Params);

      if not T.DB.Connection.Success then
         raise Entry_Error with T.DB.Connection.Error; --  FIXME: bad exception name
      end if;
   end Find;

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
      use SAL.File_Names;

      Db_File     : constant String := Read (DB.Config.all, "Database_File", "$HOME/.books/books.db");
      Db_File_Abs : constant String := Replace_Environment_Variables (Db_File);
   begin

      if not Ada.Directories.Exists (Db_File_Abs) then
         raise SAL.Config_File_Error with Db_File_Abs & " does not exist";
      end if;

      DB.Connection := GNATCOLL.SQL.Exec.Build_Connection (GNATCOLL.SQL.Sqlite.Setup (Db_File_Abs));

      if not DB.Connection.Success then
         --  This doesn't fail if Db_File doesn't exist; SQLite creates :memory: database?
         --  But then later queries fail. IMPROVEME: check for schema.
         raise SAL.Config_File_Error with Db_File & DB.Connection.Error;
      end if;

      Ada.Text_IO.Put_Line ("Connected to database " & Db_File);
   end Initialize;

   procedure Next (T : in out Table'Class)
   is begin
      T.Cursor.Next;
   end Next;

   function Valid (T : in Table) return Boolean
   is begin
      return T.Cursor.Has_Row;
   end Valid;

   function Valid_Field
     (T     : in Table;
      Field : in GNATCOLL.SQL.Exec.Field_Index)
     return Boolean
   is begin
      return T.Cursor.Has_Row and then not T.Cursor.Is_Null (Field);
   end Valid_Field;

end Books.Database;
