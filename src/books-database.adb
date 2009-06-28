--  Abstract :
--
--  see spec
--
--  Copyright (C) 2002, 2003, 2004, 2009 Stephen Leake.  All Rights Reserved.
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
--

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with GNU.DB.SQLCLI.Environment_Attribute;
package body Books.Database is

   --  Subprogram bodies (alphabetical order)

   overriding procedure Finalize (T : in out Table)
   is
      use GNU.DB.SQLCLI;
   begin
      if T.Update_Statement /= SQL_NULL_HANDLE then
         SQLFreeHandle (SQL_HANDLE_STMT, T.Update_Statement);
      end if;
      if T.Insert_Statement /= SQL_NULL_HANDLE then
         SQLFreeHandle (SQL_HANDLE_STMT, T.Insert_Statement);
      end if;
      if T.Delete_Statement /= SQL_NULL_HANDLE then
         SQLFreeHandle (SQL_HANDLE_STMT, T.Delete_Statement);
      end if;
      if T.All_By_ID_Statement /= SQL_NULL_HANDLE then
         SQLFreeHandle (SQL_HANDLE_STMT, T.All_By_ID_Statement);
      end if;
   end Finalize;

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
      use GNU.DB.SQLCLI;
   begin

      SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, DB.Environment);

      Environment_Attribute.SQLSetEnvAttr
        (DB.Environment,  Environment_Attribute.Environment_Attribute_ODBC_Version'
           (Attribute => Environment_Attribute.SQL_ATTR_ODBC_VERSION,
            Value     => Environment_Attribute.SQL_OV_ODBC3));

      SQLAllocHandle (SQL_HANDLE_DBC, DB.Environment, DB.Connection);

      declare
         use SAL.Config_Files;
         Database_Name : constant String := Read (DB.Config.all, "Database_Name", "books");
         User_Name     : constant String := Read (DB.Config.all, "Database_Username", "");
      begin
         Generate_Detailed_Exceptions := False;
         SQLConnect
           (ConnectionHandle => DB.Connection,
            ServerName       => Database_Name, --  local server, so servername only includes database name.
            UserName         => User_Name,
            Authentication   => "");
      exception
      when E : Database_Error =>
         declare
            use Ada.Strings.Fixed, GNAT.OS_Lib;
            Error_Message  : constant String := Ada.Exceptions.Exception_Message (E);

            Server_Command : constant String :=
              Read (DB.Config.all, "Server_Command", "cmd.exe /c net start mysql");

            Args    : Argument_List_Access := Argument_String_To_List (Server_Command);
            Success : Boolean;
         begin
            --  Check the error message to see what's wrong.

            if Index (Error_Message, "Can't connect to MySQL server") /= 0 then
               --  MySQL server has not been started. Assume
               --  Server_Command _does_ return.
               Ada.Text_IO.Put_Line ("Starting db server via '" & Server_Command);
               Spawn (Args (Args'First).all, Args (Args'First + 1 .. Args'Last), Success);
               Free (Args);
               if not Success then
                  Ada.Text_IO.Put_Line (" failed.");
                  Ada.Exceptions.Raise_Exception
                    (SAL.Config_File_Error'Identity,
                     SAL.Config_Files.Writeable_File_Name (DB.Config.all) &
                       ":0:0: Server_Command failed to start database server");
               else
                  --  DB server started, now connect
                  SQLConnect
                    (ConnectionHandle => DB.Connection,
                     ServerName       => Database_Name,
                     UserName         => User_Name,
                     Authentication   => "");
               end if;
            elsif Index (Error_Message, "Data source name not found") /= 0 then
               raise SAL.Config_File_Error with "ODBC data source '" & Database_Name & "' not found";
            elsif Index (Error_Message, "Access denied for user") /= 0 then
               raise SAL.Config_File_Error with "database user '" & User_Name & "' denied access: " &
                 Ada.Exceptions.Exception_Message (E);
            else
               raise;
            end if;

         end;
      end;
      Ada.Text_IO.Put_Line ("Connected to database.");
   end Initialize;

   overriding procedure Finalize (DB : in out Database)
   is
      use GNU.DB.SQLCLI;
   begin
      Ada.Text_IO.Put ("Disconnecting from database ... ");
      --  We ignore all errors, since we wouldn't be able to do
      --  anything about them at this point.
      SQLDisconnect (DB.Connection);
      SQLFreeHandle (SQL_HANDLE_DBC, DB.Connection);
      SQLFreeHandle (SQL_HANDLE_ENV, DB.Environment);

      Ada.Text_IO.Put_Line ("done.");

   exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Database disconnect: exception " & Ada.Exceptions.Exception_Message (E));
   end Finalize;

   procedure Find_All_By_ID (T : in out Table'Class)
   is
      use GNU.DB.SQLCLI;
   begin
      SQLCloseCursor (T.All_By_ID_Statement);
      Checked_Execute (T.All_By_ID_Statement);
      T.Find_Statement := T.All_By_ID_Statement;
      Next (T);
   exception
   when GNU.DB.SQLCLI.No_Data =>
      SQLCloseCursor (T.All_By_ID_Statement);
      Clear_Data (T);
   end Find_All_By_ID;

   procedure Next (T : in Table'Class)
   is begin
      GNU.DB.SQLCLI.SQLFetch (T.Find_Statement);
   exception
   when GNU.DB.SQLCLI.Database_Error =>
      --  Next button pushed twice by mistake; cursor already closed
      raise Books.Database.No_Data;
   when GNU.DB.SQLCLI.No_Data =>
      GNU.DB.SQLCLI.SQLCloseCursor (T.Find_Statement);
      raise Books.Database.No_Data;
   end Next;

   function Value (ID : in String) return ID_Type
   is begin
      return ID_Type'Value (ID);
   end Value;

   procedure Checked_Execute (Statement : in GNU.DB.SQLCLI.SQLHANDLE)
   is begin
      GNU.DB.SQLCLI.SQLExecute (Statement);
   exception
   when E : GNU.DB.SQLCLI.Database_Error =>
      if 0 /= Ada.Strings.Fixed.Index
        (Source  => Ada.Exceptions.Exception_Message (E),
         Pattern => "Duplicate entry")
      then
         raise Entry_Error with "Entry already in database";
      else
         raise;
      end if;
   end Checked_Execute;

end Books.Database;
