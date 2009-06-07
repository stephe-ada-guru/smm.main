--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNU.DB.SQLCLI.Environment_Attribute;
package body Import_Books is

   procedure Connect (Database_Username : in String)
   is
      use GNU.DB.SQLCLI;
   begin
      SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, Environment);

      Environment_Attribute.SQLSetEnvAttr
        (Environment,  Environment_Attribute.Environment_Attribute_ODBC_Version'
           (Attribute => Environment_Attribute.SQL_ATTR_ODBC_VERSION,
            Value     => Environment_Attribute.SQL_OV_ODBC3));

      SQLAllocHandle (SQL_HANDLE_DBC, Environment, MySQL_Connection);

      Ada.Text_IO.Put_Line ("Establishing connection to " & MySQL_DSN);

      SQLConnect
        (ConnectionHandle => MySQL_Connection,
         ServerName       => MySQL_DSN,
         UserName         => Database_Username,
         Authentication   => "");
   end Connect;

   procedure Disconnect
   is
      use GNU.DB.SQLCLI;

      Ignore : SQLRETURN;
      pragma Unreferenced (Ignore);
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Disconnecting");

      Ignore := SQLDisconnect (MySQL_Connection);

      Ignore := SQLFreeHandle (SQL_HANDLE_DBC, MySQL_Connection);
      Ignore := SQLFreeHandle (SQL_HANDLE_ENV, Environment);

      Ada.Text_IO.Put_Line ("Bye");
   end Disconnect;

   function Quote
     (Item        : in String;
      Item_Length : in GNU.DB.SQLCLI.SQLINTEGER)
     return String
   is
      use Ada.Strings.Fixed;
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if Item_Length <= 0 then
         return """""";
      else
         return SAL.CSV.Quote (Trim (Item (Item'First .. Integer (Item_Length)), Ada.Strings.Both));
      end if;
   end Quote;

   procedure Warm_Fuzzy
   is begin
      Ada.Text_IO.Put ('.');
      Warm_Fuzzy_Count := Warm_Fuzzy_Count + 1;
      if Warm_Fuzzy_Count mod 100 = 0 then
         Ada.Text_IO.New_Line;
      end if;
   end Warm_Fuzzy;

   procedure Read_String
     (File         : in     SAL.CSV.File_Type;
      Column       : in     Integer;
      Value        :    out String;
      Value_Length :    out GNU.DB.SQLCLI.SQLINTEGER)
   is
      Data : constant String := SAL.CSV.Unquote (SAL.CSV.Read (File, Column));
   begin
      Ada.Strings.Fixed.Move (Data, Value);
      Value_Length := Data'Length;
   end Read_String;

   procedure Read_Int_16
     (File      : in     SAL.CSV.File_Type;
      Column    : in     Integer;
      Value     :    out Interfaces.Integer_16;
      Indicator :    out GNU.DB.SQLCLI.SQLINTEGER)
   is
      Data : constant String := SAL.CSV.Read (File, Column);
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if Data'Length = 0 then
         Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA;
         Value     := 0;
      else
         Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA + 1;
         Value := Interfaces.Integer_16'Value (Data);
      end if;
   exception
   when Constraint_Error =>
      raise Constraint_Error with Data & " exceeds Integer_16";
   end Read_Int_16;

   procedure Read_Unsigned_8
     (File      : in     SAL.CSV.File_Type;
      Column    : in     Integer;
      Value     :    out Interfaces.Unsigned_8;
      Indicator :    out GNU.DB.SQLCLI.SQLINTEGER)
   is
      Data : constant String := SAL.CSV.Read (File, Column);
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if Data'Length = 0 then
         Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA;
         Value     := 0;
      else
         Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA + 1;
         Value := Interfaces.Unsigned_8'Value (Data);
      end if;
   exception
   when Constraint_Error =>
      raise Constraint_Error with Data & " exceeds Unsigned_8";
   end Read_Unsigned_8;

end Import_Books;
