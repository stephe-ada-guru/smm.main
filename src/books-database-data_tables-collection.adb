--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Fixed;
with GNU.DB.SQLCLI.Statement_Attribute;
package body Books.Database.Data_Tables.Collection is

   --  Local declarations

   procedure Copy
     (T            : in out Table;
      Name         : in     String;
      Editor       : in     ID_Type;
      Editor_Valid : in     Boolean;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean);
   --  Copy Data to Table fields.

   ----------
   --  Subprogram bodies (alphabetical order)

   procedure Copy
     (T            : in out Table;
      Name         : in     String;
      Editor       : in     ID_Type;
      Editor_Valid : in     Boolean;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      Move (Source => Name, Target => T.Name.all, Drop => Right);
      T.Name_Length := GNU.DB.SQLCLI.SQLINTEGER'Min (T.Name.all'Length, Name'Length);

      if Editor_Valid then
         T.Editor           := Editor;
         T.Editor_Indicator := ID_Type'Size / 8;
      else
         T.Editor_Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA;
      end if;

      if Year_Valid then
         T.Year           := Year;
         T.Year_Indicator := Interfaces.Unsigned_16'Size / 8;
      else
         T.Year_Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA;
      end if;
   end Copy;

   function Editor (T : in Table) return ID_Type
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if T.Editor_Indicator = GNU.DB.SQLCLI.SQL_NULL_DATA then
         raise No_Data;
      else
         return T.Editor;
      end if;
   end Editor;

   function Editor (T : in Data_Tables.Table_Access) return ID_Type
   is begin
      return Editor (Table (T.all));
   end Editor;

   function Editor_Valid (T : in Data_Tables.Table_Access) return Boolean
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      return Table (T.all).Editor_Indicator /= GNU.DB.SQLCLI.SQL_NULL_DATA;
   end Editor_Valid;

   overriding procedure Finalize (T : in out Table)
   is
      use type GNU.DB.SQLCLI.SQLHANDLE;
   begin
      Books.Database.Data_Tables.Finalize (Books.Database.Data_Tables.Table (T));

      if T.By_Editor_Statement /= GNU.DB.SQLCLI.SQL_NULL_HANDLE then
         GNU.DB.SQLCLI.SQLFreeHandle (GNU.DB.SQLCLI.SQL_HANDLE_STMT, T.By_Editor_Statement);
      end if;
   end Finalize;

   procedure Find_Editor (T : in out Table; Editor : in ID_Type)
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      T.Editor           := Editor;
      T.Editor_Indicator := ID_Type'Size / 8;

      Find (T, T.By_Editor_Statement);
   end Find_Editor;

   procedure Find_Editor (T : in Data_Tables.Table_Access; Editor : in ID_Type)
   is begin
      Find_Editor (Table (T.all), Editor);
   end Find_Editor;

   overriding procedure Initialize (T : in out Table)
   is
      use GNU.DB.SQLCLI;
      use GNU.DB.SQLCLI.Statement_Attribute;
   begin
      if T.Name = null then
         T.Name         := new String'(1 .. Field_Length  => ' ');
         T.Find_Pattern := new String'(1 .. Field_Length + 1 => ' '); -- for '%'
      end if;

      --  All_By_ID_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.All_By_ID_Statement);
      SQLPrepare
        (T.All_By_ID_Statement,
         String'("SELECT Name, Editor, Year FROM Collection ORDER BY ID"));

      SQLBindCol                     (T.All_By_ID_Statement, 1, T.Name, T.Name_Length'Access);
      ID_Binding.SQLBindCol          (T.All_By_ID_Statement, 2, T.Editor'Access, T.Editor_Indicator'Access);
      Unsigned_16_Binding.SQLBindCol (T.All_By_ID_Statement, 3, T.Year'Access, T.Year_Indicator'Access);

      --  By_ID_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_ID_Statement);
      SQLPrepare
        (T.By_ID_Statement,
         String'("SELECT ID, Name, Editor, Year FROM Collection WHERE ID = ?"));

      ID_Binding.SQLBindParameter (T.By_ID_Statement, 1, T.ID'Access, T.ID_Indicator'Access);

      ID_Binding.SQLBindCol          (T.By_ID_Statement, 1, T.ID'Access, T.ID_Indicator'Access);
      SQLBindCol                     (T.By_ID_Statement, 2, T.Name, T.Name_Length'Access);
      ID_Binding.SQLBindCol          (T.By_ID_Statement, 3, T.Editor'Access, T.Editor_Indicator'Access);
      Unsigned_16_Binding.SQLBindCol (T.By_ID_Statement, 4, T.Year'Access, T.Year_Indicator'Access);

      --  By_Name_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_Name_Statement);
      SQLPrepare
        (T.By_Name_Statement,
         String'("SELECT ID, Name, Editor, Year FROM Collection WHERE Name LIKE ? ORDER BY Name"));

      SQLSetStmtAttr (T.By_Name_Statement, SQL_BIND_BY_COLUMN);
      SQLSetStmtAttr (T.By_Name_Statement, Statement_Attribute_Unsigned'(SQL_ROWSET_SIZE, 1));

      SQLBindParameter (T.By_Name_Statement, 1, T.Find_Pattern, T.Find_Pattern_Length'Access);

      ID_Binding.SQLBindCol          (T.By_Name_Statement, 1, T.ID'Access, T.ID_Indicator'Access);
      SQLBindCol                     (T.By_Name_Statement, 2, T.Name, T.Name_Length'Access);
      ID_Binding.SQLBindCol          (T.By_Name_Statement, 3, T.Editor'Access, T.Editor_Indicator'Access);
      Unsigned_16_Binding.SQLBindCol (T.By_Name_Statement, 4, T.Year'Access, T.Year_Indicator'Access);

      T.Find_Pattern (1)    := '%';
      T.Find_Pattern_Length := 1;
      T.Find_Statement      := T.By_Name_Statement;

      Checked_Execute (T.Find_Statement); --  So Next is valid.

      --  Update statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Update_Statement);
      SQLPrepare
        (T.Update_Statement,
         String'("UPDATE Collection SET Name = ?, Editor = ?, Year = ? WHERE ID = ?"));

      SQLBindParameter                     (T.Update_Statement, 1, T.Name, T.Name_Length'Access);
      ID_Binding.SQLBindParameter          (T.Update_Statement, 2, T.Editor'Access, T.Editor_Indicator'Access);
      Unsigned_16_Binding.SQLBindParameter (T.Update_Statement, 3, T.Year'Access, T.Year_Indicator'Access);
      ID_Binding.SQLBindParameter          (T.Update_Statement, 4, T.ID'Access, T.ID_Indicator'Access);

      --  Insert statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Insert_Statement);
      SQLPrepare
        (T.Insert_Statement,
         String'("INSERT INTO Collection (Name, Editor, Year) VALUES (?, ?, ?)"));

      SQLBindParameter                     (T.Insert_Statement, 1, T.Name, T.Name_Length'Access);
      ID_Binding.SQLBindParameter          (T.Insert_Statement, 2, T.Editor'Access, T.Editor_Indicator'Access);
      Unsigned_16_Binding.SQLBindParameter (T.Insert_Statement, 3, T.Year'Access, T.Year_Indicator'Access);

      --  Delete statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Delete_Statement);
      SQLPrepare
        (T.Delete_Statement,
         String'("DELETE FROM Collection WHERE ID = ?"));

      ID_Binding.SQLBindParameter (T.Delete_Statement, 1, T.ID'Access, T.ID_Indicator'Access);

      --  By_Editor_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_Editor_Statement);
      SQLPrepare
        (T.By_Editor_Statement,
         String'("SELECT ID, Name, Editor, Year FROM Collection WHERE Editor = ?"));

      ID_Binding.SQLBindParameter (T.By_Editor_Statement, 1, T.Editor'Access, T.Editor_Indicator'Access);

      ID_Binding.SQLBindCol          (T.By_Editor_Statement, 1, T.ID'Access, T.ID_Indicator'Access);
      SQLBindCol                     (T.By_Editor_Statement, 2, T.Name, T.Name_Length'Access);
      ID_Binding.SQLBindCol          (T.By_Editor_Statement, 3, T.Editor'Access, T.Editor_Indicator'Access);
      Unsigned_16_Binding.SQLBindCol (T.By_Editor_Statement, 4, T.Year'Access, T.Year_Indicator'Access);

   end Initialize;

   procedure Insert
     (T            : in out Table;
      Name         : in     String;
      Editor       : in     ID_Type;
      Editor_Valid : in     Boolean;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean)
   is begin
      Copy (T, Name, Editor, Editor_Valid, Year, Year_Valid);
      Checked_Execute (T.Insert_Statement);
      Find (T, Name);
   end Insert;

   function Name (T : in Table) return String is
   begin
      return T.Name (1 .. Integer (T.Name_Length));
   end Name;

   function Name (T : in Data_Tables.Table_Access) return String
   is begin
      return Name (Table (T.all));
   end Name;

   procedure Update
     (T      : in out Table;
      Name   : in     String;
      Editor : in     ID_Type;
      Editor_Valid : in     Boolean;
      Year   : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean)
   is begin
      Copy (T, Name, Editor, Editor_Valid, Year, Year_Valid);
      Checked_Execute (T.Update_Statement);
   end Update;

   function Year (T : in Table) return Interfaces.Unsigned_16
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if T.ID_Indicator = GNU.DB.SQLCLI.SQL_NULL_DATA then
         raise No_Data;
      else
         return T.Year;
      end if;
   end Year;

   function Year (T : in Data_Tables.Table_Access) return Interfaces.Unsigned_16
   is begin
      return Year (Table (T.all));
   end Year;

   function Year_Valid (T : in Data_Tables.Table_Access) return Boolean
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      return Table (T.all).Year_Indicator /= GNU.DB.SQLCLI.SQL_NULL_DATA;
   end Year_Valid;

end Books.Database.Data_Tables.Collection;
