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
package body Books.Database.Data_Tables.Author is

   --  Local declarations

   procedure Copy
     (T           : in out Table;
      First_Name  : in     String;
      Middle_Name : in     String;
      Last_Name   : in     String);
   --  Copy names to Table fields.

   ----------
   --  Subprogram bodies (alphabetical order)

   procedure Clear_Data (T : in out Table)
   is begin
      Copy (T, "", "", "");
   end Clear_Data;

   procedure Copy
     (T           : in out Table;
      First_Name  : in     String;
      Middle_Name : in     String;
      Last_Name   : in     String)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use GNU.DB.SQLCLI;
   begin
      Move (Source => First_Name, Target => T.First.all, Drop => Right);
      T.First_Length := SQLINTEGER'Min (T.First.all'Length, First_Name'Length);
      if T.First_Length = 0 then
         T.First_Length := SQL_NULL_DATA;
      end if;

      Move (Source => Middle_Name, Target => T.Middle.all, Drop => Right);
      T.Middle_Length := SQLINTEGER'Min (T.Middle.all'Length, Middle_Name'Length);
      if T.Middle_Length = 0 then
         T.Middle_Length := SQL_NULL_DATA;
      end if;

      Move (Source => Last_Name, Target => T.Last.all, Drop => Right);
      T.Last_Length := SQLINTEGER'Min (T.Last.all'Length, Last_Name'Length);
      if T.Last_Length = 0 then
         T.Last_Length := SQL_NULL_DATA;
      end if;
   end Copy;

   procedure Find_Name (T : in out Table; Item : in String)
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      T.Find_Pattern (1 .. Item'Length) := Item;
      T.Find_Pattern (Item'Length + 1) := '%';
      T.Find_Pattern_Length := Item'Length + 1;
      GNU.DB.SQLCLI.SQLCloseCursor (T.By_Name_Statement);
      Checked_Execute (T.By_Name_Statement);
      T.Find_Statement := T.By_Name_Statement;
      Next (T);
   exception
   when GNU.DB.SQLCLI.No_Data =>
      GNU.DB.SQLCLI.SQLCloseCursor (T.By_Name_Statement);
      --  Just keep current data.
   end Find_Name;

   function First_Name (T : in Table) return String is
   begin
      return T.First (1 .. Integer (T.First_Length));
   end First_Name;

   procedure Initialize (T : in out Table)
   is
      use GNU.DB.SQLCLI;
      use GNU.DB.SQLCLI.Statement_Attribute;
   begin
      if T.First = null then
         T.First        := new String'(1 .. Name_Field_Length + 1 => ' ');
         T.Middle       := new String'(1 .. Name_Field_Length + 1 => ' ');
         T.Last         := new String'(1 .. Name_Field_Length + 1 => ' ');
         T.Find_Pattern := new String'(1 .. Name_Field_Length + 1 => ' ');
      end if;

      --  All_By_ID_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.All_By_ID_Statement);
      SQLPrepare
        (T.All_By_ID_Statement,
         String'("SELECT First, Middle, Last FROM Author ORDER BY ID"));

      SQLBindCol (T.All_By_ID_Statement, 1, T.First, T.First_Length'Access);
      SQLBindCol (T.All_By_ID_Statement, 2, T.Middle, T.Middle_Length'Access);
      SQLBindCol (T.All_By_ID_Statement, 3, T.Last, T.Last_Length'Access);

      --  By_ID_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_ID_Statement);
      SQLPrepare
        (T.By_ID_Statement,
         String'("SELECT ID, First, Middle, Last FROM Author WHERE ID = ?"));

      ID_Binding.SQLBindParameter (T.By_ID_Statement, 1, T.ID'Access, T.ID_Indicator'Access);

      ID_Binding.SQLBindCol (T.By_ID_Statement, 1, T.ID'Access, T.ID_Indicator'Access);
      SQLBindCol            (T.By_ID_Statement, 2, T.First, T.First_Length'Access);
      SQLBindCol            (T.By_ID_Statement, 3, T.Middle, T.Middle_Length'Access);
      SQLBindCol            (T.By_ID_Statement, 4, T.Last, T.Last_Length'Access);

      --  By_Name_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_Name_Statement);
      SQLPrepare
        (T.By_Name_Statement,
         String'("SELECT ID, First, Middle, Last FROM Author WHERE Last LIKE ? ORDER BY Last, First, Middle"));

      --  Either MyODBC or MySQL apparently does not support
      --  scrollable cursors. "Prev" is not really critical, so ignore
      --  it for now.

      SQLSetStmtAttr (T.By_Name_Statement, SQL_BIND_BY_COLUMN);
      SQLSetStmtAttr (T.By_Name_Statement, Statement_Attribute_Unsigned'(SQL_ROWSET_SIZE, 1));

      SQLBindParameter (T.By_Name_Statement, 1, T.Find_Pattern, T.Find_Pattern_Length'Access);

      ID_Binding.SQLBindCol (T.By_Name_Statement, 1, T.ID'Access, T.ID_Indicator'Access);
      SQLBindCol            (T.By_Name_Statement, 2, T.First, T.First_Length'Access);
      SQLBindCol            (T.By_Name_Statement, 3, T.Middle, T.Middle_Length'Access);
      SQLBindCol            (T.By_Name_Statement, 4, T.Last, T.Last_Length'Access);

      T.Find_Pattern (1)    := '%';
      T.Find_Pattern_Length := 1;
      T.Find_Statement      := T.By_Name_Statement;

      Checked_Execute (T.Find_Statement); --  So Next is valid.

      --  Update statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Update_Statement);
      SQLPrepare
        (T.Update_Statement,
         String'("UPDATE Author SET First = ?, Middle = ?, Last = ? WHERE ID = ?"));

      SQLBindParameter            (T.Update_Statement, 1, T.First, T.First_Length'Access);
      SQLBindParameter            (T.Update_Statement, 2, T.Middle, T.Middle_Length'Access);
      SQLBindParameter            (T.Update_Statement, 3, T.Last, T.Last_Length'Access);
      ID_Binding.SQLBindParameter (T.Update_Statement, 4, T.ID'Access, T.ID_Indicator'Access);

      --  Insert statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Insert_Statement);
      SQLPrepare
        (T.Insert_Statement,
         String'("INSERT INTO Author (First, Middle, Last) VALUES (?, ?, ?)"));

      SQLBindParameter (T.Insert_Statement, 1, T.First, T.First_Length'Access);
      SQLBindParameter (T.Insert_Statement, 2, T.Middle, T.Middle_Length'Access);
      SQLBindParameter (T.Insert_Statement, 3, T.Last, T.Last_Length'Access);

      --  Delete statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Delete_Statement);
      SQLPrepare
        (T.Delete_Statement,
         String'("DELETE FROM Author WHERE ID = ?"));

      ID_Binding.SQLBindParameter (T.Delete_Statement, 1, T.ID'Access, T.ID_Indicator'Access);

   end Initialize;

   procedure Insert
     (T           : in out Table;
      First_Name  : in     String;
      Middle_Name : in     String;
      Last_Name   : in     String)
   is begin
      Copy (T, First_Name, Middle_Name, Last_Name);
      Checked_Execute (T.Insert_Statement);
      Find_Name (T, Last_Name);
   end Insert;

   function Last_Name (T : in Table) return String is
   begin
      return T.Last (1 .. Integer (T.Last_Length));
   end Last_Name;

   function Middle_Name (T : in Table) return String is
   begin
      return T.Middle (1 .. Integer (T.Middle_Length));
   end Middle_Name;

   procedure Update
     (T           : in out Table;
      First_Name  : in     String;
      Middle_Name : in     String;
      Last_Name   : in     String)
   is begin
      Copy (T, First_Name, Middle_Name, Last_Name);
      Checked_Execute (T.Update_Statement);
   end Update;

end Books.Database.Data_Tables.Author;
