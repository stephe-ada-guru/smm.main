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

with Ada.Strings.Fixed;
with GNU.DB.SQLCLI.Statement_Attribute;
package body Books.Database.Data_Tables.Title is

   --  Local declarations

   procedure Copy
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Interfaces.Unsigned_8;
      Rating_Valid : in     Boolean);
   --  Copy Data to Table fields.

   ----------
   --  Subprogram bodies (alphabetical order)

   function Comment (T : in Table) return String is
   begin
      return T.Comment (1 .. Integer (T.Comment_Length));
   end Comment;

   function Comment (T : in Data_Tables.Table_Access) return String
   is begin
      return Comment (Table (T.all));
   end Comment;

   procedure Copy
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Interfaces.Unsigned_8;
      Rating_Valid : in     Boolean)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      Move (Source => Title, Target => T.Title.all, Drop => Right);
      T.Title_Length := GNU.DB.SQLCLI.SQLINTEGER'Min (T.Title.all'Length, Title'Length);

      if Year_Valid then
         T.Year           := Year;
         T.Year_Indicator := Interfaces.Unsigned_16'Size / 8;
      else
         T.Year_Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA;
      end if;

      Move (Source => Comment, Target => T.Comment.all, Drop => Right);
      T.Comment_Length := GNU.DB.SQLCLI.SQLINTEGER'Min (T.Comment.all'Length, Comment'Length);
      if T.Comment_Length = 0 then
         T.Comment_Length := GNU.DB.SQLCLI.SQL_NULL_DATA;
      end if;

      if Rating_Valid then
         T.Rating           := Rating;
         T.Rating_Indicator := Interfaces.Unsigned_8'Size / 8;
      else
         T.Rating_Indicator := GNU.DB.SQLCLI.SQL_NULL_DATA;
      end if;
   end Copy;

   procedure Find_Title (T : in out Table; Item : in String)
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      Ada.Strings.Fixed.Move
        (Source => Item,
         Target => T.Find_Pattern.all (1 .. Field_Length),
         Drop   => Ada.Strings.Right);

      T.Find_Pattern (Integer'Min (T.Find_Pattern'Last, Item'Length + 1)) := '%';
      T.Find_Pattern_Length := Item'Length + 1;
      GNU.DB.SQLCLI.SQLCloseCursor (T.By_Name_Statement);
      Checked_Execute (T.By_Name_Statement);
      T.Find_Statement := T.By_Name_Statement;
      Next (T);
   end Find_Title;

   procedure Find_Title (T : in Data_Tables.Table_Access; Item : in String)
   is begin
      Find_Title (Table (T.all), Item);
   end Find_Title;

   overriding procedure Initialize (T : in out Table)
   is
      use GNU.DB.SQLCLI;
      use GNU.DB.SQLCLI.Statement_Attribute;
   begin
      if T.Title = null then
         T.Title        := new String'(1 .. Field_Length => ' ');
         T.Comment      := new String'(1 .. Field_Length => ' ');
         T.Find_Pattern := new String'(1 .. Field_Length + 1 => ' '); -- for '%'
      end if;

      --  All_By_ID_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.All_By_ID_Statement);
      SQLPrepare
        (T.All_By_ID_Statement,
         String'("SELECT Title, Year, Comment, Rating FROM Title ORDER BY ID"));

      SQLBindCol                     (T.All_By_ID_Statement, 1, T.Title, T.Title_Length'Access);
      Unsigned_16_Binding.SQLBindCol (T.All_By_ID_Statement, 2, T.Year'Access, T.Year_Indicator'Access);
      SQLBindCol                     (T.All_By_ID_Statement, 3, T.Comment, T.Comment_Length'Access);
      Unsigned_8_Binding.SQLBindCol  (T.All_By_ID_Statement, 4, T.Rating'Access, T.Rating_Indicator'Access);

      --  By_ID_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_ID_Statement);
      SQLPrepare
        (T.By_ID_Statement,
         String'("SELECT ID, Title, Year, Comment, Rating FROM Title WHERE ID = ?"));

      ID_Binding.SQLBindParameter (T.By_ID_Statement, 1, T.ID'Access, T.ID_Indicator'Access);

      ID_Binding.SQLBindCol          (T.By_ID_Statement, 1, T.ID'Access, T.ID_Indicator'Access);
      SQLBindCol                     (T.By_ID_Statement, 2, T.Title, T.Title_Length'Access);
      Unsigned_16_Binding.SQLBindCol (T.By_ID_Statement, 3, T.Year'Access, T.Year_Indicator'Access);
      SQLBindCol                     (T.By_ID_Statement, 4, T.Comment, T.Comment_Length'Access);
      Unsigned_8_Binding.SQLBindCol  (T.By_ID_Statement, 5, T.Rating'Access, T.Rating_Indicator'Access);

      --  By_Name_Statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_Name_Statement);
      SQLPrepare
        (T.By_Name_Statement,
         String'("SELECT ID, Title, Year, Comment, Rating FROM Title WHERE Title LIKE ? ORDER BY Title"));

      SQLSetStmtAttr (T.By_Name_Statement, SQL_BIND_BY_COLUMN);
      SQLSetStmtAttr (T.By_Name_Statement, Statement_Attribute_Unsigned'(SQL_ROWSET_SIZE, 1));

      SQLBindParameter (T.By_Name_Statement, 1, T.Find_Pattern, T.Find_Pattern_Length'Access);

      ID_Binding.SQLBindCol          (T.By_Name_Statement, 1, T.ID'Access, T.ID_Indicator'Access);
      SQLBindCol                     (T.By_Name_Statement, 2, T.Title, T.Title_Length'Access);
      Unsigned_16_Binding.SQLBindCol (T.By_Name_Statement, 3, T.Year'Access, T.Year_Indicator'Access);
      SQLBindCol                     (T.By_Name_Statement, 4, T.Comment, T.Comment_Length'Access);
      Unsigned_8_Binding.SQLBindCol  (T.By_Name_Statement, 5, T.Rating'Access, T.Rating_Indicator'Access);

      T.Find_Pattern (1)    := '%';
      T.Find_Pattern_Length := 1;
      T.Find_Statement      := T.By_Name_Statement;

      Checked_Execute (T.Find_Statement); --  So Next is valid.

      --  Update statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Update_Statement);
      SQLPrepare
        (T.Update_Statement,
         String'("UPDATE Title SET Title = ?, Year = ?, Comment = ?, Rating = ? WHERE ID = ?"));

      SQLBindParameter                     (T.Update_Statement, 1, T.Title, T.Title_Length'Access);
      Unsigned_16_Binding.SQLBindParameter (T.Update_Statement, 2, T.Year'Access, T.Year_Indicator'Access);
      SQLBindParameter                     (T.Update_Statement, 3, T.Comment, T.Comment_Length'Access);
      Unsigned_8_Binding.SQLBindParameter  (T.Update_Statement, 4, T.Rating'Access, T.Rating_Indicator'Access);
      ID_Binding.SQLBindParameter          (T.Update_Statement, 5, T.ID'Access, T.ID_Indicator'Access);

      --  Insert statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Insert_Statement);
      SQLPrepare
        (T.Insert_Statement,
         String'("INSERT INTO Title (Title, Year, Comment, Rating) VALUES (?, ?, ?, ?)"));

      SQLBindParameter                     (T.Insert_Statement, 1, T.Title, T.Title_Length'Access);
      Unsigned_16_Binding.SQLBindParameter (T.Insert_Statement, 2, T.Year'Access, T.Year_Indicator'Access);
      SQLBindParameter                     (T.Insert_Statement, 3, T.Comment, T.Comment_Length'Access);
      Unsigned_8_Binding.SQLBindParameter  (T.Insert_Statement, 4, T.Rating'Access, T.Rating_Indicator'Access);

      --  Delete statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Delete_Statement);
      SQLPrepare
        (T.Delete_Statement,
         String'("DELETE FROM Title WHERE ID = ?"));

      ID_Binding.SQLBindParameter (T.Delete_Statement, 1, T.ID'Access, T.ID_Indicator'Access);

   end Initialize;

   procedure Insert
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Interfaces.Unsigned_8;
      Rating_Valid : in     Boolean)
   is begin
      Copy (T, Title, Year, Year_Valid, Comment, Rating, Rating_Valid);
      Checked_Execute (T.Insert_Statement);
      Find_Title (T, Title);
   end Insert;

   function Rating (T : in Table) return Interfaces.Unsigned_8
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if T.Rating_Indicator = GNU.DB.SQLCLI.SQL_NULL_DATA then
         raise No_Data;
      else
         return Interfaces.Unsigned_8 (T.Rating);
      end if;
   end Rating;

   function Rating (T : in Data_Tables.Table_Access) return Interfaces.Unsigned_8
   is begin
      return Rating (Table (T.all));
   end Rating;

   function Rating_Valid (T : in Data_Tables.Table_Access) return Boolean
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      return Table (T.all).Rating_Indicator /= GNU.DB.SQLCLI.SQL_NULL_DATA;
   end Rating_Valid;

   function Title (T : in Table) return String is
   begin
      return T.Title (1 .. Integer (T.Title_Length));
   end Title;

   function Title (T : in Data_Tables.Table_Access) return String
   is begin
      return Title (Table (T.all));
   end Title;

   procedure Update
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Interfaces.Unsigned_8;
      Rating_Valid : in     Boolean)
   is begin
      Copy (T, Title, Year, Year_Valid, Comment, Rating, Rating_Valid);
      Checked_Execute (T.Update_Statement);
   end Update;

   function Year (T : in Table) return Interfaces.Unsigned_16
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if T.ID_Indicator = GNU.DB.SQLCLI.SQL_NULL_DATA then
         raise No_Data;
      else
         return Interfaces.Unsigned_16 (T.Year);
      end if;
   end Year;

   function Year (T : in Data_Tables.Table_Access) return Interfaces.Unsigned_16
   is begin
      return Year (Table (T.all));
   end Year;

end Books.Database.Data_Tables.Title;
