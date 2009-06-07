--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2002, 2009 Stephen Leake.  All Rights Reserved.
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

with Import_Books.Author_Table;
package body Import_Books.Series_Table is

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Data         :    out Data_Type)
   is
      Author : Author_Table.Name_Type;
   begin
      Read_String (File, Start_Column + 0, Data.Title.all, Data.Title_Length);
      Author_Table.Read (File, Start_Column + 1, Author);
      Data.Author := Author_Table.Lookup (Author, Exception_On_Null => False);
   end Read;

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Series       :    out ID_Indicator_Type)
   is
      Temp : Data_Type;
   begin
      Read (File, Start_Column, Temp);
      Series := Lookup (Temp);
   end Read;

   Statement_Data : Data_Type;

   MySQL_ID : ID_Indicator_Type;

   MySQL_TA_Lookup_Statement : GNU.DB.SQLCLI.SQLHANDLE;
   MySQL_T_Lookup_Statement  : GNU.DB.SQLCLI.SQLHANDLE;
   MySQL_Quote_Statement     : GNU.DB.SQLCLI.SQLHANDLE;

   procedure Initialize
   is
      use GNU.DB.SQLCLI;
      use MySQL_ID_Binding;
   begin
      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_TA_Lookup_Statement);
      SQLPrepare (MySQL_TA_Lookup_Statement, String'("SELECT ID FROM Series WHERE Title=? AND Author=?"));
      SQLBindCol (MySQL_TA_Lookup_Statement, 1, MySQL_ID.ID'Access, MySQL_ID.Indicator'Access);

      SQLBindParameter (MySQL_TA_Lookup_Statement, 1, Statement_Data.Title, Statement_Data.Title_Length'Access);
      SQLBindParameter
        (MySQL_TA_Lookup_Statement, 2, Statement_Data.Author.ID'Access, Statement_Data.Author.Indicator'Access);

      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_T_Lookup_Statement);
      SQLPrepare (MySQL_T_Lookup_Statement, String'("SELECT ID FROM Series WHERE Title=?"));
      SQLBindCol (MySQL_T_Lookup_Statement, 1, MySQL_ID.ID'Access, MySQL_ID.Indicator'Access);

      SQLBindParameter (MySQL_T_Lookup_Statement, 1, Statement_Data.Title, Statement_Data.Title_Length'Access);

      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Quote_Statement);

      SQLPrepare (MySQL_Quote_Statement, String'("SELECT Title, Author FROM Series WHERE ID=?"));
      SQLBindCol (MySQL_Quote_Statement, 1, Statement_Data.Title, Statement_Data.Title_Length'Access);
      SQLBindCol (MySQL_Quote_Statement, 2, Statement_Data.Author.ID'Access, Statement_Data.Author.Indicator'Access);

      SQLBindParameter (MySQL_Quote_Statement, 1, MySQL_ID.ID'Access, MySQL_ID.Indicator'Access);

   end Initialize;

   function Lookup (Series : in Data_Type) return ID_Indicator_Type
   is
      use GNU.DB.SQLCLI;
      Lookup_Statement : SQLHANDLE;
   begin
      Statement_Data.Title.all    := Series.Title.all;
      Statement_Data.Title_Length := Series.Title_Length;
      Statement_Data.Author       := Series.Author;

      if Statement_Data.Author.Indicator = SQL_NULL_DATA then
         Lookup_Statement := MySQL_T_Lookup_Statement;
      else
         Lookup_Statement := MySQL_TA_Lookup_Statement;
      end if;

      begin
         SQLExecute (Lookup_Statement);
         SQLFetch (Lookup_Statement);
         SQLCloseCursor (Lookup_Statement);
      exception
      when No_Data =>
         raise No_Data with "Can't find " & Quote & " in MySQL Series table";
      end;

      return MySQL_ID;
   end Lookup;

   function Quote return String
   is begin
      return Quote (Statement_Data.Title.all, Statement_Data.Title_Length) & "," &
        Author_Table.Quote (Statement_Data.Author);
   end Quote;

   function Quote (ID : in ID_Indicator_Type) return String
   is
      use GNU.DB.SQLCLI;
   begin
      MySQL_ID := ID;
      SQLExecute (MySQL_Quote_Statement);
      SQLFetch (MySQL_Quote_Statement);
      SQLCloseCursor (MySQL_Quote_Statement);
      return Quote;
   end Quote;

end Import_Books.Series_Table;
