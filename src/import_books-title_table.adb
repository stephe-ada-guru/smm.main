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

package body Import_Books.Title_Table is

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Data         :    out Data_Type)
   is begin
      Read_String (File, Start_Column + 0, Data.Title.all, Data.Title_Length);
      Read_Int_16 (File, Start_Column + 1, Data.Year, Data.Year_Indicator);
      Read_String (File, Start_Column + 2, Data.Comment.all, Data.Comment_Length);
      Read_Unsigned_8 (File, Start_Column + 3, Data.Rating, Data.Rating_Indicator);
   end Read;

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Title        :    out Title_Type)
   is begin
      Read_String (File, Start_Column + 0, Title.Title.all, Title.Title_Length);
      Read_Int_16 (File, Start_Column + 1, Title.Year, Title.Year_Indicator);
   end Read;

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Title        :    out ID_Indicator_Type)
   is
      Temp : Title_Type;
   begin
      Read (File, Start_Column, Temp);
      Title := Lookup (Temp);
   end Read;

   Statement_Title : Title_Type;

   MySQL_ID : ID_Indicator_Type;

   MySQL_TY_Lookup_Statement : GNU.DB.SQLCLI.SQLHANDLE;
   MySQL_T_Lookup_Statement  : GNU.DB.SQLCLI.SQLHANDLE;
   MySQL_Quote_Statement     : GNU.DB.SQLCLI.SQLHANDLE;

   procedure Initialize
   is
      use GNU.DB.SQLCLI;
      use MySQL_ID_Binding;
      use Int_16_Binding;
      use Unsigned_8_Binding;
   begin
      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_TY_Lookup_Statement);
      --  Since there may be apostrophes in the title, which look like
      --  single quotes, quote the title. Note we need two layers!
      SQLPrepare
        (MySQL_TY_Lookup_Statement,
         String'("SELECT ID FROM Title WHERE Title=""""?"""" AND Year=?"));
      SQLBindCol (MySQL_TY_Lookup_Statement, 1, MySQL_ID.ID'Access, MySQL_ID.Indicator'Access);

      SQLBindParameter (MySQL_TY_Lookup_Statement, 1, Statement_Title.Title, Statement_Title.Title_Length'Access);
      SQLBindParameter
        (MySQL_TY_Lookup_Statement, 2, Statement_Title.Year'Access, Statement_Title.Year_Indicator'Access);

      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_T_Lookup_Statement);
      SQLPrepare
        (MySQL_T_Lookup_Statement,
         String'("SELECT ID FROM Title WHERE Title=""""?"""""));
      SQLBindCol (MySQL_T_Lookup_Statement, 1, MySQL_ID.ID'Access, MySQL_ID.Indicator'Access);

      SQLBindParameter (MySQL_T_Lookup_Statement, 1, Statement_Title.Title, Statement_Title.Title_Length'Access);

      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Quote_Statement);

      SQLPrepare (MySQL_Quote_Statement, String'("SELECT Title, Year FROM Title WHERE ID=?"));
      SQLBindCol (MySQL_Quote_Statement, 1, Statement_Title.Title, Statement_Title.Title_Length'Access);
      SQLBindCol (MySQL_Quote_Statement, 2, Statement_Title.Year'Access, Statement_Title.Year_Indicator'Access);

      SQLBindParameter (MySQL_Quote_Statement, 1, MySQL_ID.ID'Access, MySQL_ID.Indicator'Access);

   end Initialize;

   function Lookup (Title : in Title_Type) return ID_Indicator_Type
   is
      use GNU.DB.SQLCLI;
      Lookup_Statement : SQLHANDLE;
   begin
      Statement_Title.Title.all      := Title.Title.all;
      Statement_Title.Title_Length   := Title.Title_Length;
      Statement_Title.Year           := Title.Year;
      Statement_Title.Year_Indicator := Title.Year_Indicator;

      if Title.Year_Indicator = SQL_NULL_DATA then
         Lookup_Statement := MySQL_T_Lookup_Statement;
      else
         Lookup_Statement := MySQL_TY_Lookup_Statement;
      end if;

      begin
         SQLExecute (Lookup_Statement);
         SQLFetch (Lookup_Statement);
         SQLCloseCursor (Lookup_Statement);
      exception
      when No_Data =>
         raise No_Data with "Can't find " & Quote & " in Title table";
      end;

      return MySQL_ID;
   end Lookup;

   function Quote return String
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if Statement_Title.Year_Indicator = GNU.DB.SQLCLI.SQL_NULL_DATA then
         return Quote (Statement_Title.Title.all, Statement_Title.Title_Length) & ",";
      else
         return Quote (Statement_Title.Title.all, Statement_Title.Title_Length) & "," &
           Interfaces.Integer_16'Image (Statement_Title.Year);
      end if;

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

end Import_Books.Title_Table;
