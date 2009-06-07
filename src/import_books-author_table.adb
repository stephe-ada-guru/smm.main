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

package body Import_Books.Author_Table is

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Name         :    out Name_Type)
   is begin
      Read_String (File, Start_Column + 0, Name.First.all, Name.First_Length);
      Read_String (File, Start_Column + 1, Name.Middle.all, Name.Middle_Length);
      Read_String (File, Start_Column + 2, Name.Last.all, Name.Last_Length);
   end Read;

   MySQL_ID           : aliased MySQL_ID_Type := 0;
   MySQL_ID_Indicator : aliased GNU.DB.SQLCLI.SQLINTEGER    := 0;

   MySQL_Lookup_FML_Statement : GNU.DB.SQLCLI.SQLHANDLE;
   MySQL_Lookup_FL_Statement  : GNU.DB.SQLCLI.SQLHANDLE;
   MySQL_Quote_Statement      : GNU.DB.SQLCLI.SQLHANDLE;

   Statement_Name : Name_Type;

   procedure Initialize
   is
      use GNU.DB.SQLCLI;
      use MySQL_ID_Binding;
   begin
      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Lookup_FML_Statement);
      SQLPrepare
        (MySQL_Lookup_FML_Statement,
         String'("SELECT ID FROM Author" &
                   " WHERE First=? AND Middle=? AND Last=?" &
                   " ORDER BY First, Middle, Last"));

      SQLBindCol (MySQL_Lookup_FML_Statement, 1, MySQL_ID'Access, MySQL_ID_Indicator'Access);

      SQLBindParameter (MySQL_Lookup_FML_Statement, 1, Statement_Name.First, Statement_Name.First_Length'Access);
      SQLBindParameter (MySQL_Lookup_FML_Statement, 2, Statement_Name.Middle, Statement_Name.Middle_Length'Access);
      SQLBindParameter (MySQL_Lookup_FML_Statement, 3, Statement_Name.Last, Statement_Name.Last_Length'Access);

      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Lookup_FL_Statement);
      SQLPrepare
        (MySQL_Lookup_FL_Statement,
         String'("SELECT ID FROM Author" &
                   " WHERE First=? AND Last=?" &
                   " ORDER BY First, Middle, Last"));

      SQLBindCol (MySQL_Lookup_FL_Statement, 1, MySQL_ID'Access, MySQL_ID_Indicator'Access);

      SQLBindParameter (MySQL_Lookup_FL_Statement, 1, Statement_Name.First, Statement_Name.First_Length'Access);
      SQLBindParameter (MySQL_Lookup_FL_Statement, 2, Statement_Name.Last, Statement_Name.Last_Length'Access);

      SQLAllocHandle (SQL_HANDLE_STMT, MySQL_Connection, MySQL_Quote_Statement);

      SQLPrepare (MySQL_Quote_Statement, String'("SELECT First, Middle, Last FROM Author WHERE ID=?"));
      SQLBindCol (MySQL_Quote_Statement, 1, Statement_Name.First, Statement_Name.First_Length'Access);
      SQLBindCol (MySQL_Quote_Statement, 2, Statement_Name.Middle, Statement_Name.Middle_Length'Access);
      SQLBindCol (MySQL_Quote_Statement, 3, Statement_Name.Last, Statement_Name.Last_Length'Access);

      SQLBindParameter (MySQL_Quote_Statement, 1, MySQL_ID'Access, MySQL_ID_Indicator'Access);

   end Initialize;

   function Lookup (Name : in Name_Type) return MySQL_ID_Type
   is
      use GNU.DB.SQLCLI;
      Lookup_Statement : SQLHANDLE;
   begin
      --  prepared statements have original string pointers; preseve them
      Statement_Name.First.all     := Name.First.all;
      Statement_Name.First_Length  := Name.First_Length;
      Statement_Name.Middle.all    := Name.Middle.all;
      Statement_Name.Middle_Length := Name.Middle_Length;
      Statement_Name.Last.all      := Name.Last.all;
      Statement_Name.Last_Length   := Name.Last_Length;

      --  If a string is empty, the length is returned as -1, but must
      --  be 0 for the next SQL statement. Also we apparently need to
      --  replace the null terminator with a space.
      if Name.First_Length < 0 then
         raise No_Data with "null first name";
      end if;

      if Name.Middle_Length < 0 then
         Lookup_Statement := MySQL_Lookup_FL_Statement;
      else
         Lookup_Statement := MySQL_Lookup_FML_Statement;
      end if;

      if Name.Last_Length < 0 then
         raise No_Data with "null last name";
      end if;

      --  Fetch ID from MySQL DB
      begin
         SQLExecute (Lookup_Statement);
         SQLFetch (Lookup_Statement);
         SQLCloseCursor (Lookup_Statement);
      exception
      when No_Data =>
         raise No_Data with "Can't find " & Quote & " in Author table";
      end;

      return MySQL_ID;
   end Lookup;

   function Quote return String
   is begin
      return Quote (Statement_Name.First.all, Statement_Name.First_Length) & "," &
        Quote (Statement_Name.Middle.all, Statement_Name.Middle_Length) & "," &
        Quote (Statement_Name.Last.all, Statement_Name.Last_Length);
   end Quote;

   function Quote (ID : in MySQL_ID_Type) return String
   is
      use GNU.DB.SQLCLI;
   begin
      Author_Table.MySQL_ID := ID;
      SQLExecute (MySQL_Quote_Statement);
      SQLFetch (MySQL_Quote_Statement);
      SQLCloseCursor (MySQL_Quote_Statement);
      return Quote;
   end Quote;

end Import_Books.Author_Table;
