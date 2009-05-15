--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with GNU.DB.SQLCLI.Statement_Attribute;
package body Books.Database.Gen_Link_Tables is

   procedure Clear_Data (T : in out Table)
   is begin
      T.Data      := (others      => 0);
      T.Indicator := (others => SQL_NULL_DATA);
   end Clear_Data;

   procedure Delete (T : in out Table; Data : in Source_Array_ID_Type)
   is begin
      T.Data      := Data;
      T.Indicator := (others => Source_Labels_Type'Size / 8);
      Checked_Execute (T.Delete_Statement);
   end Delete;

   procedure Fetch_Links_Of (T : in out Table; Source : in Source_Labels_Type; Item : in ID_Type)
   is begin
      T.Data (Source)      := Item;
      T.Indicator (Source) := Source_Labels_Type'Size / 8;
      SQLCloseCursor (T.By_Source_Statement (Source));
      Checked_Execute (T.By_Source_Statement (Source));
      T.Find_Statement     := T.By_Source_Statement (Source);
      Next (T);
   end Fetch_Links_Of;

   function ID (T : in Table; Source : in Source_Labels_Type) return ID_Type is
   begin
      if T.Indicator (Source) = SQL_NULL_DATA then
         return 0;
      else
         return T.Data (Source);
      end if;
   end ID;

   procedure Initialize (T : in out Table)
   is
      use Statement_Attribute;
      Table_Name : constant String :=
        Source_Labels_Type'Image (Source_Labels_Type'First) &
        Source_Labels_Type'Image (Source_Labels_Type'Last);

      First_Column_Name : constant String := Source_Labels_Type'Image (Source_Labels_Type'First);
      Last_Column_Name  : constant String := Source_Labels_Type'Image (Source_Labels_Type'Last);
   begin

      --  Find statements
      for I in Source_Labels_Type loop
         SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.By_Source_Statement (I));
         SQLPrepare
           (T.By_Source_Statement (I),
            String'
              ("SELECT " &
                 First_Column_Name &
                 ", " &
                 Last_Column_Name &
                 " FROM " &
                 Table_Name &
                 " WHERE " &
                 Source_Labels_Type'Image (I) &
                 " = ?"));

         SQLSetStmtAttr (T.By_Source_Statement (I), Statement_Attribute_Unsigned'(SQL_ROWSET_SIZE, 1));

         ID_Binding.SQLBindParameter (T.By_Source_Statement (I), 1, T.Data (I)'Access, T.Indicator (I)'Access);

         ID_Binding.SQLBindCol
           (T.By_Source_Statement (I),
            1,
            T.Data (Source_Labels_Type'First)'Access,
            T.Indicator (Source_Labels_Type'First)'Access);

         ID_Binding.SQLBindCol
           (T.By_Source_Statement (I),
            2,
            T.Data (Source_Labels_Type'Last)'Access,
            T.Indicator (Source_Labels_Type'Last)'Access);

      end loop;

      --  Insert statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Insert_Statement);
      SQLPrepare
        (T.Insert_Statement,
         String'
           ("INSERT INTO " &
              Table_Name &
              "(" &
              First_Column_Name &
              ", " &
              Last_Column_Name &
              ") VALUES (?, ?)"));

      ID_Binding.SQLBindParameter
        (T.Insert_Statement,
         1,
         T.Data (Source_Labels_Type'First)'Access,
         T.Indicator (Source_Labels_Type'First)'Access);

      ID_Binding.SQLBindParameter
        (T.Insert_Statement,
         2,
         T.Data (Source_Labels_Type'Last)'Access,
         T.Indicator (Source_Labels_Type'Last)'Access);

      --  Delete statement
      SQLAllocHandle (SQL_HANDLE_STMT, T.DB.Connection, T.Delete_Statement);
      SQLPrepare
        (T.Delete_Statement,
         String'("DELETE FROM " &
                   Table_Name &
                   " WHERE " &
                   First_Column_Name & " = ? AND " &
                   Last_Column_Name & " = ?"));

      ID_Binding.SQLBindParameter
        (T.Delete_Statement,
         1,
         T.Data (Source_Labels_Type'First)'Access,
         T.Indicator (Source_Labels_Type'First)'Access);

      ID_Binding.SQLBindParameter
        (T.Delete_Statement,
         2,
         T.Data (Source_Labels_Type'Last)'Access,
         T.Indicator (Source_Labels_Type'Last)'Access);

   end Initialize;

   procedure Insert (T : in out Table; Data : in Source_Array_ID_Type)
   is begin
      T.Data      := Data;
      T.Indicator := (others => Source_Labels_Type'Size / 8);
      Checked_Execute (T.Insert_Statement);
   end Insert;

end Books.Database.Gen_Link_Tables;
