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
package body Books.Database.Data_Tables is

   --  Subprogram bodies (alphabetical order)

   procedure Delete (T : in out Table'Class)
   is begin
      Checked_Execute (T.Delete_Statement);
      begin
         Next (T);
      exception
      when No_Data =>
         --  Empty table
         null;
      end;
   end Delete;

   procedure Fetch (T : in out Table'Class; ID : in ID_Type)
   is
      use type GNU.DB.SQLCLI.SQLHANDLE;
   begin
      T.ID           := ID;
      T.ID_Indicator := 4; -- That's what Find sets it to on success.

      Find (T, T.By_ID_Statement);
   end Fetch;

   overriding procedure Finalize (T : in out Table)
   is
      use GNU.DB.SQLCLI;
   begin
      Books.Database.Finalize (Books.Database.Table (T));

      if T.By_ID_Statement /= SQL_NULL_HANDLE then
         SQLFreeHandle (SQL_HANDLE_STMT, T.By_ID_Statement);
      end if;
      if T.By_Name_Statement /= SQL_NULL_HANDLE then
         SQLFreeHandle (SQL_HANDLE_STMT, T.By_Name_Statement);
      end if;
   end Finalize;

   procedure Set_Find_By_ID (T : in out Table'Class)
   is begin
      T.Find_Statement := T.By_ID_Statement;
   end Set_Find_By_ID;

   procedure Set_Find_By_Name (T : in out Table'Class)
   is begin
      T.Find_Statement := T.By_Name_Statement;
   end Set_Find_By_Name;

   procedure Find (T : in out Table'Class; Item : in String)
   is
      use Ada.Strings.Fixed;
      use type GNU.DB.SQLCLI.SQLINTEGER;
      Field_Length : constant Integer := T.Find_Pattern'Length - 1; -- for '%'
   begin
      Move
        (Source => Item,
         Target => T.Find_Pattern (1 .. Field_Length),
         Drop   => Ada.Strings.Right);

      T.Find_Pattern_Length := GNU.DB.SQLCLI.SQLINTEGER (Integer'Min (Item'Length + 1, Field_Length));
      T.Find_Pattern (Integer (T.Find_Pattern_Length)) := '%';

      Find (T, T.By_Name_Statement);
   end Find;

   function ID (T : in Table'Class) return ID_Type
   is
      use type GNU.DB.SQLCLI.SQLINTEGER;
   begin
      if T.ID_Indicator = GNU.DB.SQLCLI.SQL_NULL_DATA then
         raise No_Data;
      else
         return T.ID;
      end if;
   end ID;

end Books.Database.Data_Tables;
