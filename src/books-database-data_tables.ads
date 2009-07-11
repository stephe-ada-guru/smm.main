--  Abstract :
--
--  Operations on a base table type.
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

package Books.Database.Data_Tables is

   type Table is abstract new Books.Database.Table with private;
   type Table_Access is access all Table'Class;

   --  Class-wide operations
   procedure Delete (T : in out Table'Class);
   --  Delete the current record, move to the next according to the
   --  current find statement. Calls Clear_Data in case there is no
   --  next.

   procedure Find (T : in out Table'Class; Item : in String);
   --  Search for records with data starting with String, using
   --  current find statement. Fetch first.
   --
   --  If there is no match, current data is unchanged.

   function Valid (T : in Table'Class) return Boolean;
   --  True if current data is valid (Find or Fetch did not raise No_Data)

   function ID (T : in Table'Class) return ID_Type;
   --  Return ID of current record.

   procedure Fetch (T : in out Table'Class; ID : in ID_Type);
   --  Fetch record by ID.

   ----------
   --  Dispatching operations

   --  Initialize should create database access statements, fetch first record.

   overriding procedure Finalize (T : in out Table);
   --  Free all statements. Root version frees common statements.

   overriding procedure Clear_Data (T : in out Table) is abstract;
   --  Erase local data.

private

   type Table is abstract new Books.Database.Table with record

      --  Data
      ID            : aliased ID_Type    := Invalid_ID;
      ID_Indicator  : aliased GNU.DB.SQLCLI.SQLINTEGER := GNU.DB.SQLCLI.SQL_NULL_DATA;

      By_ID_Statement   : GNU.DB.SQLCLI.SQLHANDLE := GNU.DB.SQLCLI.SQL_NULL_HANDLE;
      By_Name_Statement : GNU.DB.SQLCLI.SQLHANDLE := GNU.DB.SQLCLI.SQL_NULL_HANDLE;
   end record;

end Books.Database.Data_Tables;
