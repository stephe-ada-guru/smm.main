--  Abstract :
--
--  Operations on the Collection table
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

with Interfaces;
package Books.Database.Data_Tables.Collection is

   type Table (DB : access Database'Class) is new Data_Tables.Table with private;
   type Table_Access is access all Table;

   ----------
   --  Override parent operations.

   overriding procedure Initialize (T : in out Table);
   overriding procedure Finalize (T : in out Table);

   ----------
   --  New operations

   function Name (T : in Table) return String;
   function Name (T : in Data_Tables.Table_Access) return String;

   function Editor (T : in Table) return ID_Type;
   function Editor (T : in Data_Tables.Table_Access) return ID_Type;
   function Editor_Valid (T : in Data_Tables.Table_Access) return Boolean;

   function Year (T : in Table) return Interfaces.Unsigned_16;
   function Year (T : in Data_Tables.Table_Access) return Interfaces.Unsigned_16;
   function Year_Valid (T : in Data_Tables.Table_Access) return Boolean;

   --  Retrieve data from current record

   procedure Find_Editor (T : in out Table; Editor : in ID_Type);
   procedure Find_Editor (T : in Data_Tables.Table_Access; Editor : in ID_Type);
   --  Find records with Editor_ID = Editor.

   procedure Insert
     (T            : in out Table;
      Name         : in     String;
      Editor       : in     ID_Type;
      Editor_Valid : in     Boolean;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean);
   --  Insert a new record, fetch it using Find_Name.

   procedure Update
     (T            : in out Table;
      Name         : in     String;
      Editor       : in     ID_Type;
      Editor_Valid : in     Boolean;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean);
   --  Update the data in the current record.

private

   Field_Length : constant := 50;

   type Table (DB : access Database'Class) is new Data_Tables.Table (DB => DB) with record

      --  Data
      Name             : String_Access;
      Name_Length      : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Editor           : aliased ID_Type;
      Editor_Indicator : aliased GNU.DB.SQLCLI.SQLINTEGER := GNU.DB.SQLCLI.SQL_NULL_DATA;
      Year             : aliased Interfaces.Unsigned_16;
      Year_Indicator   : aliased GNU.DB.SQLCLI.SQLINTEGER := GNU.DB.SQLCLI.SQL_NULL_DATA;

      By_Editor_Statement : GNU.DB.SQLCLI.SQLHANDLE;
   end record;

end Books.Database.Data_Tables.Collection;
