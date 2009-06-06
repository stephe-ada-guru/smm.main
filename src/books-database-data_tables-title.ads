--  Abstract :
--
--  Operations on the Title table
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
package Books.Database.Data_Tables.Title is

   type Table (DB : access Database'Class) is new Data_Tables.Table with private;
   type Table_Access is access all Table;

   ----------
   --  Override parent operations.

   procedure Initialize (T : in out Table);
   procedure Clear_Data (T : in out Table);

   ----------
   --  New operations

   function Title (T : in Table) return String;
   function Year (T : in Table) return Interfaces.Unsigned_16;
   function Comment (T : in Table) return String;
   function Rating (T : in Table) return Interfaces.Unsigned_8;
   --  Retrieve data from current record

   procedure Find_Title (T : in out Table; Item : in String);
   --  Search for records with titles starting with String. Fetch
   --  first.
   --
   --  If there is no match, current data is unchanged.

   procedure Insert
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Interfaces.Unsigned_8;
      Rating_Valid : in     Boolean);
   --  Insert a new record, fetch it using Find_Title.

   procedure Update
     (T            : in out Table;
      Title        : in     String;
      Year         : in     Interfaces.Unsigned_16;
      Year_Valid   : in     Boolean;
      Comment      : in     String;
      Rating       : in     Interfaces.Unsigned_8;
      Rating_Valid : in     Boolean);
   --  Update the data in the current record.

private

   Field_Length : constant := 50;

   type Table (DB : access Database'Class) is new Data_Tables.Table (DB => DB) with record

      --  Data
      Title            : String_Access;
      Title_Length     : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Year             : aliased Interfaces.Unsigned_16;
      Year_Indicator   : aliased GNU.DB.SQLCLI.SQLINTEGER := GNU.DB.SQLCLI.SQL_NULL_DATA;
      Comment          : String_Access;
      Comment_Length   : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Rating           : aliased Interfaces.Unsigned_8;
      Rating_Indicator : aliased GNU.DB.SQLCLI.SQLINTEGER := GNU.DB.SQLCLI.SQL_NULL_DATA;

   end record;

end Books.Database.Data_Tables.Title;
