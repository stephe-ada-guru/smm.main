--  Abstract :
--
--  Operations on the Title table
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

package Import_Books.Title_Table is

   type Data_Type is record
      Title            : access String := new String (1 .. Title_Field_Length + 1);
      Title_Length     : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Year             : aliased Interfaces.Integer_16;
      Year_Indicator   : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Comment          : access String := new String (1 .. Title_Field_Length + 1);
      Comment_Length   : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Rating           : aliased Interfaces.Unsigned_8;
      Rating_Indicator : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
   end record;

   type Title_Type is record
      Title          : access String := new String (1 .. Title_Field_Length + 1);
      Title_Length   : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Year           : aliased Interfaces.Integer_16;
      Year_Indicator : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
   end record;

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Data         :    out Data_Type);
   --  Read title data from current line in File, starting at Start_Column

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Title        :    out Title_Type);
   --  Read just title, year from current line in File, starting at Start_Column

   ----------
   --  Database operations

   procedure Initialize;
   --  Set up queries. Must be called once before first call to
   --  Lookup.

   function Lookup (Title : in Title_Type) return MySQL_ID_Type;
   --  Return the Title ID for Title in the database.

   function Quote return String;
   --  Return title, year from last operation, as quoted comma
   --  separated values.

   function Quote (MySQL_ID : in MySQL_ID_Type) return String;
   --  Return title, year from MySQL database, as quoted comma
   --  separated values.

end Import_Books.Title_Table;
