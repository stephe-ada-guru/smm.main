--  Abstract :
--
--  Operations on the Collection table.
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

package Import_Books.Collection_Table is

   type Data_Type is record
      Name             : access String                    := new String (1 .. Title_Field_Length);
      Name_Length      : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Editor           : ID_Indicator_Type;
      Year             : aliased Interfaces.Integer_16;
      Year_Indicator   : aliased GNU.DB.SQLCLI.SQLINTEGER := GNU.DB.SQLCLI.SQL_NULL_DATA;
   end record;

   type Name_Editor_Type is record
      Name             : access String                    := new String (1 .. Title_Field_Length);
      Name_Length      : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Editor           : ID_Indicator_Type;
   end record;

   --  CSV operations

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Data         :    out Data_Type);
   --  Read collection data from current line in File, starting at
   --  Start_Column

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Name_Editor  :    out Name_Editor_Type);
   --  Read collection, editor name from current line in File,
   --  starting at Start_Column

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Collection   :    out ID_Indicator_Type);
   --  Read collection, editor name from current line in File,
   --  starting at Start_Column; lookup collection ID.

   ----------
   --  Database operations

   procedure Initialize;
   --  Set up queries. Must be called once before first call to
   --  Lookup.

   function Lookup (Name_Editor : in Name_Editor_Type) return ID_Indicator_Type;
   --  Return the collection ID for Name_Editor.

   function Quote return String;
   --  Return collection name and editor from last operation, as quoted
   --  comma separated values.

   function Quote (ID : in ID_Indicator_Type) return String;
   --  Return collection name and editor from database, as quoted
   --  comma separated values.

end Import_Books.Collection_Table;
