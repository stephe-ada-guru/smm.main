--  Abstract :
--
--  Operations on the Author table
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

package Import_Books.Author_Table is

   type Name_Type is record
      First         : access String := new String (1 .. Name_Field_Length + 1); -- Null termination
      First_Length  : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Middle        : access String := new String (1 .. Name_Field_Length + 1);
      Middle_Length : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
      Last          : access String := new String (1 .. Name_Field_Length + 1);
      Last_Length   : aliased GNU.DB.SQLCLI.SQLINTEGER := 0;
   end record;

   --  CSV operations

   procedure Read
     (File         : in     SAL.CSV.File_Type;
      Start_Column : in     Integer;
      Name         :    out Name_Type);
   --  Read an author name from current line in File, starting at Start_Column

   ----------
   --  Database operations

   procedure Initialize;
   --  Set up queries. Must be called once before first call to
   --  Lookup.

   function Lookup (Name : in Name_Type) return MySQL_ID_Type;
   --  Return the Author ID in the database.

   function Quote return String;
   --  Return name from last operation, for error message

   function Quote (ID : in MySQL_ID_Type) return String;
   --  Return name from MySQL database, as quoted comma separated
   --  values.

end Import_Books.Author_Table;
