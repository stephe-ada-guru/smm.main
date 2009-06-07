--  Abstract :
--
--  Base package; import CSV data into Stephe's books database.
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

with GNU.DB.SQLCLI;
with Interfaces;
with SAL.CSV;
package Import_Books is

   Name_Field_Length  : constant := 20;
   Title_Field_Length : constant := 50;


   Environment      : GNU.DB.SQLCLI.SQLHANDLE;
   MySQL_Connection : GNU.DB.SQLCLI.SQLHANDLE;

   MySQL_DSN : constant String := "Books";

   Warm_Fuzzy_Count : Integer := 0;

   procedure Warm_Fuzzy;

   type MySQL_ID_Type is new Interfaces.Integer_32;

   package MySQL_ID_Binding   is new GNU.DB.SQLCLI.IntegerBinding (MySQL_ID_Type);
   package Int_16_Binding     is new GNU.DB.SQLCLI.IntegerBinding (Interfaces.Integer_16);
   package Unsigned_8_Binding is new GNU.DB.SQLCLI.UnsignedBinding (Interfaces.Unsigned_8);

   procedure Connect (Database_Username : in String);

   procedure Disconnect;

   function Quote
     (Item        : in String;
      Item_Length : in GNU.DB.SQLCLI.SQLINTEGER)
     return String;
   --  Return quoted Item.

   procedure Read_String
     (File         : in     SAL.CSV.File_Type;
      Column       : in     Integer;
      Value        :    out String;
      Value_Length :    out GNU.DB.SQLCLI.SQLINTEGER);

   procedure Read_Int_16
     (File      : in     SAL.CSV.File_Type;
      Column    : in     Integer;
      Value     :    out Interfaces.Integer_16;
      Indicator :    out GNU.DB.SQLCLI.SQLINTEGER);

   procedure Read_Unsigned_8
     (File      : in     SAL.CSV.File_Type;
      Column    : in     Integer;
      Value     :    out Interfaces.Unsigned_8;
      Indicator :    out GNU.DB.SQLCLI.SQLINTEGER);

end Import_Books;
