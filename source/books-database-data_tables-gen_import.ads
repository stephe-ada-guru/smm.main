--  Abstract :
--
--  Generic table import
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with SAL.CSV;
generic
   Table_Name   : in String;
   Column_Count : in Integer;

   with procedure Read_Insert_Find (File : in out SAL.CSV.File_Type);
   --  Read a line from Input, insert into appropriate table. Ignore insert error (assume duplicate).
   --  Fetch matching record (just inserted or previously inserted).
   --  Add ID to appropriate map and link tables.

procedure Books.Database.Data_Tables.Gen_Import
  (Root_File_Name : in String);
