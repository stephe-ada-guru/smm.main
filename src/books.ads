--  Abstract :
--
--  Root of Stephe's Books Database Interface.
--
--  Copyright (C) 2002, 2009, 2012 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

limited with Books.Database.Data_Tables;
limited with Books.Database.Link_Tables;
package Books is
   pragma Pure;

   function Version return String;

   type Table_Names is (Author, Collection, Series, Title);

   function Image (Item : in Table_Names) return String;
   --  With proper capitalization.

   type Table_Arrays is array (Table_Names) of access Books.Database.Data_Tables.Table'Class;

   type Link_Arrays is array (Table_Names, Table_Names) of access Books.Database.Link_Tables.Table'Class;

end Books;
