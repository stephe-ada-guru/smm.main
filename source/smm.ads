--  Abstract :
--
--  Root of Stephe's Music Manager packages
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

package SMM is

   Verbosity : Integer;

   function Relative_Name_Sans_Extension (Root : in String; Full_Name : in String) return String;

   --  database keys
   Category_Key        : constant String := "Category";
   File_Key            : constant String := "File";
   Last_Downloaded_Key : constant String := "Last_Downloaded";
   Songs_Key           : constant String := "Songs";
   Root_Key            : constant String := "Root";
end SMM;
