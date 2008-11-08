--  Abstract :
--
--  see spec
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

with SAL;
package body SMM is

   function Relative_Name
     (Root      : in String;
      Full_Name : in String)
      return String
   is
   begin
      if Root = Full_Name (Full_Name'First .. Full_Name'First + Root'Length - 1) then
         return Full_Name (Full_Name'First + Root'Length .. Full_Name'Last);
      else
         raise SAL.Programmer_Error with Full_Name & " not relative to root " & Root;
      end if;
   end Relative_Name;

end SMM;
