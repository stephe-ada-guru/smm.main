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

with Ada.Strings.Fixed;
with SAL;
package body SMM is

   function Relative_Name_Sans_Extension
     (Root      : in String;
      Full_Name : in String)
      return String
   is
   begin
      if Root = Full_Name (Full_Name'First .. Full_Name'First + Root'Length - 1) then
         declare
            Temp            : constant String  := Full_Name (Full_Name'First + Root'Length .. Full_Name'Last);
            Extension_First : constant Integer := Ada.Strings.Fixed.Index
              (Source  => Temp,
               Pattern => ".",
               Going   => Ada.Strings.Backward);
         begin
            if Extension_First = 0 then
               return Temp;
            else
               return Temp (Temp'First .. Extension_First - 1);
            end if;
         end;
      else
         raise SAL.Programmer_Error with Full_Name & " not relative to root " & Root;
      end if;
   end Relative_Name_Sans_Extension;

end SMM;
