--  Abstract :
--
--  see spec
--
--  Copyright (C) 2004 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with AUnit.Assertions;
package body Test_Books.Database is

   procedure Check
     (Label    : in String;
      ID       : in Books.Database.ID_Type;
      Expected : in Books.Database.ID_Type)
   is
      use type Books.Database.ID_Type;
   begin
      AUnit.Assertions.Assert (ID = Expected, Label & "; got " & Books.Database.ID_Type'Image (ID));
   end Check;

end Test_Books.Database;
