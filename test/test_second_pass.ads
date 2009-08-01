--  Abstract :
--
--  Test playlists --second
--
--  Copyright (C) 2007, 2008, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Strings.Unbounded;
with AUnit.Test_Cases; use AUnit.Test_Cases;
package Test_Second_Pass is

   type Test_Case
     (Verbosity : Integer;
      Debug     : Integer)
      is new Standard.AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access;

   overriding procedure Set_Up_Case (T : in out Test_Case);

end Test_Second_Pass;
