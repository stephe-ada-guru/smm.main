--  Abstract :
--
--  Utilities for unit tests
--
--  Copyright (C) 2007, 2009 Stephen Leake.  All Rights Reserved.
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

package Test_Utils is
   pragma Elaborate_Body; -- Ada.Text_IO

   procedure Cleanup;
   --  Delete tmp/

   procedure Create_Test_File (Path : in String);
   --  File contains one line.

   procedure Check_Exists (Path : in String; Expected_Exists : in Boolean);
   --  Assertion

end Test_Utils;
