--  Abstract :
--
--  Test smm_server, via http.
--
--  Copyright (C) 2016, 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases;
package Test_Server is

   type Test_Case
     (Debug     : Integer;
      Verbosity : Integer)
     is new AUnit.Test_Cases.Test_Case with null record;
   --  If the development machine has VMWare installed, it creates IP
   --  addresses that are not accessible from outside the machine, so
   --  they are not suitable for books-central_server. There seems to
   --  be no way to filter those out from Ada in the server. So we
   --  have to specify the correct IP address to use when spawning and
   --  connecting to the server.
   --
   --  Debug
   --  0 : normal
   --  1 : don't spawn server, don't recreate db
   --  2 : spawn server with log

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return AUnit.Message_String;

   overriding procedure Set_Up_Case (T : in out Test_Case);

   overriding procedure Tear_Down_Case (T : in out Test_Case);

end Test_Server;
