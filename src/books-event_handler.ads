--  Abstract :
--
--  Gdk event handler for Books, providing catch-all exception handling.
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
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

with Ada.Exceptions;
with Gdk.Event;
with System;
package Books.Event_Handler is

   Unhandled_Exception : Boolean := False;
   Unhandled_Occurrence : Ada.Exceptions.Exception_Occurrence;

   procedure Event_Handler (Event : in Gdk.Event.Gdk_Event; Data : in System.Address);
   --  Provides catch-all exception handling. If exception is not
   --  recognized, sets Unhandled_Exception to True, and stores
   --  exception occurance in Unhandled_Occurance, for unit testing.
   pragma Convention (C, Event_Handler);

end Books.Event_Handler;
