--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with AUnit.Checks;
with SAL.Memory_Streams;
package body SMM.ID3.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in String;
      Expected : in Tag_Lists.List)
   is
      use Ada.Strings.Unbounded;
      use SAL.Memory_Streams;
      use Standard.AUnit.Checks;

      Stream : aliased Stream_Type (Computed'Length);
   begin
      Stream.Create (Computed'Address);

      for Tag of Expected loop
         Stream.Reset;
         Check (Label & Tag.ID, Read (Stream'Access, Tag.ID), To_String (Tag.Data));
      end loop;
   end Check;

end SMM.ID3.AUnit;
