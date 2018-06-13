--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2016, 2017, 2018 Stephen Leake.  All Rights Reserved.
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

with AUnit.Checks;
with Ada.Streams.Stream_IO;
with SAL.Interfaces_More.AUnit;
with SMM.ID3;
package body SMM.ID3.Test is

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Ada.Streams.Stream_IO.Count);

   procedure Check
     (Label    : in String;
      Computed : in Size_Type;
      Expected : in Size_Type)
   is
      use SAL.Interfaces_More.AUnit;
   begin
      Check (Label & ".byte_3", Computed.Byte_3, Expected.Byte_3);
      Check (Label & ".byte_2", Computed.Byte_2, Expected.Byte_2);
      Check (Label & ".byte_1", Computed.Byte_1, Expected.Byte_1);
      Check (Label & ".byte_0", Computed.Byte_0, Expected.Byte_0);
   end Check;

   ----------
   --  Test procedures

   procedure Test_Size (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use all type Ada.Streams.Stream_IO.Count;

      procedure Check_One
        (Label          : in String;
         Item           : in Size_Type;
         Expected_Count : in Ada.Streams.Stream_IO.Count)
      is
         use Ada.Streams.Stream_IO;
         Computed_Count : constant Count := Size (Item);
         Computed_Size  : constant Size_Type  := To_Size (Computed_Count);
      begin
         Check (Label & ".count", Computed_Count, Expected_Count);
         Check (Label & ".size", Computed_Size, Item);
      end Check_One;

   begin
      Check_One ("1", (0, 0, 0, 1), 1);
      Check_One ("2", (0, 0, 1, 0), 128);
      Check_One ("3", (0, 1, 0, 0), 128 * 128);
      Check_One ("4", (1, 0, 0, 0), 128 * 128 * 128);

      --  (+ 1 (* 1 128) (* 1 128 128) (* 1 128 128 128))
      --  (+ #x200000 #x4000 #x80 1)
      Check_One ("5", (1, 1, 1, 1), 2113665);

      --  (+ 4 (* 3 128) (* 2 128 128) (* 1 128 128 128))
      Check_One ("6", (1, 2, 3, 4), 2130308);
   end Test_Size;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("smm-id3-test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Size'Access, "Test_Size");
   end Register_Tests;

end SMM.ID3.Test;
