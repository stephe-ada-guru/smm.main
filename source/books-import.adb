--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2009, 2012, 2016 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
package body Books.Import is

   procedure Warm_Fuzzy
   is begin
      Warm_Fuzzy_Count := Warm_Fuzzy_Count + 1;
      if Warm_Fuzzy_Count mod 10 = 0 then
         Ada.Text_IO.Put ('.');
      end if;

      if Warm_Fuzzy_Count mod 1000 = 0 then
         Ada.Text_IO.New_Line;
      end if;
   end Warm_Fuzzy;

   procedure Read
     (File   : in     SAL.CSV.File_Type;
      Column : in     Integer;
      Value  :    out Integer;
      Valid  :    out Boolean)
   is
      Data : constant String := SAL.CSV.Read (File, Column);
   begin
      Value := Integer'Value (Data);
      Valid := True;
   exception
   when Constraint_Error =>
      Value := 0;
      Valid := False;
   end Read;

   function Read
     (File   : in SAL.CSV.File_Type;
      Column : in Integer)
     return Integer
   is
      Data : constant String := SAL.CSV.Read (File, Column);
   begin
      return Integer'Value (Data);
   exception
   when Constraint_Error =>
      raise SAL.Initialization_Error with
        SAL.CSV.Name (File) & ":" & Ada.Text_IO.Positive_Count'Image (SAL.CSV.Line (File)) & ": '" &
        Data & "' is not a valid integer";
   end Read;

end Books.Import;
