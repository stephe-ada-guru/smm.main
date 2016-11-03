--  Abstract :
--
--  Report average, standard deviation of download period (last to previous)
--
--  Copyright (C) 2016 Stephen Leake.  All Rights Reserved.
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

with Ada.Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;      use Ada.Text_IO;
with SAL.Config_Files; use SAL.Config_Files;
with SAL.Gen_Stats;
procedure SMM.History (Db : in out SAL.Config_Files.Configuration_Type)
is
   use SAL.Time_Conversions;

   package Float_Elementary is new Ada.Numerics.Generic_Elementary_Functions (Float);

   package Float_Stats is new SAL.Gen_Stats (Float, Float_Elementary);

   Stats : Float_Stats.Stats_Type;

   procedure Accumulate (I : in Iterator_Type)
   is
      Last : constant Time_Type := Read_Last_Downloaded (Db, I);
      Prev : constant Time_Type := Read_Prev_Downloaded (Db, I);
   begin
      if Last /= 0.0 and Prev /= 0.0 then
         Stats.Accumulate (Float ((Last - Prev) / Seconds_Per_Year));
      end if;
   end Accumulate;

   I : Iterator_Type := First (Db, Songs_Key);
begin

   loop
      exit when Is_Null (I);
      Accumulate (I);
      Next (I);
   end loop;

   declare
      use Ada.Float_Text_IO;
      Display : constant Float_Stats.Display_Type := Stats.Display;
   begin
      Put_Line ("last/prev downloaded stats (years):");
      Put_Line ("songs   :" & Integer'Image (Display.Count));
      Put ("mean    :"); Put (Display.Mean, Fore => 3, Aft => 2, Exp => 0); New_Line;
      Put ("std dev :"); Put (Display.Standard_Deviation, Fore => 3, Aft => 2, Exp => 0); New_Line;
      Put ("min     :"); Put (Display.Min, Fore => 3, Aft => 2, Exp => 0); New_Line;
      Put ("max     :"); Put (Display.Max, Fore => 3, Aft => 2, Exp => 0); New_Line;
   end;

end SMM.History;
