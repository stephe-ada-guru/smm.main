--  Abstract :
--
--  Report average, standard deviation of download period (last to
--  previous) for 'vocal' and 'instrumental' categories. Also output
--  histogram plot in gnuplot files.
--
--  References:
--
--  gnu plot manual:    /usr/share/doc/gnuplot/manual/gnuplot.pdf
--
--  Copyright (C) 2016 - 2017 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen_Histogram.Gen_Gnuplot;
procedure SMM.History (Db : in out SAL.Config_Files.Configuration_Type)
is
   use SAL.Time_Conversions;

   package Float_Elementary is new Ada.Numerics.Generic_Elementary_Functions (Float);

   package Float_Stats is new SAL.Gen_Stats (Float, Float_Elementary);

   Stats : Float_Stats.Stats_Type;

   Years_Per_Bin : constant Float := 0.5;

   function To_Bin (Item : in Float) return Integer
   is begin
      return Integer (Item / Years_Per_Bin);
   end To_Bin;

   Bin_Max : constant := 10;

   package Histograms is new SAL.Gen_Histogram
     (Bin_Min => 0,
      Bin_Max => Bin_Max,
      Value   => Float,
      To_Bin  => To_Bin);

   package Histogram_Plots is new Histograms.Gen_Gnuplot (Years_Per_Bin);

   procedure Do_Category (Category : in String)
   is
      Histogram        : Histograms.Object;
      Dont_Play        : Integer := 0;
      Never_Downloaded : Integer := 0;
      Downloaded_Once  : Integer := 0;

      procedure Accumulate (I : in Iterator_Type)
      is
         Item_Category : constant String := Read (Db, I, Category_Key, Missing_Key => Ignore);
      begin
         if Category /= Item_Category then
            return;
         elsif Category = "dont_play" then
            Dont_Play := Dont_Play + 1;
            return;
         end if;

         declare
            use Ada.Float_Text_IO;

            Last   : constant Time_Type := Read_Last_Downloaded (Db, I);
            Prev   : constant Time_Type := Read_Prev_Downloaded (Db, I);
            Period : constant Float     := Float ((Last - Prev) / Seconds_Per_Year);
         begin
            if Last = 0.0 then
               Never_Downloaded := Never_Downloaded + 1;

            elsif Prev = 0.0 then
               Downloaded_Once := Downloaded_Once + 1;

            else
               Stats.Accumulate (Period);
               Histogram.Accumulate (Period);

               if To_Bin (Period) > Bin_Max then
                  Put ("song:" & Current (I) & " period:");
                  Put (Period, Aft => 2, Exp => 0);
                  New_Line;
               end if;
            end if;
         end;
      end Accumulate;

      I : Iterator_Type := First (Db, Songs_Key);
   begin
      New_Line;
      Put_Line (Category & " :");
      loop
         exit when Is_Null (I);
         Accumulate (I);
         Next (I);
      end loop;

      declare
         use Ada.Float_Text_IO;
         Display : constant Float_Stats.Display_Type := Stats.Display;
      begin
         Put_Line ("songs            :" & Integer'Image (Display.Count));
         Put_Line ("dont_play        :" & Integer'Image (Dont_Play));
         Put_Line ("never downloaded :" & Integer'Image (Never_Downloaded));
         Put_Line ("downloaded once  :" & Integer'Image (Downloaded_Once));
         Put_Line ("last/prev downloaded stats (years):");
         Put ("mean    :"); Put (Display.Mean, Fore => 3, Aft => 2, Exp => 0); New_Line;
         Put ("std dev :"); Put (Display.Standard_Deviation, Fore => 3, Aft => 2, Exp => 0); New_Line;
         Put ("min     :"); Put (Display.Min, Fore => 3, Aft => 2, Exp => 0); New_Line;
         Put ("max     :"); Put (Display.Max, Fore => 3, Aft => 2, Exp => 0); New_Line;
      end;

      Histogram_Plots.Put_Plot
        (Histogram, Category, "years");

   end Do_Category;

begin
   Do_Category ("vocal");
   Do_Category ("instrumental");
end SMM.History;
