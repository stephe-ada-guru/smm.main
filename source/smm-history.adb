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

   Bin_Max       : constant       := 10;
   Years_Per_Bin : constant Float := 0.1;

   Total_Songs : Integer := 0;

   type Categories is (Vocal, Instrumental, Meditation, Christmas, Dont_Play, Talk);

   function To_Bin (Item : in Float) return Integer
   is begin
      return Integer (Item / Years_Per_Bin);
   end To_Bin;

   package Histograms is new SAL.Gen_Histogram
     (Bin_Min => 0,
      Bin_Max => Bin_Max,
      Value   => Float,
      To_Bin  => To_Bin);

   package Histogram_Plots is new Histograms.Gen_Gnuplot (Years_Per_Bin);

   type Category_Data_Type (Hist : Boolean := False) is record
      case Hist is
      when True =>
         Stats            : Float_Stats.Stats_Type;
         Histogram        : Histograms.Object;
         Never_Downloaded : Integer := 0;
         Downloaded_Once  : Integer := 0;
      when False =>
         Count : Integer := 0;
      end case;
   end record;

   Null_Histogram_Data : constant Category_Data_Type :=
     (Hist             => True,
      Stats            => Float_Stats.Null_Stats,
      Histogram        => Histograms.Null_Histogram,
      Never_Downloaded => 0,
      Downloaded_Once  => 0);

   Null_Count_Data : constant Category_Data_Type :=
     (Hist  => False,
      Count => 0);

   type Category_Array_Data is array (Categories) of Category_Data_Type;

   Category_Data : Category_Array_Data :=
     (Vocal        => Null_Histogram_Data,
      Instrumental => Null_Histogram_Data,
      Meditation   => Null_Count_Data,
      Christmas    => Null_Count_Data,
      Dont_Play    => Null_Count_Data,
      Talk         => Null_Count_Data);

   procedure Accumulate (I : in Iterator_Type)
   is
      String_Category : constant String := Read (Db, I, Category_Key, Missing_Key => Ignore);
      Category : Categories;
   begin
      Total_Songs := Total_Songs + 1;

      if String_Category = "" then
         Category := Vocal;
      else
         Category := Categories'Value (String_Category);
      end if;

      declare
         Data : Category_Data_Type renames Category_Data (Category);
      begin
         case Category_Data (Category).Hist is
         when True =>
            declare
               use Ada.Float_Text_IO;

               Last   : constant Time_Type := Read_Last_Downloaded (Db, I);
               Prev   : constant Time_Type := Read_Prev_Downloaded (Db, I);
               Period : constant Float     := Float ((Last - Prev) / Seconds_Per_Year);
            begin
               if Last = 0.0 then
                  Data.Never_Downloaded := Data.Never_Downloaded + 1;
                  Put_Line ("new: " & Read (Db, I, File_Key));

               elsif Prev = 0.0 then
                  Data.Downloaded_Once := Data.Downloaded_Once + 1;

               else
                  Data.Stats.Accumulate (Period);
                  Data.Histogram.Accumulate (Period);

                  if To_Bin (Period) > Bin_Max or
                    To_Bin (Period) = 0
                  then
                     Put ("song:" & Current (I) & " period:");
                     Put (Period, Aft => 2, Exp => 0);
                     New_Line;
                  end if;
               end if;
            end;
         when False =>
            Data.Count := Data.Count + 1;
         end case;
      end;
   end Accumulate;

   procedure Put_Category (Category : in Categories)
   is
      use Ada.Float_Text_IO;
      Data    : Category_Data_Type renames Category_Data (Category);
   begin
      New_Line;
      Put_Line (Categories'Image (Category) & " :");

      case Data.Hist is
      when True =>
         declare
            Display : constant Float_Stats.Display_Type := Data.Stats.Display;
         begin
            Put_Line ("songs            :" & Integer'Image (Display.Count));
            Put_Line ("never downloaded :" & Integer'Image (Data.Never_Downloaded));
            Put_Line ("downloaded once  :" & Integer'Image (Data.Downloaded_Once));
            Put_Line ("last/prev downloaded stats (years):");
            Put ("mean    :"); Put (Display.Mean, Fore => 3, Aft => 2, Exp => 0); New_Line;
            Put ("std dev :"); Put (Display.Standard_Deviation, Fore => 3, Aft => 2, Exp => 0); New_Line;
            Put ("min     :"); Put (Display.Min, Fore => 3, Aft => 2, Exp => 0); New_Line;
            Put ("max     :"); Put (Display.Max, Fore => 3, Aft => 2, Exp => 0); New_Line;

            Histogram_Plots.Put_Plot (Data.Histogram, Categories'Image (Category), "years");
         end;

      when False =>
         Put_Line ("songs            :" & Integer'Image (Data.Count));

      end case;
   end Put_Category;

   I : Iterator_Type := First (Db, Songs_Key);
begin
   loop
      exit when Is_Null (I);
      Accumulate (I);
      Next (I);
   end loop;

   New_Line;
   Put_Line ("total songs     :" & Integer'Image (Total_Songs));

   for Category in Categories'Range loop
      Put_Category (Category);
   end loop;

end SMM.History;
