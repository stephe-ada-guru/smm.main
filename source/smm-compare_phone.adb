--  Abstract :
--
--  Compare dates of local music files against those in Phone_Filename.
--
--  Design:
--
--  phone file has output of:
--  ssh -p 2222 root@192.168.7.180 "cd /storage/emulated/0/Music/Music; ls -lR" > /tmp/phone_music.log
--
--  Copyright (C) 2025 Stephen Leake.  All Rights Reserved.
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

with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with SAL.Config_Files;
with SAL.Gen_Trimmed_Image;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
procedure SMM.Compare_Phone
  (Source_Root    : in     String;
   Phone_Filename : in     String;
   Config         : in out SAL.Config_Files.Configuration_Type)
is
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Phone_File : File_Type;

   type Data is record
      Date : Ada.Calendar.Time;
      Name : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Name (Item : in Data) return String is (Ada.Strings.Unbounded.To_String (Item.Name));

   function Compare (Left, Right : in String) return SAL.Compare_Result
   is begin
      if Left = Right then
         return SAL.Equal;
      elsif Left > Right then
         return SAL.Greater;
      else
         return SAL.Less;
      end if;
   end Compare;

   package String_Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Data,
      Key_Type     => String,
      Key          => Name,
      Key_Compare  => Compare);

   Phone_Data : String_Trees.Tree;
   Local_Data : String_Trees.Tree;

   Current_Dir : Ada.Strings.Unbounded.Unbounded_String;

   Last_Compare_Time_String : constant SMM.Time_String   := SAL.Config_Files.Read (Config, "compare_phone");
   Last_Compare             : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Value (Last_Compare_Time_String);
begin
   Open (Phone_File, In_File, Phone_Filename);

   Read_Phone_File :
   loop
      exit Read_Phone_File when End_Of_File (Phone_File);
      declare
         Line : constant String := Get_Line (Phone_File);

         function Error_Line return String
         is
            function Trimmed_Image is new SAL.Gen_Trimmed_Image (Positive_Count);
         begin
            return Phone_Filename & ":" & Trimmed_Image (Ada.Text_IO.Line (Phone_File) - 1) & ":";
         end Error_Line;

      begin
         if Line'Length = 0 then
            null;

         elsif Line (Line'Last) = ':' then
            --  New directory. must be relative to Music
            if Line'Length = 2 then
               Current_Dir := +".";
            elsif Line (Line'First + 0 .. Line'First + 1) = "./" then
               Current_Dir := +Line (Line'First + 2 .. Line'Last - 1);
            end if;

         elsif Line (Line'First) = 't' then
            --  total
            null;

         elsif Line (Line'First) = 'd' then
            --  directory
            null;

         elsif Line (Line'First) = '-' then
            --  A file. We compare all files; all are important, any could change.
            --  Except ignore files in root; they are only on the phone
            if -Current_Dir /= "." then
               declare
                  --  phone: Line from 'ls -lR' looks like:
                  --  drwxrwx--- 4 u0_a288 u0_a288  3452 2023-06-28 14:44 Aaron Copland

                  --  portable mounted in Debian: Line from
                  --  'ls -lR --time-style "+%F %H:%M:%S" --quoting-style=literal'
                  --  looks like:
                  --  drwxrwxrwx 1 stephe stephe 8192 2024-04-28 09:26:23 Appalachian Spring

                  --  but the column widths are not fixed.
                  --  Calendar.Formatting.Value requires a seconds field
                  function Find_Date_First return Integer
                  is
                     use Ada.Strings.Fixed;
                     First_Space : constant Integer := Index (Line, Pattern => " ");
                  begin
                     return Index (Line, Pattern => "-", From => First_Space + 1) - 4;
                  end Find_Date_First;

                  function Find_Date_Last (Date_First : in Integer) return Integer
                  is
                     use Ada.Strings.Fixed;
                     First_Space : constant Integer := Index (Line, Pattern => " ", From => Date_First);
                  begin
                     return Index (Line, Pattern => " ", From => First_Space + 1) - 1;
                  end Find_Date_Last;

                  Date_First  : constant Integer := Find_Date_First;
                  Date_Last   : constant Integer := Find_Date_Last (Date_First);
                  Date_String : constant String  := Line (Date_First .. Date_Last) &
                    (if Date_Last - Date_First > 15 then "" else ":00");
                  Filename    : constant String  := -Current_Dir & "/" & Line (Date_Last + 2 .. Line'Last);
               begin
                  Phone_Data.Insert
                    (Data'
                       (Date => Ada.Calendar.Formatting.Value (Date_String),
                        Name => +Filename));
               exception
               when Constraint_Error =>
                  --  From Value
                  raise SAL.Programmer_Error with "bad date format: '" & Date_String & "'";

               when SAL.Duplicate_Key =>
                  --  From Insert
                  raise SAL.Programmer_Error with Error_Line & "duplicate filename? '" & Filename & "'";
               end;
            end if;
         else
            raise SAL.Programmer_Error with Error_Line & " '" & Line & "' ";
         end if;
      end;
   end loop Read_Phone_File;

   Read_Local :
   declare
      procedure Process_Dir (Dir : in String)
      is
         use Ada.Directories;

         procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
         is
            File_Name : constant String := Relative_Name (Source_Root, Normalize (Full_Name (Dir_Entry)));
         begin
            case Kind (Dir_Entry) is
            when Directory =>
               if Simple_Name (Dir_Entry) = "." or
                 Simple_Name (Dir_Entry) = ".."
               then
                  return;
               end if;

               Process_Dir (File_Name);

            when Ordinary_File =>
               Local_Data.Insert
                 (Data'
                    (Date => Modification_Time (Dir_Entry),
                     Name => +File_Name));

            when Special_File =>
               raise SAL.Programmer_Error with "found special file";
            end case;
         end Process_Dir_Entry;
      begin
         Search
           (Dir,
            Pattern          => "*", -- matches directories and everything else
            Filter           =>
              (Ordinary_File => True,
               Directory     => True,
               Special_File  => False),
            Process          => Process_Dir_Entry'Access);
      end Process_Dir;

   begin
      Process_Dir (Source_Root);
   end Read_Local;

   Compare_Trees :
   declare
      use String_Trees;

      Phone_Iterator : constant Iterator := Iterate (Phone_Data);
      Phone_Cur      : Cursor   := First (Phone_Iterator);

      Local_Iterator : constant Iterator := Iterate (Local_Data);
      Local_Cur      : Cursor            := First (Local_Iterator);
      Error_Count    : Integer           := 0;

   begin
      --  We assume changes can occur on either side, which means we compare
      --  file dates to last time this was run.
      loop
         exit when not Has_Element (Phone_Cur) or not Has_Element (Local_Cur);
         exit when Max_Errors > 0 and then Error_Count >= Max_Errors;
         declare
            use type Ada.Strings.Unbounded.Unbounded_String; --  "/="
            use type Ada.Calendar.Time; -- "<"

            Phone : Data renames Element (Phone_Cur);
            Local : Data renames Element (Local_Cur);
         begin
            if Verbosity > 0 then
               Put_Line ("comparing '" & (-Phone.Name) & "'");
               Put_Line ("          '" & (-Local.Name) & "'");
            end if;

            if Phone.Name /= Local.Name then
               if Verbosity = 0 then
                  declare
                     --  We can't show the previous phone and local,
                     --  because cursor is ascending.
                     Next_Phone : constant Cursor := Next (Phone_Iterator, Phone_Cur);
                     Next_Local : constant Cursor := Next (Local_Iterator, Local_Cur);
                  begin
                     Put_Line ("current phone:'" & (-Phone.Name) & "'");
                     Put_Line ("next phone   :'" & (-Element (Next_Phone).Name) & "'");
                     Put_Line ("next local   :'" & (-Element (Next_Local).Name) & "'");
                  end;
               end if;

               Put_Line ("new local file:  '" & (-Local.Name) & "'");
               New_Line;
               Error_Count := @ + 1;
               Local_Cur := Next (Local_Iterator, Local_Cur);

            elsif Phone.Date > Last_Compare then
               Put_Line ("file updated on phone '" & (-Phone.Name) & "'");
               New_Line;
               Phone_Cur := Next (Phone_Iterator, Phone_Cur);
               Local_Cur := Next (Local_Iterator, Local_Cur);
               Error_Count := @ + 1;

            elsif Local.Date > Last_Compare then
               Put_Line ("file updated on laptop '" & (-Local.Name) & "'");
               New_Line;
               Phone_Cur := Next (Phone_Iterator, Phone_Cur);
               Local_Cur := Next (Local_Iterator, Local_Cur);
               Error_Count := @ + 1;

            else
               Phone_Cur := Next (Phone_Iterator, Phone_Cur);
               Local_Cur := Next (Local_Iterator, Local_Cur);
            end if;
         end;
      end loop;

      if Max_Errors > 0 and then Error_Count >= Max_Errors then
         Put_Line ("all done - max errors");
      else
         if Has_Element (Phone_Cur) then
            Put_Line ("phone has extra files:");
            loop
               exit when not Has_Element (Phone_Cur);
               Put_Line ((-Element (Phone_Cur).Name) & "'");
               Phone_Cur := Next (Phone_Iterator, Phone_Cur);
            end loop;
         end if;

         if Has_Element (Local_Cur) then
            loop
               exit when not Has_Element (Local_Cur);
               Put_Line ("new file '" & (-Element (Local_Cur).Name) & "'");
               Local_Cur := Next (Local_Iterator, Local_Cur);
            end loop;
         end if;
         Put_Line ("all done");
         SAL.Config_Files.Write (Config, "compare_phone", Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));

      end if;
   end Compare_Trees;
end SMM.Compare_Phone;
