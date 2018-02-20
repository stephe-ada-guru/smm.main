--  Abstract :
--
--  Convert a music SAL config file to an sqlite3 database.
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

with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL.Config_Files.Integer;
with SAL.Time_Conversions;
with SMM.Database;
with SMM.ID3;
procedure Config_To_Sqlite
is
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   procedure Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("config_to_sqlite <config file name> <sql db file name>");
   end Usage;

   Config    : SAL.Config_Files.Configuration_Type;
   Config_DB : SAL.Config_Files.Configuration_Type;
   SQL_DB    : SMM.Database.Database;

   --  database keys
   Category_Key        : constant String := "Category";
   File_Key            : constant String := "File";
   Last_Downloaded_Key : constant String := "Last_Downloaded";
   Prev_Downloaded_Key : constant String := "Prev_Downloaded";
   Play_After_Key      : constant String := "Play_After";
   Play_Before_Key     : constant String := "Play_Before";
   Songs_Key           : constant String := "Songs";

   function Read_Time
     (Db  : in SAL.Config_Files.Configuration_Type;
      I   : in SAL.Config_Files.Iterator_Type;
      Key : in String)
     return SAL.Time_Conversions.Time_Type
   is
      use SAL.Config_Files;
      use SAL.Time_Conversions;
   begin
      if Is_Present (Db, I, Key)  then
         declare
            Temp : constant String := Read (Db, I, Key);
         begin
            if Temp'Length = Extended_ASIST_Time_String_Type'Last and then Temp (5) = '-' then
               --  ASIST string
               return To_TAI_Time (Temp, Absolute => True);
            else
               --  Time_type'image; old db, or unit test.
               return Time_Type'Value (Temp);
            end if;
         exception
         when others =>
            --  bad format in db file; report it, but keep going
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, "bad time format for " & Current (I) & "." & Key & ": " & Temp);
            return 0.0;
         end;
      else
         return 0.0;
      end if;
   end Read_Time;

   function Read_Last_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type
   is begin
      return Read_Time (Db, I, Last_Downloaded_Key);
   end Read_Last_Downloaded;

   function Read_Prev_Downloaded
     (Db : in SAL.Config_Files.Configuration_Type;
      I  : in SAL.Config_Files.Iterator_Type)
     return SAL.Time_Conversions.Time_Type
   is begin
      return Read_Time (Db, I, Prev_Downloaded_Key);
   end Read_Prev_Downloaded;

   function SAL_To_UTC (SAL_Time : in SAL.Time_Conversions.Time_Type) return SMM.Database.Time_String
   is begin
      return SMM.Database.UTC_Image (SAL.Time_Conversions.To_Calendar_Time (SAL_Time));
   end SAL_To_UTC;

   function UTC_To_SAL (UTC_Time : in SMM.Database.Time_String) return String
   is
      use SAL.Time_Conversions;
      SAL_Time : constant Time_Type := To_TAI_Time (Ada.Calendar.Formatting.Value (UTC_Time));
   begin
      return To_Extended_ASIST_String (SAL_Time);
   end UTC_To_SAL;

   function Get_Root
     (Server_Config : in SAL.Config_Files.Configuration_Type;
      DB_Config     : in SAL.Config_Files.Configuration_Type)
     return String
   is
      use SAL.Config_Files;
   begin
      if Is_Present (Server_Config, SMM.Root_Key) then
         return Read (Server_Config, SMM.Root_Key);
      elsif Is_Present (DB_Config, SMM.Root_Key) then
         return Read (DB_Config, SMM.Root_Key);
      else
         raise SAL.Not_Found with "no root key found";
      end if;
   end Get_Root;

begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count /= 2 then
         Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      SAL.Config_Files.Open
        (Config, Argument (1),
         Duplicate_Key         => SAL.Config_Files.Raise_Exception,
         Read_Only             => True,
         Case_Insensitive_Keys => True);

      SMM.Database.Open (SQL_DB, Argument (2));
   end;

   declare
      use SAL.Config_Files;
      use SMM;
   begin
      Open
        (Config_DB,
         Name                  => Read (Config, "DB_Filename", Missing_Key => Raise_Exception),
         Missing_File          => Raise_Exception,
         Duplicate_Key         => Raise_Exception,
         Read_Only             => True,
         Case_Insensitive_Keys => True);
   end;

   declare
      use Ada.Strings.Unbounded;
      use SMM;
      use SMM.Database;
      use SAL.Config_Files;
      use SAL.Config_Files.Integer;

      Root_Dir : constant String := As_Directory (Get_Root (Server_Config => Config, DB_Config => Config_DB));

      I          : Iterator_Type := First (Config_DB, Songs_Key);
      Warm_Fuzzy : Integer       := 0;
   begin
      loop
         exit when Is_Null (I);
         declare
            File_Name : constant String := Read (Config_DB, I, File_Key);
            File      : SMM.ID3.File;
         begin
            File.Open (Root_Dir & File_Name);

            SQL_DB.Insert
              (ID             => Integer'Value (Current (I)),
               File_Name      => File_Name,
               Category       => Read (Config_DB, I, Category_Key, Default => "vocal", Missing_Key => Ignore),
               Artist         => File.Read (SMM.ID3.Artist),
               Album          => File.Read (SMM.ID3.Album),
               Title          => File.Read (SMM.ID3.Title),
               Last_Downloaded   => SAL_To_UTC (Read_Last_Downloaded (Config_DB, I)),
               Prev_Downloaded =>
                 (if Is_Present (Config_DB, I, Prev_Downloaded_Key)
                  then SAL_To_UTC (Read_Prev_Downloaded (Config_DB, I))
                  else Jan_1_1958),
               Play_Before =>
                 (if Is_Present (Config_DB, I, Play_Before_Key)
                  then Read (Config_DB, I, Play_Before_Key)
                  else Null_ID),
               Play_After =>
                 (if Is_Present (Config_DB, I, Play_After_Key)
                  then Read (Config_DB, I, Play_After_Key)
                  else Null_ID));
         exception
         when E : SMM.Database.Entry_Error =>
            declare
               use Ada.Strings.Fixed;
            begin
               if 0 /= Index
                 (Source => Ada.Exceptions.Exception_Message (E),
                  Pattern => "UNIQUE constraint failed")
               then
                  --  resuming a previously failed import; ignore
                  null;
               else
                  raise;
               end if;
            end;
         end;
         Next (I);

         if 0 = Warm_Fuzzy mod 10_000 then
            Warm_Fuzzy := 0;
            Ada.Text_IO.New_Line;
         elsif 0 = Warm_Fuzzy mod 100 then
            Ada.Text_IO.Put (".");
         end if;
         Warm_Fuzzy := Warm_Fuzzy + 1;
      end loop;
   end;

   --  Convert back to config to check by diff.
   declare
      use SAL.Config_Files;
      use SAL.Config_Files.Integer;
      use SMM;

      New_Config_DB : Configuration_Type;
      Cursor        : SMM.Database.Cursor;
   begin
      Open
        (New_Config_DB, "smm_config.db",
         Duplicate_Key => Raise_Exception,
         Read_Only     => False);

      Cursor := SMM.Database.First (SQL_DB);
      loop
         declare
            Song     : constant SMM.Database.Song_Type := Cursor.Element;
            ID_Image : constant String                 := Integer'Image (Song.ID);
         begin
            Write_String (New_Config_DB, Songs_Key & "." & ID_Image & "." & File_Key, -Song.File_Name);
            Write_String (New_Config_DB, Songs_Key & "." & ID_Image & "." & Category_Key, -Song.Category);
            --  Artist, Album, Title only in File_Name and ID3 content in file.

            if Song.Last_Downloaded /= SMM.Database.Jan_1_1958 then
               Write_String
                 (New_Config_DB,
                  Songs_Key & "." & ID_Image & "." & Last_Downloaded_Key,
                  UTC_To_SAL (Song.Last_Downloaded));
            end if;

            if Song.Prev_Downloaded /= SMM.Database.Jan_1_1958 then
               Write_String
                 (New_Config_DB,
                  Songs_Key & "." & ID_Image & "." & Prev_Downloaded_Key,
                  UTC_To_SAL (Song.Prev_Downloaded));
            end if;

            if Song.Play_Before /= SMM.Database.Null_ID then
               Write (New_Config_DB, Songs_Key & "." & ID_Image & "." & Play_Before_Key, Song.Play_Before);
            end if;

            if Song.Play_After /= SMM.Database.Null_ID then
               Write (New_Config_DB, Songs_Key & "." & ID_Image & "." & Play_After_Key, Song.Play_After);
            end if;
         end;

         Cursor.Next;
         exit when not Cursor.Has_Element;
      end loop;
   end;
exception
when E : others =>
   declare
      use Ada.Text_IO;
      use Ada.Exceptions;
      use GNAT.Traceback.Symbolic;
   begin
      Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Symbolic_Traceback (E));
   end;
end Config_To_Sqlite;
