--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020, 2025 Stephen Leake All Rights Reserved.
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
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNATCOLL.SQL.Sqlite;
package body SMM.Database is

   procedure Checked_Execute
     (DB        : in Database'Class;
      Statement : in String;
      Params    : in GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
   is begin
      GNATCOLL.SQL.Exec.Execute (DB.Connection, Statement, Params);

      if DB.Connection.Success then
         GNATCOLL.SQL.Exec.Commit (DB.Connection);
      else
         declare
            Msg : constant String := DB.Connection.Error;
         begin
            GNATCOLL.SQL.Exec.Rollback (DB.Connection);

            raise Entry_Error with Msg;
         end;
      end if;
   end Checked_Execute;

   function Checked_Fetch
     (DB        : in Database'Class;
      Statement : in String;
      Params    : in GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters)
     return Cursor
   is begin
      return Result : Cursor do
         GNATCOLL.SQL.Exec.Fetch (Result.Cursor, DB.Connection, Statement, Params);

         if not DB.Connection.Success then
            raise Entry_Error with "'" & Statement & "' " & DB.Connection.Error;
         end if;
      end return;
   end Checked_Fetch;

   procedure Insert_Update
     (DB              : in Database;
      Update          : in Boolean;
      ID              : in Integer;
      Modified        : in Time_String;
      File_Name       : in String      := "";
      Category        : in String      := "";
      Artist          : in String      := "";
      Album_Artist    : in String      := "";
      Composer        : in String      := "";
      Album           : in String      := "";
      Year            : in Integer     := No_Year;
      Title           : in String      := "";
      Track           : in Integer     := No_Track;
      Last_Downloaded : in Time_String := Default_Time_String;
      Prev_Downloaded : in Time_String := Default_Time_String;
      Play_Before     : in Integer     := Null_ID;
      Play_After      : in Integer     := Null_ID)
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.SQL.Exec;

      Statement : Unbounded_String :=
        +(if Update
          then "UPDATE Song SET "
          else "INSERT INTO Song (");

      Values : Unbounded_String := +"VALUES (";

      Params : SQL_Parameters (1 .. 16) := (others => Null_Parameter);

      Need_Comma : Boolean := False;
      Last       : Integer := 0;

      procedure Add_Param (Name : in String; Value : in String; Default : in String)
      is begin
         if Value /= Default then
            if Need_Comma then
               Statement := Statement & ", ";
               Values    := Values & ",";
            end if;
            Need_Comma := True;
            if Update then
               Statement := Statement & Name & " = ?";
            else
               Statement := Statement & Name;
               Values    := Values & "?";
            end if;
            Last := Last + 1;
            Params (Last) := +Value;
         end if;
      end Add_Param;

      procedure Add_Param (Name : in String; Value : in Integer; Default : in Integer)
      is begin
         if Value /= Default then
            if Need_Comma then
               Statement := Statement & ", ";
               Values    := Values & ",";
            end if;
            Need_Comma := True;
            if Update then
               Statement := Statement & Name & " = ?";
            else
               Statement := Statement & Name;
               Values    := Values & "?";
            end if;
            Last := Last + 1;
            Params (Last) := +Value;
         end if;
      end Add_Param;

   begin
      Add_Param ("Modified", Modified, Default_Time_String);
      Add_Param ("File_Name", File_Name, "");
      Add_Param ("Category", Category, "");
      Add_Param ("Artist", Artist, "");
      Add_Param ("Album_Artist", Album_Artist, "");
      Add_Param ("Album", Album, "");
      Add_Param ("Composer", Composer, "");
      Add_Param ("Title", Title, "");
      Add_Param ("Year", Year, No_Year);
      Add_Param ("Track", Track, No_Track);
      Add_Param ("Last_Downloaded", Last_Downloaded, Default_Time_String);
      Add_Param ("Prev_Downloaded", Prev_Downloaded, Default_Time_String);
      Add_Param ("Play_Before", Play_Before, Null_ID);
      Add_Param ("Play_After", Play_After, Null_ID);

      if Update then
         Statement := Statement & " WHERE ID = ?";
      else
         Add_Param ("Deleted", Default_Time_String, "");
         if Need_Comma then
            Statement := Statement & ", ";
            Need_Comma := True;
         end if;
         Statement := Statement & "ID";
         Values    := Values & ",?";
      end if;
      Last          := Last + 1;
      Params (Last) := +ID;

      if not Update then
         Statement := Statement & ") " & Values & ")";
      end if;

      Checked_Execute (DB, -Statement, Params (1 .. Last));
   end Insert_Update;

   ----------
   --  Public subprograms, declaration order

   overriding procedure Finalize (DB : in out Database)
   is
      use type GNATCOLL.SQL.Exec.Database_Connection;
   begin
      if DB.Connection = null then
         null;
      else
         GNATCOLL.SQL.Exec.Free (DB.Connection);
      end if;
   exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Database disconnect: exception " & Ada.Exceptions.Exception_Message (E));
   end Finalize;

   procedure Open (DB : in out Database; File_Name : in String; Expected_Schema : in Integer := Schema_Version)
   is
      use GNATCOLL.SQL.Exec;
   begin
      if not Ada.Directories.Exists (File_Name) then
         raise Ada.IO_Exceptions.Name_Error with File_Name & " does not exist";
      end if;

      DB.Connection := GNATCOLL.SQL.Exec.Build_Connection (GNATCOLL.SQL.Sqlite.Setup (File_Name));

      if not DB.Connection.Success then
         raise Ada.IO_Exceptions.Use_Error with File_Name & DB.Connection.Error;
      end if;

      declare
         Temp : constant Integer := Read_Schema_Version (DB);
      begin
         if Temp /= Expected_Schema then
            raise Schema_Version_Error with "expecting Schema_Version" & Schema_Version'Image & ", found" & Temp'Image;
         end if;
      end;
   exception
   when Ada.IO_Exceptions.Name_Error =>
      raise Ada.IO_Exceptions.Use_Error with "invalid database file name: '" & File_Name & "'";
   end Open;

   procedure Insert
     (DB              : in Database;
      ID              : in Integer;
      File_Name       : in String;
      Category        : in String;
      Artist          : in String;
      Album_Artist    : in String;
      Composer        : in String;
      Album           : in String;
      Year            : in Integer;
      Title           : in String;
      Track           : in Integer;
      Last_Downloaded : in Time_String := Default_Time_String;
      Prev_Downloaded : in Time_String := Default_Time_String;
      Play_Before     : in Integer     := Null_ID;
      Play_After      : in Integer     := Null_ID)
   is begin
      Insert_Update
        (DB,
         Update          => False,
         ID              => ID,
         Modified        => UTC_Image (Ada.Calendar.Clock),
         File_Name       => File_Name,
         Category        => Category,
         Artist          => Artist,
         Album_Artist    => Album_Artist,
         Composer        => Composer,
         Album           => Album,
         Title           => Title,
         Year            => Year,
         Track           => Track,
         Last_Downloaded => Last_Downloaded,
         Prev_Downloaded => Prev_Downloaded,
         Play_Before     => Play_Before,
         Play_After      => Play_After);
   end Insert;

   procedure Insert_JSON (DB : in Database; Value : in GNATCOLL.JSON.JSON_Value)
   is
      ID : constant Song_ID := Value.Get ("ID");
   begin
      if Value.Has_Field ("Deleted") then
         --  On the remote, a record was inserted by mistake, then deleted.
         Insert_Update
           (DB,
            Update   => False,
            ID       => ID,
            Modified => Default_Time_String);

         DB.Mark_Deleted (DB.Find_ID (ID), Value.Get ("Deleted"));
      else
         Insert_Update
           (DB,
            ID           => ID,
            Update       => False,
            Modified     => Value.Get ("Modified"),
            File_Name    => Value.Get ("File_Name"),
            Category     => Value.Get ("Category"),
            Artist       => (if Value.Has_Field ("Artist") then Value.Get ("Artist") else ""),
            Album_Artist => Value.Get ("Album_Artist"),
            Composer     => (if Value.Has_Field ("Composer") then Value.Get ("Composer") else ""),
            Album        => (if Value.Has_Field ("Album") then Value.Get ("Album") else ""),
            Year         => (if Value.Has_Field ("Year") then Value.Get ("Year") else No_Year),
            Title        => Value.Get ("Title"),
            Track        => (if Value.Has_Field ("Track") then Value.Get ("Track") else No_Track),
            Last_Downloaded =>
              (if Value.Has_Field ("Last_Downloaded") then Value.Get ("Last_Downloaded") else Default_Time_String),
            Prev_Downloaded =>
              (if Value.Has_Field ("Prev_Downloaded") then Value.Get ("Prev_Downloaded") else Default_Time_String),
            Play_Before => (if Value.Has_Field ("Play_Before") then Value.Get ("Play_Before") else Null_ID),
            Play_After => (if Value.Has_Field ("Play_After") then Value.Get ("Play_After") else Null_ID));
      end if;
   end Insert_JSON;

   function Get_JSON (DB : in Database; ID : in Song_ID) return GNATCOLL.JSON.JSON_Value
   is begin
      return Get_JSON (DB.Find_ID (ID));
   end Get_JSON;

   function Index_Fields_Equal
     (DB        : in out Database;
      ID        : in     Song_ID;
      New_Value : in     GNATCOLL.JSON.JSON_Value)
     return Boolean
   is
      Cur : constant Cursor := DB.Find_ID (ID);
   begin
      --  Fields involved in unique indices are ID, File_Name, Album_Artist, Album, Title
      if ID = New_Value.Get ("ID") then
         return True;
      end if;

      if Cur.File_Name = New_Value.Get ("File_Name") then
         return True;
      end if;

      if Cur.Album_Artist = New_Value.Get ("Album_Artist") then
         if Cur.Album = (if New_Value.Has_Field ("Album") then New_Value.Get ("Album") else "") then
            if Cur.Title = New_Value.Get ("Title") then
               return True;
            else
               return False;
            end if;
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Index_Fields_Equal;

   function UTC_Image (Item : in Ada.Calendar.Time) return Time_String
   is
      --  GNAT GPL 2016 Clock returns UTC
   begin
      return Ada.Calendar.Formatting.Image (Item);
   end UTC_Image;

   ----------
   --  Iterators

   --  Field indices; create_schema.sql declaration order
   use all type GNATCOLL.SQL.Exec.Field_Index;
   ID_Field              : constant GNATCOLL.SQL.Exec.Field_Index := GNATCOLL.SQL.Exec.Field_Index'First;
   Modified_Field        : constant GNATCOLL.SQL.Exec.Field_Index := ID_Field + 1;
   Deleted_Field         : constant GNATCOLL.SQL.Exec.Field_Index := Modified_Field + 1;
   File_Name_Field       : constant GNATCOLL.SQL.Exec.Field_Index := Deleted_Field + 1;
   Category_Field        : constant GNATCOLL.SQL.Exec.Field_Index := File_Name_Field + 1;
   Artist_Field          : constant GNATCOLL.SQL.Exec.Field_Index := Category_Field + 1;
   Album_Artist_Field    : constant GNATCOLL.SQL.Exec.Field_Index := Artist_Field + 1;
   Composer_Field        : constant GNATCOLL.SQL.Exec.Field_Index := Album_Artist_Field + 1;
   Album_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Composer_Field + 1;
   Year_Field            : constant GNATCOLL.SQL.Exec.Field_Index := Album_Field + 1;
   Title_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Year_Field + 1;
   Track_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Title_Field + 1;
   Last_Downloaded_Field : constant GNATCOLL.SQL.Exec.Field_Index := Track_Field + 1;
   Prev_Downloaded_Field : constant GNATCOLL.SQL.Exec.Field_Index := Last_Downloaded_Field + 1;
   Play_Before_Field     : constant GNATCOLL.SQL.Exec.Field_Index := Prev_Downloaded_Field + 1;
   Play_After_Field      : constant GNATCOLL.SQL.Exec.Field_Index := Play_Before_Field + 1;

   Field_Fields : constant array (Fields) of GNATCOLL.SQL.Exec.Field_Index :=
     (Artist       => Artist_Field,
      Album        => Album_Field,
      Album_Artist => Album_Artist_Field,
      Composer     => Composer_Field,
      Category     => Category_Field,
      Title        => Title_Field,
      Year         => Year_Field,
      Track        => Track_Field,
      Play_Before  => Play_Before_Field,
      Play_After   => Play_After_Field);

   function Count (Position : in Cursor) return Natural
   is begin
      return Position.Cursor.Rows_Count;
   end Count;

   function Has_Element (Position : Cursor) return Boolean
   is begin
      return Position.Cursor.Has_Row;
   end Has_Element;

   function Get_JSON (Position : in Cursor) return GNATCOLL.JSON.JSON_Value
   is
      Cur : Cursor renames Position;
   begin
      return Result : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object do
         Result.Set_Field ("ID", Cur.ID);
         if Cur.Modified /= Default_Time_String then Result.Set_Field ("Modified", Cur.Modified); end if;
         if Cur.Deleted /= Default_Time_String then Result.Set_Field ("Deleted", Cur.Deleted); end if;
         Result.Set_Field ("File_Name", Cur.File_Name);
         Result.Set_Field ("Category", Cur.Category);
         if Cur.Artist /= "" then Result.Set_Field ("Artist", Cur.Artist); end if;
         Result.Set_Field ("Album_Artist", Cur.Album_Artist);
         if Cur.Composer /= "" then Result.Set_Field ("Composer", Cur.Composer); end if;
         if Cur.Album /= "" then Result.Set_Field ("Album", Cur.Album); end if;
         if Cur.Year /= No_Year then Result.Set_Field ("Year", Cur.Year); end if;
         Result.Set_Field ("Title", Cur.Title);
         if Cur.Track /= No_Track then Result.Set_Field ("Track", Cur.Track); end if;
         if Cur.Last_Downloaded /= Default_Time_String then
            Result.Set_Field ("Last_Downloaded", Cur.Last_Downloaded);
         end if;
         if Cur.Prev_Downloaded /= Default_Time_String then
            Result.Set_Field ("Prev_Downloaded", Cur.Prev_Downloaded);
         end if;
         if Cur.Play_Before /= Null_ID then Result.Set_Field ("Play_Before", Cur.Play_Before); end if;
         if Cur.Play_After /= Null_ID then Result.Set_Field ("Play_After", Cur.Play_After); end if;
      end return;
   end Get_JSON;

   function First_By_ID (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT * FROM Song ORDER BY ID ASC";
   begin
      return Checked_Fetch (DB, Statement);
   end First_By_ID;

   function Last_By_ID (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT * FROM Song ORDER BY ID DESC";
   begin
      return Checked_Fetch (DB, Statement);
   end Last_By_ID;

   function First_By_Last_Downloaded (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT * FROM Song ORDER BY Last_Downloaded ASC";
   begin
      return Checked_Fetch (DB, Statement);
   end First_By_Last_Downloaded;

   function Last_By_Last_Downloaded (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT * FROM Song ORDER BY Last_Downloaded DESC";
   begin
      return Checked_Fetch (DB, Statement);
   end Last_By_Last_Downloaded;

   function First_By_Name (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT * FROM Song ORDER BY Album_Artist, Album, Title ASC";
      --  Order matches Song_Name index; create_schema.sql.
   begin
      return Checked_Fetch (DB, Statement);
   end First_By_Name;

   function Find_File_Name (DB : in Database'Class; File_Name : in String) return Cursor
   is
      use GNATCOLL.SQL.Exec;
      Statement : constant String := "SELECT * FROM Song WHERE File_Name = ?";
   begin
      return Checked_Fetch (DB, Statement, Params => (1 => +File_Name));
   end Find_File_Name;

   function Find_ID (DB : in Database'Class; ID : in Integer) return Cursor
   is
      use GNATCOLL.SQL.Exec;
      Statement : constant String := "SELECT * FROM Song WHERE ID = ?";
   begin
      return Checked_Fetch (DB, Statement, Params => (1 => +ID));
   end Find_ID;

   function Last_ID (DB : in Database) return Song_ID
   is
      Cur : constant Cursor := Checked_Fetch (DB, "SELECT MAX (ID) FROM Song");
   begin
      return Song_ID'Value (Cur.Cursor.Value (ID_Field));
   end Last_ID;

   function Get_Modified
     (DB       : in out Database;
      ID       : in     Song_ID;
      Modified : in     Time_String)
     return ID_Lists.List
   is
      use GNATCOLL.SQL.Exec;
      Cur : Cursor := Checked_Fetch (DB, "SELECT ID FROM Song WHERE ID <= ?" &
           " AND Modified > ? or Deleted > ? ORDER BY ID", (+ID, +Modified, +Modified));
   begin
      return Result : ID_Lists.List do
         loop
            exit when not Has_Element (Cur);
            Result.Append (Cur.ID);
            Next (Cur);
         end loop;
      end return;
   end Get_Modified;

   function Get_New
     (DB        : in out Database;
      ID        : in     Song_ID;
      Max_Count : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last)
     return ID_Lists.List
   is
      use GNATCOLL.SQL.Exec;
      use type Ada.Containers.Count_Type;
      Cur : Cursor := Checked_Fetch (DB, "SELECT ID FROM Song WHERE ID > ?" &
           " AND Deleted is null ORDER BY ID", (1 => +ID));
   begin
      return Result : ID_Lists.List do
         loop
            exit when not Has_Element (Cur);
            exit when Result.Length >= Max_Count;
            Result.Append (Cur.ID);
            Next (Cur);
         end loop;
      end return;
   end Get_New;

   procedure Update
     (DB              : in Database;
      Position        : in Cursor'Class;
      File_Name       : in String      := "";
      Category        : in String      := "";
      Artist          : in String      := "";
      Album           : in String      := "";
      Album_Artist    : in String      := "";
      Composer        : in String      := "";
      Title           : in String      := "";
      Year            : in Integer     := No_Year;
      Track           : in Integer     := No_Track;
      Last_Downloaded : in Time_String := Default_Time_String;
      Prev_Downloaded : in Time_String := Default_Time_String;
      Play_Before     : in Integer     := Null_ID;
      Play_After      : in Integer     := Null_ID)
   is begin
      Insert_Update
        (DB,
         Update          => True,
         ID              => Position.ID,
         Modified        => UTC_Image (Ada.Calendar.Clock),
         File_Name       => File_Name,
         Category        => Category,
         Artist          => Artist,
         Album           => Album,
         Album_Artist    => Album_Artist,
         Composer        => Composer,
         Title           => Title,
         Year            => Year,
         Track           => Track,
         Last_Downloaded => Last_Downloaded,
         Prev_Downloaded => Prev_Downloaded,
         Play_Before     => Play_Before,
         Play_After      => Play_After);
   end Update;

   procedure Update_JSON (DB : in Database; Value : in GNATCOLL.JSON.JSON_Value)
   is
      ID : constant Song_ID := Value.Get ("ID");
   begin
      if Value.Has_Field ("Deleted") then
         DB.Mark_Deleted (DB.Find_ID (ID), Value.Get ("Deleted"));
      else
         Insert_Update
           (DB,
            Update       => False,
            ID           => ID,
            Modified     => Value.Get ("Modified"),
            File_Name    => Value.Get ("File_Name"),
            Category     => Value.Get ("Category"),
            Artist       => (if Value.Has_Field ("Artist") then Value.Get ("Artist") else ""),
            Album_Artist => Value.Get ("Album_Artist"),
            Composer     => (if Value.Has_Field ("Composer") then Value.Get ("Composer") else ""),
            Album        => (if Value.Has_Field ("Album") then Value.Get ("Album") else ""),
            Year         => (if Value.Has_Field ("Year") then Value.Get ("Year") else No_Year),
            Title        => Value.Get ("Title"),
            Track        => (if Value.Has_Field ("Track") then Value.Get ("Track") else No_Track),
            Last_Downloaded =>
              (if Value.Has_Field ("Last_Downloaded") then Value.Get ("Last_Downloaded") else Default_Time_String),
            Prev_Downloaded =>
              (if Value.Has_Field ("Prev_Downloaded") then Value.Get ("Prev_Downloaded") else Default_Time_String),
            Play_Before => (if Value.Has_Field ("Play_Before") then Value.Get ("Play_Before") else Null_ID),
            Play_After => (if Value.Has_Field ("Play_After") then Value.Get ("Play_After") else Null_ID));
      end if;
   end Update_JSON;

   procedure Mark_Deleted
     (DB       : in Database;
      Position : in Cursor'Class;
      Deleted  : in Time_String := Default_Time_String)
   is
      use GNATCOLL.SQL.Exec;
   begin
      Checked_Execute
        (DB,
         Statement => "UPDATE Song SET Deleted = ? WHERE ID =?",
         Params    =>
           (+(if Deleted = Default_Time_String then UTC_Image (Ada.Calendar.Clock) else Deleted),
            +Position.ID));
   end Mark_Deleted;

   procedure Really_Delete
     (DB : in Database;
      ID : in Song_ID)
   is
      use GNATCOLL.SQL.Exec;
   begin
      Checked_Execute (DB, "DELETE FROM Song WHERE ID = ?", Params => (1 => +ID));
   end Really_Delete;

   function Image (Item : Field_Values) return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String;
      Need_Comma : Boolean := False;
   begin
      for I in Fields loop
         if Length (Item (I)) > 0 then
            if Need_Comma then
               Result := Result & ", ";
            end if;

            Result     := Result & Field_Image (I) & " : '" & (-Item (I)) & "'";
            Need_Comma := True;
         end if;
      end loop;

      return -Result;
   end Image;

   function Valid_Field (Item : in String) return Boolean
   is
      Lc_Item : constant String := Ada.Characters.Handling.To_Lower (Item);
   begin
      for I of Field_Image loop
         if Lc_Item = -I then
            return True;
         end if;
      end loop;
      return False;
   end Valid_Field;

   procedure Update
     (DB       : in Database;
      Position : in Cursor'Class;
      Data     : in Field_Values)
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.SQL.Exec;

      Statement : Unbounded_String := +"UPDATE Song SET ";

      Params : SQL_Parameters (1 .. 4) := (others => Null_Parameter);

      Need_Comma : Boolean := False;
      Last       : Integer := 0;

      procedure Add_Param (Name : in String; Value : in String)
      is begin
         if Need_Comma then
            Statement := Statement & ", ";
         end if;
         Need_Comma := True;

         Statement := Statement & Name & " = ?";
         Last := Last + 1;
         Params (Last) := +Value;
      end Add_Param;
   begin
      Add_Param ("Modified", UTC_Image (Ada.Calendar.Clock));

      for Field in Fields loop
         if Length (Data (Field)) > 0 then
            Add_Param (-Field_Image (Field), -Data (Field));
         end if;
      end loop;

      Statement     := Statement & " WHERE ID = ?";
      Last          := Last + 1;
      Params (Last) := +Position.ID;

      Checked_Execute (DB, -Statement, Params (1 .. Last));
   end Update;

   function Find_Like
     (DB       : in Database'Class;
      Param    : in Field_Values;
      Order_By : in Field_Array)
     return Cursor
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.SQL.Exec;
      Statement  : Unbounded_String := +"SELECT * FROM Song WHERE ";
      Need_And   : Boolean          := False;
      Need_Comma : Boolean          := False;
      Params     : GNATCOLL.SQL.Exec.SQL_Parameters (1 .. 3);
      Last       : Integer          := 0;
   begin
      for I in Param'Range loop
         if Length (Param (I)) > 0 then
            if Need_And then
               Statement := Statement & " AND ";
            end if;

            Statement     := Statement & Field_Image (I) & " like ?";
            Last          := Last + 1;
            Params (Last) := +("%" & Param (I) & "%");
            Need_And      := True;
         end if;
      end loop;

      Statement := Statement & " ORDER BY ";
      for Field of Order_By loop
         if Need_Comma then
            Statement := Statement & ", ";
         end if;
         Statement  := Statement & Field_Image (Field);
         Need_Comma := True;
      end loop;

      return Checked_Fetch (DB, -Statement, Params (1 .. Last));
   end Find_Like;

   function Find_Like
     (DB       : in Database'Class;
      Search   : in String;
      Order_By : in Field_Array)
     return Cursor
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.SQL.Exec;
      Spaces      : array (1 .. 20) of Integer;
      Spaces_Last : Integer := Spaces'First - 1;
      First       : Integer := Search'First;

      Statement   : Unbounded_String := +"SELECT * FROM Song WHERE";
      Need_And    : Boolean          := False;
      Need_Comma  : Boolean          := False;
      Params      : GNATCOLL.SQL.Exec.SQL_Parameters (1 .. 60);
      Params_Last : Integer          := Params'First - 1;
   begin
      for I in Search'Range loop
         if Search (I) = ' ' then
            Spaces_Last := Spaces_Last + 1;
            Spaces (Spaces_Last) := I;
         end if;
      end loop;

      for I in 1 .. Spaces_Last + 1 loop
         if Need_And then
            Statement := Statement & " and";
         end if;

         declare
            Need_Or : Boolean := False;

            Word : constant String := Search (First .. (if I > Spaces_Last then Search'Last else Spaces (I) - 1));
         begin
            for J in Fields'Range loop
               Statement            := Statement & (if Need_Or then " or " else " (") & Field_Image (J) & " like ?";
               exit when Params_Last = Params'Last;
               Params_Last          := Params_Last + 1;
               Params (Params_Last) := +("%" & Word & "%");
               Need_Or              := True;
            end loop;
            Statement := Statement & ")";
         end;

         if I <= Spaces_Last then
            First := Spaces (I) + 1;
         end if;
         Need_And := True;
      end loop;

      Statement := Statement & " ORDER BY ";
      for Field of Order_By loop
         if Need_Comma then
            Statement := Statement & ", ";
         end if;
         Statement  := Statement & Field_Image (Field);
         Need_Comma := True;
      end loop;

      return Checked_Fetch (DB, -Statement, Params (1 .. Params_Last));
   end Find_Like;

   procedure Next (Position : in out Cursor)
   is begin
      Position.Cursor.Next;
   end Next;

   function Row (Position : in Cursor) return Positive
   is begin
      return Position.Cursor.Current;
   end Row;

   procedure Set_Row (Position : in out Cursor; Row : in Positive)
   is begin
      Position.Cursor.Absolute (Row);
   end Set_Row;

   function Field (Position : in Cursor; Item : in Fields) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Field_Fields (Item))
         then ""
         else Position.Cursor.Value (Field_Fields (Item)));
   end Field;

   function ID (Position : in Cursor) return Integer
   is begin
      return Integer'Value (Position.Cursor.Value (ID_Field));
   end ID;

   function ID_String (Position : in Cursor) return String
   is begin
      return Position.Cursor.Value (ID_Field);
   end ID_String;

   function Modified (Position : in Cursor) return Time_String
   is begin
      return Position.Cursor.Value (Modified_Field);
   end Modified;

   function Deleted (Position : in Cursor) return Time_String
   is begin
      return Position.Cursor.Value (Deleted_Field);
   end Deleted;

   function File_Name (Position : in Cursor) return String
   is begin
      return Position.Cursor.Value (File_Name_Field);
   end File_Name;

   function Category (Position : in Cursor) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Category_Field)
         then ""
         else Position.Cursor.Value (Category_Field));
   end Category;

   function Artist (Position : in Cursor) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Artist_Field)
         then ""
         else Position.Cursor.Value (Artist_Field));
   end Artist;

   function Album (Position : in Cursor) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Album_Field)
         then ""
         else Position.Cursor.Value (Album_Field));
   end Album;

   function Album_Artist (Position : in Cursor) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Album_Artist_Field)
         then ""
         else Position.Cursor.Value (Album_Artist_Field));
   end Album_Artist;

   function Song_Name (Position : in Cursor) return SMM.Song_Name
   is
      use Ada.Strings.Unbounded;
   begin
      return SMM.Song_Name'
        (Album_Artist =>
           (if Position.Cursor.Is_Null (Album_Artist_Field)
            then Null_Unbounded_String
            else +Position.Cursor.Value (Album_Artist_Field)),
         Album =>
           (if Position.Cursor.Is_Null (Album_Field)
            then Null_Unbounded_String
            else +Position.Cursor.Value (Album_Field)),
         Title =>
           (if Position.Cursor.Is_Null (Title_Field)
            then Null_Unbounded_String
            else +Position.Cursor.Value (Title_Field)));
   end Song_Name;

   function Composer (Position : in Cursor) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Composer_Field)
         then ""
         else Position.Cursor.Value (Composer_Field));
   end Composer;

   function Title (Position : in Cursor) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Title_Field)
         then ""
         else Position.Cursor.Value (Title_Field));
   end Title;

   function Year (Position : in Cursor) return Integer
   is begin
      return
        (if Position.Cursor.Is_Null (Year_Field)
         then No_Year
         else Integer'Value (Position.Cursor.Value (Year_Field)));
   end Year;

   function Track (Position : in Cursor) return Integer
   is begin
      return
        (if Position.Cursor.Is_Null (Track_Field)
         then No_Track
         else Integer'Value (Position.Cursor.Value (Track_Field)));
   end Track;

   function Last_Downloaded (Position : in Cursor) return Time_String
   is begin
      return
        (if Position.Cursor.Is_Null (Last_Downloaded_Field)
         then Default_Time_String
         else Position.Cursor.Value (Last_Downloaded_Field));
   end Last_Downloaded;

   function Prev_Downloaded (Position : in Cursor) return Time_String
   is begin
      return
        (if Position.Cursor.Is_Null (Prev_Downloaded_Field)
         then Default_Time_String
         else Position.Cursor.Value (Prev_Downloaded_Field));
   end Prev_Downloaded;

   function Play_After (Position : in Cursor) return Integer
   is begin
      return
        (if Position.Cursor.Is_Null (Play_After_Field)
         then Null_ID
         else Integer'Value (Position.Cursor.Value (Play_After_Field)));
   end Play_After;

   function Play_Before (Position : in Cursor) return Integer
   is begin
      return
        (if Position.Cursor.Is_Null (Play_Before_Field)
         then Null_ID
         else Integer'Value (Position.Cursor.Value (Play_Before_Field)));
   end Play_Before;

   function Category_Contains (Position : in Cursor; Item : in String) return Boolean
   is
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;
   begin
      return 0 /= Index (Source => To_Lower (Position.Category), Pattern => To_Lower (Item));
   end Category_Contains;

   function Category_First (Position : in Cursor) return String
   is
      use Ada.Strings.Fixed;
      Data : constant String := Position.Category;
      Last : constant Integer := Index (Source => Data, Pattern => ",");
   begin
      return Data (Data'First .. (if Last = 0 then Data'Last else Last - 1));
   end Category_First;

   function Play_After_Is_Present (Position : in Cursor) return Boolean
   is begin
      return not Position.Cursor.Is_Null (Play_After_Field);
   end Play_After_Is_Present;

   function Play_Before_Is_Present (Position : in Cursor) return Boolean
   is begin
      return not Position.Cursor.Is_Null (Play_Before_Field);
   end Play_Before_Is_Present;

   procedure Write_Last_Downloaded
     (Position : in Cursor;
      DB       : in Database'Class;
      Time     : in Time_String)
   is
      use GNATCOLL.SQL.Exec;
   begin
      Checked_Execute
        (DB,
         Statement => "UPDATE Song SET Modified = ?, Last_Downloaded = ?, Prev_Downloaded = ? WHERE ID =?",
         Params    => (+Time, +Time, +Position.Last_Downloaded, +Position.ID));
   end Write_Last_Downloaded;

   procedure Write_Play_Before_After
     (DB        : in Database'Class;
      Before_ID : in Integer;
      After_ID  : in Integer)
   is
      use GNATCOLL.SQL.Exec;
   begin
      Checked_Execute
        (DB,
         Statement => "UPDATE Song SET Modified = ?, Play_Before = ? WHERE ID =?",
         Params    => (+UTC_Image (Ada.Calendar.Clock), +After_ID, +Before_ID));

      Checked_Execute
        (DB,
         Statement => "UPDATE Song SET Modified = ?, Play_After = ? WHERE ID =?",
         Params    => (+UTC_Image (Ada.Calendar.Clock), +Before_ID, +After_ID));
   end Write_Play_Before_After;

   function Read_Schema_Version (DB : in Database'Class) return Integer
   is
      Table_Cur : constant Cursor := Checked_Fetch
        (DB, "SELECT name FROM sqlite_master WHERE type='table' AND name='Schema_Version'");
   begin
      if Has_Element (Table_Cur) then
         declare
            Version_Cur : constant Cursor := Checked_Fetch (DB, "SELECT Version FROM Schema_Version WHERE ID=1");
         begin
            return Integer'Value (Version_Cur.Cursor.Value (GNATCOLL.SQL.Exec.Field_Index'First));
         end;
      else
         return 0;
      end if;
   end Read_Schema_Version;

end SMM.Database;
