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
      File_Name       : in String      := "";
      Category        : in String      := "";
      Artist          : in String      := "";
      Album           : in String      := "";
      Album_Artist    : in String      := "";
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

      Params : SQL_Parameters (1 .. 11) := (others => Null_Parameter);

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
      Add_Param ("File_Name", File_Name, "");
      Add_Param ("Category", Category, "");
      Add_Param ("Artist", Artist, "");
      Add_Param ("Album", Album, "");
      Add_Param ("Album_Artist", Album_Artist, "");
      Add_Param ("Title", Title, "");
      Add_Param ("Track", Track, No_Track);
      Add_Param ("Last_Downloaded", Last_Downloaded, Default_Time_String);
      Add_Param ("Prev_Downloaded", Prev_Downloaded, Default_Time_String);
      Add_Param ("Play_Before", Play_Before, Null_ID);
      Add_Param ("Play_After", Play_After, Null_ID);

      if Update then
         Statement := Statement & " WHERE ID = ?";
      else
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

   procedure Open (DB : in out Database; File_Name : in String)
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
   end Open;

   procedure Insert
     (DB              : in Database;
      ID              : in Integer;
      File_Name       : in String;
      Category        : in String;
      Artist          : in String;
      Album           : in String;
      Album_Artist    : in String;
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
         File_Name       => File_Name,
         Category        => Category,
         Artist          => Artist,
         Album           => Album,
         Album_Artist    => Album_Artist,
         Title           => Title,
         Track           => Track,
         Last_Downloaded => Last_Downloaded,
         Prev_Downloaded => Prev_Downloaded,
         Play_Before     => Play_Before,
         Play_After      => Play_After);
   end Insert;

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
   File_Name_Field       : constant GNATCOLL.SQL.Exec.Field_Index := ID_Field + 1;
   Category_Field        : constant GNATCOLL.SQL.Exec.Field_Index := File_Name_Field + 1;
   Artist_Field          : constant GNATCOLL.SQL.Exec.Field_Index := Category_Field + 1;
   Album_Artist_Field    : constant GNATCOLL.SQL.Exec.Field_Index := Artist_Field + 1;
   Album_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Album_Artist_Field + 1;
   Title_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Album_Field + 1;
   Track_Field           : constant GNATCOLL.SQL.Exec.Field_Index := Title_Field + 1;
   Last_Downloaded_Field : constant GNATCOLL.SQL.Exec.Field_Index := Track_Field + 1;
   Prev_Downloaded_Field : constant GNATCOLL.SQL.Exec.Field_Index := Last_Downloaded_Field + 1;
   Play_Before_Field     : constant GNATCOLL.SQL.Exec.Field_Index := Prev_Downloaded_Field + 1;
   Play_After_Field      : constant GNATCOLL.SQL.Exec.Field_Index := Play_Before_Field + 1;

   Field_Fields : constant array (Fields) of GNATCOLL.SQL.Exec.Field_Index :=
     (Artist       => Artist_Field,
      Album        => Album_Field,
      Album_Artist => Album_Artist_Field,
      Category     => Category_Field,
      Title        => Title_Field,
      Track        => Track_Field);

   function Has_Element (Position : Cursor) return Boolean
   is begin
      return Position.Cursor.Has_Row;
   end Has_Element;

   function First (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT * FROM Song ORDER BY ID ASC";
   begin
      return Checked_Fetch (DB, Statement);
   end First;

   function Last (DB : in Database'Class) return Cursor
   is
      Statement : constant String := "SELECT * FROM Song ORDER BY ID DESC";
   begin
      return Checked_Fetch (DB, Statement);
   end Last;

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

   procedure Update
     (DB              : in Database;
      Position        : in Cursor'Class;
      File_Name       : in String      := "";
      Category        : in String      := "";
      Artist          : in String      := "";
      Album           : in String      := "";
      Album_Artist    : in String      := "";
      Title           : in String      := "";
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
         File_Name       => File_Name,
         Category        => Category,
         Artist          => Artist,
         Album           => Album,
         Album_Artist    => Album_Artist,
         Title           => Title,
         Track           => Track,
         Last_Downloaded => Last_Downloaded,
         Prev_Downloaded => Prev_Downloaded,
         Play_Before     => Play_Before,
         Play_After      => Play_After);
   end Update;

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
      First       : Integer := Spaces'First;

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

   function Title (Position : in Cursor) return String
   is begin
      return
        (if Position.Cursor.Is_Null (Title_Field)
         then ""
         else Position.Cursor.Value (Title_Field));
   end Title;

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
      use Ada.Strings.Fixed;
   begin
      return 0 /= Index (Source => Position.Category, Pattern => Item);
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
         Statement => "UPDATE Song SET Last_Downloaded = ?, Prev_Downloaded = ? WHERE ID =?",
         Params    => (+Time, +Position.Last_Downloaded, +Position.ID));
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
         Statement => "UPDATE Song SET Play_Before = ? WHERE ID =?",
         Params    => (+After_ID, +Before_ID));

      Checked_Execute
        (DB,
         Statement => "UPDATE Song SET Play_After = ? WHERE ID =?",
         Params    => (+Before_ID, +After_ID));
   end Write_Play_Before_After;

end SMM.Database;
