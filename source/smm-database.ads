--  Abstract :
--
--  Interface to SQLite3 database
--
--  Copyright (C) 2018 - 2020, 2025, 2026 Stephen Leake All Rights Reserved.
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

with Ada.Calendar;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with GNATCOLL.SQL.Exec;
package SMM.Database is

   Schema_Version : constant Integer := 2;
   --  Increment each time create_schema.sql is changed. Must match
   --  create_schema.sql Schema_Version.Version.

   Schema_Version_Error : exception;
   No_Data              : exception;
   Null_Field           : exception;
   Entry_Error          : exception; --  User violated some limit or index constraint

   Null_ID  : constant Integer := -1;
   No_Track : constant Integer := -1;
   No_Year  : constant Integer := -1;

   type Database is new Ada.Finalization.Limited_Controlled with private;
   type Database_Not_Null_Access is not null access all Database'Class;

   overriding procedure Finalize (DB : in out Database);
   --  Disconnect from database.

   procedure Open (DB : in out Database; File_Name : in String; Expected_Schema : in Integer := Schema_Version);
   --  If File_Name exists, open it. If not, create it.
   --
   --  Raises Schema_Version_Error with message containing expected,
   --  found if db Schema_Version.Version is not Expected_Schema.

   procedure Close (DB : in out Database);

   procedure Insert
     (DB              : in Database;
      ID              : in Song_ID;
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
      Play_Before     : in Song_ID     := Null_ID;
      Play_After      : in Song_ID     := Null_ID;
      Modified        : in Time_String := Default_Time_String);
   --  If Modified = Default_Time_String, sets Modified to Clock. Sets
   --  Deleted to null.

   procedure Insert_JSON (DB : in Database; Value : in GNATCOLL.JSON.JSON_Value);
   --  Calls Insert, getting values from Value. Value must have structure
   --  returned by Get_JSON.

   function Get_JSON (DB : in Database; ID : in Song_ID) return GNATCOLL.JSON.JSON_Value;
   --  Return all known data for ID. Result structure is either {id,
   --  deleted} or {id, modified, data: {...}}. This allows
   --  comparing data without id, modified, deleted.

   function Index_Fields_Equal
     (DB        : in out Database;
      ID        : in     Song_ID;
      New_Value : in     GNATCOLL.JSON.JSON_Value)
     return Boolean;
   --  If this returns True, Insert (New_Value) would raise a database
   --  exception for colliding values. If it returns False, Insert
   --  will not raise an exception.

   function UTC_Image (Item : in Ada.Calendar.Time) return Time_String;

   ----------
   --  Iterate over db contents

   type Cursor is tagged private;
   --  We'd like to be able to do:
   --
   --  1)
   --    declare
   --       I : Cursor := DB.First;
   --    begin
   --       ...
   --       I.Next;
   --
   --  2)
   --    declare
   --       I : Cursor
   --    begin
   --       ...
   --       I := DB.Find ...;
   --       ...
   --       I.Next;
   --
   --  3)
   --    declare
   --       I : Cursor := DB.Find_Like (..);
   --       J : Cursor;
   --    begin
   --       ...
   --       J := I;
   --       I.Next; -- must not change J
   --
   --  Ada forbids two dispatching parameters, so these functions
   --  dispatch either on DB or Cursor; the other must be classwide.
   --
   --  1) requires dispatching on DB, which means Cursor is classwide;
   --  that means it must be initialized, which forbids 2).
   --
   --  3) Requires GNATCOLL.SQL.Exec.Direct_Cursor, which only gives us:
   --    declare
   --       I : Cursor := DB.Find_Like (..);
   --       J : Positive;
   --    begin
   --       ...
   --       J := I.Row;
   --       I.Next;
   --       ...
   --       I.Set_Row (J);

   function Count (Position : in Cursor) return Natural;
   --  Count of items Next will visit.

   function Has_Element (Position : in Cursor) return Boolean;

   function Get_JSON (Position : in Cursor) return GNATCOLL.JSON.JSON_Value;
   --  Return all known data for Position. See Get_JSON (ID) for
   --  structure of result.

   function First_By_ID (DB : in Database'Class) return Cursor;
   --  Increasing ID order.

   function Last_By_ID (DB : in Database'Class) return Cursor;
   --  Decreasing ID order.

   function First_By_Last_Downloaded (DB : in Database'Class) return Cursor;
   --  Increasing Last_Downloaded order.

   function Last_By_Last_Downloaded (DB : in Database'Class) return Cursor;
   --  Decreasing Last_Downloaded order.

   function First_By_Name (DB : in Database'Class) return Cursor;
   --  Increasing <album_artist>_<album>_<title> order.

   function Find_File_Name (DB : in Database'Class; File_Name : in String) return Cursor;
   function Find_ID (DB : in Database'Class; ID : in Song_ID) return Cursor;

   function Last_ID (DB : in Database) return Song_ID;

   function Get_Modified
     (DB       : in out Database;
      ID       : in     Song_ID;
      Modified : in     Time_String)
     return ID_Lists.List;
   --  Get a list of Song IDs with Song.ID <= ID and Song.Modified |
   --  Song.Deleted > Modified.
   --
   --  Result is in ID order.

   function Get_New
     (DB        : in out Database;
      ID        : in     Song_ID;
      Max_Count : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last)
     return ID_Lists.List;
   --  Get a list of up to Max_Count Song IDs > ID.
   --
   --  Result is in ID order.

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
      Play_Before     : in Song_ID     := Null_ID;
      Play_After      : in Song_ID     := Null_ID;
      Modified        : in Time_String := Default_Time_String);
   --  Items that are the defaults are not updated.
   --  Cursor must be refetched to reflect changes.
   --
   --  If Modified = Default_Time_String, sets Modified to Clock.

   procedure Update_JSON (DB : in Database; Value : in GNATCOLL.JSON.JSON_Value);
   --  Calls Update, getting values from Value. Value must have structure
   --  returned by DB.Get_JSON, except any fields other than ID, Modified
   --  may be empty (not updated).

   procedure Mark_Deleted
     (DB       : in Database;
      Position : in Cursor'Class;
      Deleted : in Time_String := Default_Time_String);
   --  Mark item at Position as 'deleted'. If Deleted is
   --  Default_Time_String, set deleted time to Clock.

   procedure Really_Delete
     (DB : in Database;
      ID : in Song_ID);
   --  Actually delete the record; used only to resolve add/add conflicts.

   type Fields is (Artist, Album, Album_Artist, Composer, Title, Year, Category, Track, Play_Before, Play_After);
   subtype Required_Fields is Fields range Artist .. Category;
   subtype Min_Required_Fields is Fields range Artist .. Album_Artist;

   type Field_Values is array (Fields) of Ada.Strings.Unbounded.Unbounded_String;

   type Field_Array is array (Natural range <>) of Fields;

   function Image (Item : Field_Values) return String;
   --  User-friendly image, good for "not found" error messages.

   Field_Image : constant Field_Values :=
     (Artist       => +"artist",
      Album        => +"album",
      Album_Artist => +"album_artist",
      Composer     => +"composer",
      Title        => +"title",
      Year         => +"year",
      Category     => +"category",
      Track        => +"track",
      Play_Before  => +"play_before",
      Play_After   => +"play_after");

   function Valid_Field (Item : in String) return Boolean;
   --  True if Item in Field_Image (case insensitive).

   procedure Update
     (DB       : in Database;
      Position : in Cursor'Class;
      Data     : in Field_Values);
   --  Cursor must be refetched to reflect changes.
   --
   --  Sets Modified to Clock.

   function Find_Like
     (DB       : in Database'Class;
      Param    : in Field_Values;
      Order_By : in Field_Array)
     return Cursor;

   function Find_Like
     (DB       : in Database'Class;
      Search   : in String;
      Order_By : in Field_Array)
     return Cursor;
   --  Match Search against Fields.
   --  Search is a space-separated list of values for Fields.

   procedure Next (Position : in out Cursor);

   function Row (Position : in Cursor) return Positive;
   procedure Set_Row (Position : in out Cursor; Row : in Positive);

   function Field (Position : in Cursor; Item : in Fields) return String;

   function ID (Position : in Cursor) return Song_ID;
   function ID_String (Position : in Cursor) return String;
   function Modified (Position : in Cursor) return Time_String;
   function Deleted (Position : in Cursor) return String; -- Empty string if null
   function File_Name (Position : in Cursor) return String;
   function Category (Position : in Cursor) return String;
   function Artist (Position : in Cursor) return String;
   function Album (Position : in Cursor) return String;
   function Album_Artist (Position : in Cursor) return String;
   function Song_Name (Position : in Cursor) return SMM.Song_Name
   with Pre => Has_Element (Position);
   function Composer (Position : in Cursor) return String;
   function Title (Position : in Cursor) return String;
   function Year (Position : in Cursor) return Integer;
   function Track (Position : in Cursor) return Integer;
   function Last_Downloaded (Position : in Cursor) return Time_String;
   function Prev_Downloaded (Position : in Cursor) return Time_String;
   function Play_After (Position : in Cursor) return Integer;
   function Play_Before (Position : in Cursor) return Integer;

   function Category_Contains (Position : in Cursor; Item : in String) return Boolean;
   --  "Category" is a comma separated list of labels; return True if
   --  Item equals one of those labels, using case insensitive compare.

   function Category_First (Position : in Cursor) return String;
   --  First item in category list.

   function Play_After_Is_Present (Position : in Cursor) return Boolean;
   function Play_Before_Is_Present (Position : in Cursor) return Boolean;

   procedure Write_Last_Downloaded
     (Position : in Cursor;
      DB       : in Database'Class;
      Time     : in Time_String);
   --  Sets Modified to Time.

   procedure Write_Play_Before_After
     (DB        : in Database'Class;
      Before_ID : in Song_ID;
      After_ID  : in Song_ID);
   --  Sets Modified to Clock.

   function Read_Schema_Version (DB : in Database'Class) return Integer;
   --  Returns 0 if table Schema_Version does not exist.

private

   type Database is new Ada.Finalization.Limited_Controlled with
   record
      Connection : GNATCOLL.SQL.Exec.Database_Connection;
   end record;

   type Cursor is tagged record
      --  GNATCOLL.SQL.Exec Finalize releases cursor
      Cursor : GNATCOLL.SQL.Exec.Direct_Cursor;
   end record;

   ----------
   --  Visible for child packages

   procedure Checked_Execute
     (DB        : in Database'Class;
      Statement : in String;
      Params    : in GNATCOLL.SQL.Exec.SQL_Parameters := GNATCOLL.SQL.Exec.No_Parameters);

end SMM.Database;
