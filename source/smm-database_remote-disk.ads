--  Abstract :
--
--  Access to a "remote" database that is actually a local disk file.
--  This is used by SMM.Remote_Server, and is also useful for unit
--  tests.
--
--  Copyright (C) 2025  All Rights Reserved.
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

with SMM.Database;
package SMM.Database_Remote.Disk is

   --  WORKAROUND: using 'not null access SMM.Database.Database'
   --  for the discriminant encounters a gnat bug.
   type Database (DB : SMM.Database.Database_Not_Null_Access) is new Database_Remote.Database with null record;

   overriding function Get_JSON
     (DB : in Database;
      ID : in Song_ID)
     return GNATCOLL.JSON.JSON_Value;

   overriding function Get_Last_ID (DB : in out Database) return Song_ID;

   overriding function Get_Modified
     (DB       : in out Database;
      ID       : in     Song_ID;
      Modified : in     Time_String)
     return ID_Lists.List;

   overriding function Get_New
     (DB        : in out Database;
      ID        : in     Song_ID;
      Max_Count : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last)
     return ID_Lists.List;

   overriding function Index_Fields_Equal
     (DB        : in out Database;
      ID        : in     Song_ID;
      New_Value : in     GNATCOLL.JSON.JSON_Value)
     return Boolean;

   overriding procedure Apply
     (DB  : in out Database;
      Msg : in     GNATCOLL.JSON.JSON_Value);

end SMM.Database_Remote.Disk;
