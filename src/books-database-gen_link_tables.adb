--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002, 2004, 2009, 2012 Stephen Leake.  All Rights Reserved.
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

package body Books.Database.Gen_Link_Tables is

   function To_Params (Data : in Source_Array_ID_Type) return GNATCOLL.SQL.Exec.SQL_Parameters
   is
      use GNATCOLL.SQL.Exec;
   begin
      return
        (1 => +Data (Source_Array_ID_Type'First),
         2 => +Data (Source_Array_ID_Type'Last));
   end To_Params;

   procedure Delete (T : in out Table; Data : in Source_Array_ID_Type)
   is begin
      Checked_Execute (T, T.Delete_By_ID_Statement.all, To_Params (Data));
   end Delete;

   procedure Find (T : in out Table; Source : in Source_Labels_Type; Item : in ID_Type)
   is
      use type GNATCOLL.SQL.Exec.SQL_Parameter;
   begin
      Find (T, T.Find_By_Source_Statement (Source), Params => (1 => +Item));
   end Find;

   function ID (T : in Table; Source : in Source_Labels_Type) return ID_Type is
   begin
      return ID_Type'Value (Field (T, Source_Labels_Type'Pos (Source)));
   end ID;

   overriding procedure Initialize (T : in out Table)
   is
      First_Column_Name : constant String := Source_Labels_Type'Image (Source_Labels_Type'First);
      Last_Column_Name  : constant String := Source_Labels_Type'Image (Source_Labels_Type'Last);

      Table_Name : constant String := First_Column_Name & Last_Column_Name;
   begin

      for I in Source_Labels_Type loop
         T.Find_By_Source_Statement (I) := new String'
           ("SELECT " &
              First_Column_Name &
              ", " &
              Last_Column_Name &
              " FROM " &
              Table_Name &
              " WHERE " &
              Source_Labels_Type'Image (I) &
              " = ?");
      end loop;

      T.Insert_Statement := new String'
        ("INSERT INTO " &
           Table_Name &
           "(" &
           First_Column_Name &
           ", " &
           Last_Column_Name &
           ") VALUES (?, ?)");

      T.Delete_By_ID_Statement := new String'
        ("DELETE FROM " &
           Table_Name &
           " WHERE " &
           First_Column_Name & " = ? AND " &
           Last_Column_Name & " = ?");

   end Initialize;

   procedure Insert (T : in out Table; Data : in Source_Array_ID_Type)
   is begin
      Checked_Execute (T, T.Insert_Statement.all, To_Params (Data));
   end Insert;

end Books.Database.Gen_Link_Tables;
