--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

with AUnit.Assertions;
with SAL.AUnit;
package body Test_Books.String_Lists is

   procedure Check (Label : in String; Computed, Expected : in String_List_Type)
   is
      use SAL.AUnit;
      use type Glib.Gint;
   begin
      Check (Label & ".count", Computed'Length, Expected'Length);
      for I in Computed'Range loop
         Check (Label & Glib.Gint'Image (I), -Computed (I), -Expected (Expected'First + I - Computed'First));
      end loop;
   end Check;

   function "+" (Right : in String) return String_List_Type
   is begin
      return (0 => +Right);
   end "+";

   function "+" (Right : in String_List_Type) return String_Table_Type
   is
      Table : String_Table_Type;
   begin
      String_Tables.Add (Table, Right);
      return Table;
   end "+";

   procedure Check (Computed, Expected : in String_Table_Type)
   is
      use AUnit.Assertions;
      use String_Tables;
      Computed_Iterator : Iterator_Type := First (Computed);
      Expected_Iterator : Iterator_Type := First (Expected);
      Row               : Integer       := 0;
   begin
      loop
         exit when Is_Done (Computed_Iterator);

         Assert (not Is_Done (Expected_Iterator), "not all of Computed compared");

         declare
            Computed : String_List_Access_Type renames Current (Computed_Iterator);
            Expected : String_List_Access_Type renames Current (Expected_Iterator);
            use type Glib.Gint;
         begin
            for Column in Computed.all'Range loop
               declare
                  Computed_String : constant String := -Computed (Column);
                  Expected_String : constant String := -Expected (Expected'First + Column - Computed'First);
               begin
                  Assert
                    (Computed_String = Expected_String,
                     "(" & Integer'Image (Row) & "," & Glib.Gint'Image (Column) & ") got '" &
                       Computed_String & "', expected '" & Expected_String & "'");
               end;
            end loop;
         end;
         Row := Row + 1;
         Next (Computed_Iterator);
         Next (Expected_Iterator);
      end loop;
      Assert (Is_Null (Expected_Iterator), "not all of Expected compared");
   end Check;

end Test_Books.String_Lists;
