--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2016, 2025  All Rights Reserved.
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

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Interfaces;
with SAL.Network_Order;
package body SMM.Database_Remote is

   procedure Free (Pointer : in out Database_Access)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (Database'Class, Database_Access);
   begin
      Deallocate (Pointer);
   end Free;

   function Input_Network_String
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Network_String
   is
      use Ada.Streams;
      use Interfaces;
      use SAL.Network_Order;

      First    : Integer_32;
      Last     : Integer_32;
      Str      : Stream_Element_Array (1 .. 8);
      Str_Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Str, Str_Last);
      if Str_Last < Str'Last then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      Str_Last := Str'First - 1;

      From_Network (First, Str, Str_Last);
      From_Network (Last, Str, Str_Last);

      return
         Result : Network_String (Integer (First) .. Integer (Last))
      do
         String'Read (Stream, String (Result));
      end return;
   end Input_Network_String;

   procedure Output_Network_String
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Network_String)
   is
      use Ada.Streams;
      use Interfaces;
      use SAL.Network_Order;

      First    : constant Integer_32   := Integer_32 (Item'First);
      Last     : constant Integer_32   := Integer_32 (Item'Last);
      Str      : Stream_Element_Array (1 .. 8);
      Str_Last : Stream_Element_Offset := Str'First - 1;
   begin
      To_Network (First, Str, Str_Last);
      To_Network (Last, Str, Str_Last);

      Write (Stream.all, Str);
      Network_String'Write (Stream, Item);
   end Output_Network_String;

end SMM.Database_Remote;
