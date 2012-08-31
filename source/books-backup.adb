--  Abstract :
--
--  Driver for backup; dump database to text files.
--
--  Copyright (C) 2002 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--

with Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with Books.Database.Data_Tables.Author;
with Books.Database.Data_Tables.Collection;
with Books.Database.Data_Tables.Series;
with Books.Database.Data_Tables.Title;
with Interfaces;
procedure Books.Backup
is
   DB : Books.Database.Database_Access := new Books.Database.Database;
   --  Initialize connects to Books database

   Author_Table     : Books.Database.Data_Tables.Author.Table (DB);
   Collection_Table : Books.Database.Data_Tables.Collection.Table (DB);
   Series_Table     : Books.Database.Data_Tables.Series.Table (DB);
   Title_Table      : Books.Database.Data_Tables.Title.Table (DB);

   Warm_Fuzzy_Count : Integer := 0;

   Output_File : File_Type;

   procedure Warm_Fuzzy
   is begin
      Put ('.');
      Warm_Fuzzy_Count := Warm_Fuzzy_Count + 1;
      if Warm_Fuzzy_Count mod 100 = 0 then
         New_Line;
      end if;
   end Warm_Fuzzy;

   procedure Finalize
   is
      use type Books.Database.Database_Access;
   begin
      if DB /= null then
         --  Need to explicitly finalize tables before freeing DB,
         --  since the tables reference DB when they finalize. An
         --  alternate design would be to allocate the tables, and
         --  free them in the proper order.
         Books.Database.Data_Tables.Author.Finalize (Author_Table);
         Books.Database.Data_Tables.Collection.Finalize (Collection_Table);
         Books.Database.Data_Tables.Series.Finalize (Series_Table);
         Books.Database.Data_Tables.Title.Finalize (Title_Table);
         Books.Database.Free (DB);
      end if;

      if Is_Open (Output_File) then
         Close (Output_File);
      end if;
   end Finalize;

   generic
      Table_Name       : in String;
      Output_File_Name : in String;
      type Table_Type is new Books.Database.Table with private;
      Table            : in out Table_Type;
      with function Image (Table : in Table_Type) return String;
   procedure Gen_Copy_Table;

   procedure Gen_Copy_Table
   is
   begin
      Create (Output_File, Out_File, Output_File_Name);
      Put_Line ("Dumping " & Table_Name & " table to " & Output_File_Name);
      Books.Database.Find_All_By_ID (Table);
      loop
         Warm_Fuzzy;
         Put_Line (Output_File, Image (Table));

         begin
            Books.Database.Next (Table);
         exception
         when Books.Database.No_Data =>
            exit;
         end;

      end loop;
      New_Line;
      Put_Line (Output_File_Name & " done");
      New_Line;
      Close (Output_File);
   end Gen_Copy_Table;

   function Author_Image (Table : in Books.Database.Data_Tables.Author.Table) return String
   is
      use Books.Database.Data_Tables.Author;
   begin
      return
        """" & First_Name (Table) &
        """,""" & Middle_Name (Table) &
        """,""" & Last_Name (Table) &
        """";
   end Author_Image;

   procedure Copy_Author is new Gen_Copy_Table
     (Table_Name       => "Author",
      Output_File_Name => "author.csv",
      Table_Type       => Books.Database.Data_Tables.Author.Table,
      Table            => Author_Table,
      Image            => Author_Image);

   function Collection_Image (Table : in Books.Database.Data_Tables.Collection.Table) return String
   is
      use Books.Database.Data_Tables.Collection;
   begin
      return
        """" & Name (Table) &
        """,""" & Books.Database.Image (Editor (Table)) &
        """,""" & Interfaces.Unsigned_16'Image (Year (Table)) &
        """";
   end Collection_Image;

   procedure Copy_Collection is new Gen_Copy_Table
     (Table_Name       => "Collection",
      Output_File_Name => "collection.csv",
      Table_Type       => Books.Database.Data_Tables.Collection.Table,
      Table            => Collection_Table,
      Image            => Collection_Image);

   function Series_Image (Table : in Books.Database.Data_Tables.Series.Table) return String
   is
      use Books.Database.Data_Tables.Series;
   begin
      return
        """" & Title (Table) &
        """,""" & Books.Database.Image (Author (Table)) &
        """";
   end Series_Image;

   procedure Copy_Series is new Gen_Copy_Table
     (Table_Name       => "Series",
      Output_File_Name => "series.csv",
      Table_Type       => Books.Database.Data_Tables.Series.Table,
      Table            => Series_Table,
      Image            => Series_Image);

   function Title_Image (Table : in Books.Database.Data_Tables.Title.Table) return String
   is
      use Books.Database.Data_Tables.Title;
   begin
      return
        """" & Title (Table) &
        """,""" & Interfaces.Unsigned_16'Image (Year (Table)) &
        """,""" & Comment (Table) &
        """,""" & Interfaces.Unsigned_8'Image (Rating (Table)) &
        """";
   end Title_Image;

   procedure Copy_Title is new Gen_Copy_Table
     (Table_Name       => "Title",
      Output_File_Name => "title.csv",
      Table_Type       => Books.Database.Data_Tables.Title.Table,
      Table            => Title_Table,
      Image            => Title_Image);

begin
   Copy_Author;
   Copy_Collection;
   Copy_Series;
   Copy_Title;

   Finalize;
exception
when E : others =>
   Put_Line ("Exception " &
               Ada.Exceptions.Exception_Name (E) & " " &
               Ada.Exceptions.Exception_Message (E));
   Finalize;
end Books.Backup;
