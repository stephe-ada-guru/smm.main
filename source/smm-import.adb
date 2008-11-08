--  Abstract :
--
--  Import new files into SMM db.
--
--  Copyright (C) 2008 Stephen Leake.  All Rights Reserved.
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

with Ada.Directories;
with Ada.Text_IO;
with SAL.Config_Files;
with SAL.Time_Conversions;
procedure SMM.Import
  (Db   : in out SAL.Config_Files.Configuration_Type;
   Root : in     String)
is
   procedure Import_Dir (Dir : in String)
   is
      use Ada.Directories;
      use SAL.Config_Files;
      use SAL.Time_Conversions;

      procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
      is
         Name : constant String := Relative_Name (Root, Full_Name (Dir_Entry));
      begin
         case Kind (Dir_Entry) is
         when Directory =>
            if Simple_Name (Dir_Entry) = "." or
              Simple_Name (Dir_Entry) = ".."
            then
               return;
            end if;

            Import_Dir (Name);

         when Ordinary_File =>
            if not Is_Present (db, Name) then
               if Verbosity > 0 then
                  Ada.Text_IO.Put_Line ("adding file " & Name);
               end if;
               Write_String (Db,  Name & ".Last_Played", Time_Type'Image (0.0));
            end if;

         when Special_File =>
            raise SAL.Programmer_Error with "found special file";
         end case;
      end Process_Dir_Entry;
   begin
      if Verbosity > 1 then
         Ada.Text_IO.Put_Line ("reading directory " & Dir);
      end if;

      if Exists (Dir) then
         Search
           (Dir,
            Pattern          => "*",
            Filter           =>
              (Ordinary_File => True,
               Directory     => True,
               Special_File  => False),
            Process          => Process_Dir_Entry'Access);
      else
         Ada.Text_IO.Put_Line (Dir & " does not exist");
      end if;
   end Import_Dir;

begin
   Ada.Directories.Set_Directory (Root);
   Import_Dir (Root);
end SMM.Import;
