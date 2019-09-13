--  Abstract :
--
--  Check against db, report missing in either.
--
--  Copyright (C) 2016 - 2019 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with SAL;
with SMM.Database;
procedure SMM.Check
  (DB          : in out SMM.Database.Database;
   Source_Root : in     String)
is
   DB_Count   : Integer := 0;
   Disk_Count : Integer := 0;
   Failed     : Integer := 0;

   procedure Check_Before_After_Exists
   is
      use SMM.Database;
      I : Cursor := First (DB);
   begin
      loop
         exit when not I.Has_Element;

         DB_Count := DB_Count + 1;

         declare
            File_Name : constant String := Source_Root & I.File_Name;
         begin
            if Verbosity > 1 then
               Put_Line ("./" & I.File_Name); -- match 'find' output
            end if;

            if Ada.Directories.Extension (File_Name) /= "mp3" then
               Failed := Failed + 1;
               Put_Line ("db extension:" & Integer'Image (I.ID) & " '" & Ada.Directories.Extension (File_Name) & "'");
            end if;

            if not Ada.Directories.Exists (File_Name) then
               Failed := Failed + 1;
               Put_Line ("db extra:" & Integer'Image (I.ID) & " '" & File_Name & "'");
            end if;
         end;

         if I.Play_After_Is_Present then
            declare
               After_ID  : constant Integer := I.ID;
               Before_ID : constant Integer := I.Play_After;
               Before_J  : constant Cursor  := Find_ID (DB, Before_ID);
            begin
               if not Before_J.Has_Element then
                  Failed := Failed + 1;
                  Put_Line ("db Play_After bad link:" & Integer'Image (After_ID));

               elsif Before_J.Play_Before_Is_Present then
                  if After_ID /= Before_J.Play_Before then
                     Failed := Failed + 1;
                     Put_Line
                       ("db mismatch" & Integer'Image (I.ID) & " Play_Before:" & Integer'Image (Before_ID) &
                          "; Play_After" & Integer'Image (After_ID));
                  end if;
               else
                  Failed := Failed + 1;
                  Put_Line
                    ("db missing Play_Before:" & Integer'Image (Before_ID) &
                       "; Play_After" & Integer'Image (After_ID));
               end if;
            end;
         end if;

         if I.Play_Before_Is_Present then
            declare
               Before_ID : constant Integer := I.ID;
               After_ID  : constant Integer := I.Play_Before;
               After_J   : constant Cursor  := Find_ID (DB, After_ID);
            begin
               if not After_J.Has_Element then
                  Failed := Failed + 1;
                  Put_Line ("db Play_Before bad link:" & Integer'Image (I.ID));

               elsif After_J.Play_After_Is_Present then
                  if Before_ID /= After_J.Play_After then
                     Failed := Failed + 1;
                     Put_Line
                       ("db mismatch" & Integer'Image (I.ID) & " Play_Before:" & Integer'Image (Before_ID) &
                          "; Play_After" & Integer'Image (After_J.Play_After));
                  end if;
               else
                  Failed := Failed + 1;
                  Put_Line
                    ("db missing Play_After:" & Integer'Image (After_J.ID) &
                       "; Play_Before" & Integer'Image (I.ID));
               end if;
            end;
         end if;

         Next (I);
      end loop;
   end Check_Before_After_Exists;

   procedure Check_Dir (Dir : in String)
   is
      use Ada.Directories;

      Found_Mp3           : Boolean := False;
      Found_Liner_Notes   : Boolean := False;
      Found_AlbumArt_Huge : Boolean := False;

      procedure Process_Dir_Entry (Dir_Entry : in Directory_Entry_Type)
      is
         use Ada.Strings.Fixed;
         use SMM.Database;

         File_Name : constant String := Relative_Name (Source_Root, Normalize (Full_Name (Dir_Entry)));
      begin
         case Kind (Dir_Entry) is
         when Directory =>
            if Simple_Name (Dir_Entry) = "." or
              Simple_Name (Dir_Entry) = ".."
            then
               return;
            end if;

            Check_Dir (File_Name);

         when Ordinary_File =>
            if Extension (File_Name) = "mp3" then
               Disk_Count := Disk_Count + 1;
               Found_Mp3  := True;

               declare
                  I : constant Cursor := DB.Find_File_Name (File_Name);
               begin
                  if I.Has_Element then
                     for J in Required_Fields loop
                        if I.Field (J)'Length = 0 then
                           Put_Line ("db missing " & (-Field_Image (J)) & ": " & File_Name);
                        end if;
                     end loop;
                  else
                     Put_Line ("db missing file: " & File_Name);
                  end if;
               end;
            elsif 0 < Index (File_Name, "liner_notes.pdf") then
               Found_Liner_Notes := True;

            elsif 0 < Index (File_Name, "AlbumArt_huge") then
               Found_AlbumArt_Huge := True;
            end if;

         when Special_File =>
            raise SAL.Programmer_Error with "found special file";
         end case;
      end Process_Dir_Entry;
   begin
      if Verbosity > 0 then
         Put_Line ("checking directory " & Dir);
      end if;

      Search
        (Dir,
         Pattern          => "*",
         Filter           =>
           (Ordinary_File => True,
            Directory     => True,
            Special_File  => False),
         Process          => Process_Dir_Entry'Access);

      if Found_Mp3 then
         if not Found_Liner_Notes then
            Put_Line ("liner_notes missing  : " & Dir);
         end if;
         if not Found_AlbumArt_Huge then
            Put_Line ("AlbumArt_huge missing: " & Dir);
         end if;
      end if;
   end Check_Dir;

begin
   Check_Before_After_Exists;

   Check_Dir (Source_Root);

   Put_Line ("db files:" & Integer'Image (DB_Count));
   Put_Line ("Disk files:" & Integer'Image (Disk_Count));
   Put_Line ("failed checks:" & Integer'Image (Failed));

   if Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end SMM.Check;
