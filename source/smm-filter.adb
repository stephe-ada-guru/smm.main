with Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;
with SAL.Config_Files; use SAL.Config_Files;
with SAL.Time_Conversions.Config;
procedure SMM.Filter
is
   Db_File_Name : constant String := "/Stephe/.smm/smm.db";

   Db        : Configuration_Type;
   I         : Iterator_Type;
   To_Delete : Iterator_Type;

   use String_Maps;

   Song_Files : String_Maps.Map;
   Pos        : Cursor;
   Inserted   : Boolean;
begin
   Open (Db, Db_File_Name, Read_Only => False);

   I := First (Db, Songs_Key);

   loop
      exit when Is_Null (I);
      declare
         use Ada.Directories;
         Root      : constant String := As_Directory (Read (Db, Root_Key));
         Song_File : constant String := Normalize (Read (Db, I, File_Key));
      begin
         --  Ada.Directories.Exists rejects some file names that
         --  Windows accepts. GNAT.OS_Lib isn't any better. So filter
         --  them, fix the names later, and re-import.

         if not Exists (Root & Song_File) then
            Put_Line ("does not exist: " & Song_File);
            To_Delete := I;

         elsif Extension (Song_File) /= "mp3" then
            Put_Line ("invalid extension: " & Song_File);
            To_Delete := I;

         else
            Insert (Song_Files, Song_File, I, Pos, Inserted);

            if not Inserted then
               Put_Line ("duplicate: " & Song_File);
               --  Pos has same name as I
               declare
                  use SAL.Time_Conversions;

                  Old_I : constant Iterator_Type := Element (Pos);

                  Last_Downloaded : constant Time_Type := Config.Read (Db, I, Last_Downloaded_Key);
               begin
                  Write
                    (Db, Old_I, Last_Downloaded_Key,
                     Time_Type'Image (Time_Type'Max (Last_Downloaded, Config.Read (Db, Old_I, Last_Downloaded_Key))));

                  if not Is_Present (Db, Old_I, Category_Key) then
                     if Is_Present (Db, I, Category_Key) then
                        Write
                          (Db, Old_I, Category_Key,
                           Read (Db, I, Category_Key, Default => "vocal", Missing_Key => Ignore));
                     end if;
                  end if;
               end;
               To_Delete := I;
            end if;
         end if;
      exception
      when Ada.Directories.Name_Error =>
         --  From Exists
         Put_Line ("invalid name: " & Song_File);
         To_Delete := I;
      end;

      Next (I);

      if not Is_Null (To_Delete) then
         Delete (Db, To_Delete);
      end if;
   end loop;

   Close (Db);
end SMM.Filter;
