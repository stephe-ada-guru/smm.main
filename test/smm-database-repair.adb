--  Abstract :
--
--  repair bogus default times

procedure SMM.Database.Repair
is
   Bogus_Default : constant Time_String := "1958-01-01 06:00:00";
   DB_File_Name  : constant String      := "c:/home/stephe/smm/smm.db";
   DB            : SMM.Database.Database;

   procedure Repair_Prev (I : in Cursor)
   is
      use GNATCOLL.SQL.Exec;

      Statement : constant String := "update Song set prev_downloaded = ? where id = ?";
      Params : constant SQL_Parameters (1 .. 2) := (+Default_Time_String, +I.ID);
   begin
      Checked_Execute (DB, Statement, Params);
   end Repair_Prev;

begin
   DB.Open (DB_File_Name);

   declare
      I : Cursor := First (DB);
   begin
      loop
         exit when not Has_Element (I);
         if I.Prev_Downloaded = Bogus_Default then
            Repair_Prev (I);
         end if;
         Next (I);
      end loop;
   end;
end SMM.Database.Repair;
