--  Abstract :
--
--  See spec
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
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

with AUnit.Test_Cases.Registration;
with Books.Table_Views.Test;
with Gdk.Test_Events;
with Test_Books.GUI_Utils;
with Test_Books.String_Lists;
package body Test_Books.Show_Other is

   ----------
   --  Test procedures

   procedure Show_From_Title (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Gdk.Test_Events;
      use Test_Books.GUI_Utils;
      use Test_Books.String_Lists;
   begin
      Mouse_Move (Title_Origin + Find_Entry);
      Mouse_Double_Click;
      Key_Stroke ("2001");

      Mouse_Move (Title_Origin + Title_First_Link); -- Asimov
      Mouse_Double_Click;

      Mouse_Move (Author_Origin + Find_Entry);
      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check_List (Books.Table_Views.Test.Clist_Contents, +(+"00002", +"Isaac", +"", +"Asimov"));

      Mouse_Move (Title_Origin + Title_First_Link); -- clarke
      Mouse_Double_Click;

      Mouse_Move (Author_Origin + Find_Entry);
      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check_List (Books.Table_Views.Test.Clist_Contents, +(+"00001", +"Arthur", +"C.", +"Clarke"));
   end Show_From_Title;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Books.Show_Other");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug_Level < 2 then
         Register_Routine (T, Show_From_Title'Access, "Show_From_Title");
      end if;
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      GUI_Utils.Empty_Database;
      GUI_Utils.Create_Some_Data;

      GUI_Utils.Set_Up_Case (T.Config_File, T.Debug_Level);
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      if GUI_Utils.Background.Debug_Level < 2 then
         GUI_Utils.Background.Close (GUI_Utils.Main_Window);
      end if;
      GUI_Utils.Background.Background_Task.Wait_Shutdown;
   end Tear_Down_Case;

end Test_Books.Show_Other;
