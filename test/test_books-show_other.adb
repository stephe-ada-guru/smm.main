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
with Books.Table_Views.Author.Test;
with Books.Table_Views.Test;
with Gdk.Test_Events;
with Test_Books.GUI_Utils;
with Test_Books.String_Lists;
package body Test_Books.Show_Other is

   ----------
   --  Test procedures

   procedure Show_Author_From_Title (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Books.Table_Views.Test;
      use Gdk.Test_Events;
      use Test_Books.GUI_Utils;
      use Test_Books.String_Lists;
   begin
      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Author.Test.Dump_Author'Access);

      Find_Title ("Foundation");

      Mouse_Move (Second_Link (Main_Window.Title_View)); -- Asimov
      Mouse_Double_Click;

      Mouse_Move (Find_Entry (Main_Window.Author_View));
      Mouse_Click;
      delay 0.1;
      Alt_Key_Stroke ('t'); -- test

      Check ("asimov", Books.Table_Views.Author.Test.Author_Contents, (+"Isaac", +"", +"Asimov"));

      Mouse_Move (First_Link (Main_Window.Title_View)); -- clarke
      Mouse_Double_Click;

      Mouse_Move (Find_Entry (Main_Window.Author_View));
      Mouse_Click;
      delay 0.1;
      Alt_Key_Stroke ('t'); -- test

      Check ("clarke", Books.Table_Views.Author.Test.Author_Contents, (+"Arthur", +"C.", +"Clarke"));
   end Show_Author_From_Title;

   procedure Show_Title_From_Collection (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Books.Table_Views.Test;
      use Gdk.Test_Events;
      use Test_Books.GUI_Utils;
      use Test_Books.String_Lists;
   begin
      Find_Collection ("Analog");

      Mouse_Move (Second_Link (Main_Window.Collection_View)); -- Foundation
      Mouse_Double_Click;

      Check_Title ("foundation", (+"Foundation", +"1960"));

      Mouse_Move (First_Link (Main_Window.Collection_View)); -- 2001
      Mouse_Double_Click;

      Check_Title ("2001", (+"2001", +"1970"));
   end Show_Title_From_Collection;

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
      case T.Debug_Level is
      when 0 =>
         Register_Routine (T, Show_Author_From_Title'Access, "Show_Author_From_Title");
         Register_Routine (T, Show_Title_From_Collection'Access, "Show_Title_From_Collection");
      when 1 =>
         Register_Routine (T, Show_Title_From_Collection'Access, "Show_Title_From_Collection");
      when 2 =>
         null;
      when others =>
         null;
      end case;
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
      if Gdk.Test_Events.Debug_Level < 2 then
         Gdk.Test_Events.Close (GUI_Utils.Main_Window);
      end if;
      GUI_Utils.Background.Background_Task.Wait_Shutdown;
   end Tear_Down_Case;

end Test_Books.Show_Other;
