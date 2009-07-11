--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004, 2009 Stephen Leake.  All Rights Reserved.
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
--

with AUnit.Test_Cases.Registration;
with Books.Table_Views.Author.Test;
with Books.Table_Views.Test;
with Books.Table_Views.Title.Test;
with Gdk.Test_Events;
with Test_Books.GUI_Utils;
with Test_Books.String_Lists;
package body Test_Books.Nominal is

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Books.Table_Views.Test;
      use GUI_Utils;
      use Gdk.Test_Events;
      use Test_Books.String_Lists;
   begin
      --  Add two authors and two books. Add author/title links from
      --  author and title views (not in Add view).
      Add_Author
        (Last   => "Clarke",
         Middle => "C.",
         First  => "Arthur");

      Add_Author
        (Last   => "Asimov",
         Middle => "",
         First  => "Isaac");

      Add_Title
        (Title   => "2001",
         Year    => "1970",
         Comment => "Obelisk",
         Rating  => "9");

      Add_Title
        (Title   => "Foundation",
         Year    => "1960",
         Comment => "Hari Seldon",
         Rating  => "9");

      --  authors
      --  id    last
      --  1     clarke
      --  2     asimov
      --
      --  titles
      --  id    title
      --  1     2001
      --  2     foundation

      --  Search for first author of Foundation
      Find_Author ("clarke");

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Author.Test.Dump_Author'Access);
      Alt_Key_Stroke ('t'); -- test

      Check ("author 1", Books.Table_Views.Author.Test.Author_Contents, (+"Arthur", +"C.", +"Clarke"));

      --  add link in author view
      Mouse_Move (Add_Link_Button (Main_Window.Author_View));
      Mouse_Click;

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check ("links 1", Books.Table_Views.Test.Clist_Contents, +(+"00002", +"Foundation", +"1960"));

      --  Search for second author of Foundation
      Find_Author ("asimov");

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Author.Test.Dump_Author'Access);
      Alt_Key_Stroke ('t'); -- test

      Check ("author 2", Books.Table_Views.Author.Test.Author_Contents, (+"Isaac", +"", +"Asimov"));

      --  add link in author view
      Mouse_Move (Add_Link_Button (Main_Window.Author_View));
      Mouse_Click;

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check ("links 1", Books.Table_Views.Test.Clist_Contents, +(+"00002", +"Foundation", +"1960"));

      --  add authors of 2001 in title view
      Find_Title ("2001");

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Title.Test.Dump_Title'Access);
      Alt_Key_Stroke ('t'); -- test

      Check ("title 3", Books.Table_Views.Title.Test.Title_Contents, (+"2001", +"1970"));

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check
        ("links 3a",
         Books.Table_Views.Test.Clist_Contents,
         Null_String_Table);

      --  Add author asimov (currently showing in author)
      Mouse_Move (Add_Link_Button (Main_Window.Title_View));
      Mouse_Click;

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check ("links 3b", Books.Table_Views.Test.Clist_Contents, +(+"00002", +"Isaac", +"", +"Asimov"));

      --  Add author clark (need to search in author)
      Find_Author ("clark");

      Mouse_Move (Add_Link_Button (Main_Window.Title_View));
      Mouse_Click;

      Books.Table_Views.Test.Set_Test_Hook (Books.Table_Views.Test.Dump_Clist'Access);
      Alt_Key_Stroke ('t'); -- test

      Check
        ("links 4",
         Books.Table_Views.Test.Clist_Contents,
         +(+"00001", +"Arthur", +"C.", +"Clarke") +
           (+"00002", +"Isaac", +"", +"Asimov"));

   end Nominal;

   ----------
   --  Public bodies

   overriding function Name (T : Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Books.Nominal");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug_Level < 3 then
         Register_Routine (T, Nominal'Access, "Nominal");
      end if;
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      use Ada.Strings.Unbounded;
   begin
      GUI_Utils.Empty_Database;

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

end Test_Books.Nominal;
