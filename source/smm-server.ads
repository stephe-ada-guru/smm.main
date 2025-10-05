--  Abstract :
--
--  Stephe's Music Manager Server
--
--  Copyright (C) 2016, 2025 Stephen Leake All Rights Reserved.
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

with SAL.Web_Utils;
package SMM.Server is

   procedure Server;

   --  Visible for testing
   DB_Filename : constant String := "/var/www/html/music_server_data/smm.db";

   subtype API_Versions is Integer range 1 .. 2;
   --  1 - API not specified in GET. Client always downloads all songs. 'download' => list of filenames
   --  2 - API specified in GET. Client only downloads new songs (all others previously downloaded).
   --      'get_new_songs_list => list of "Album_Artist", "album", "title", "filename"

   function Handle_Get_New_Songs_List
     (Parameters : in SAL.Web_Utils.Parameter_Lists.Map;
      API        : in API_Versions)
     return String;

end SMM.Server;
