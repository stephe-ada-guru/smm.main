--  Abstract :
--
--  We need a non-local object to reference in smm-db_sync_server
--  calls to Diff.
--
--  Copyright (C) 2025 Stephen Leake All Rights Reserved.
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

with SMM.Database;
with SMM.Database_Remote.Disk;
package SMM.DB_Sync is
   Disk_DB  : aliased SMM.Database.Database;
   Local_DB : aliased SMM.Database_Remote.Disk.Database (Disk_DB'Access);
end SMM.DB_Sync;
