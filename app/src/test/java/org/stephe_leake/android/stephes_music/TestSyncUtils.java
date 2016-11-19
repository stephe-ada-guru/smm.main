//  Abstract :
//
//  Test functions in SyncUtils
//
//  Copyright (C) 2016 Stephen Leake. All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or
//  modify it under terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 3, or (at
//  your option) any later version. This program is distributed in the
//  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
//  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
//  PURPOSE. See the GNU General Public License for more details. You
//  should have received a copy of the GNU General Public License
//  distributed with this program; see file COPYING. If not, write to
//  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
//  MA 02110-1335, USA.

package org.stephe_leake.android.stephes_music;

import org.junit.Test;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.LineNumberReader;
import org.apache.commons.io.FileUtils;

import static org.junit.Assert.assertTrue;

public class TestSyncUtils
{
  private void cleanup()
  {
     try
     {
        FileUtils.deleteDirectory(new File("tmp"));
     }
     catch (java.io.IOException e)
     {
        // Probably directory not created yet.
     }
  }

   @Test
   public void editPlaylistNominal()
   {
      //  Create the test environment; a playlist, a .last
      String     playlistFilename = "tmp/playlists/Vocal.m3u";
      String     lastFilename     = "tmp/smm/Vocal.last";
      FileWriter output;

      cleanup();

      try
      {
         assertTrue("mkdir tmp", new File("tmp").mkdir());
         assertTrue("mkdir playlists", new File("tmp/playlists/").mkdir());
         output = new FileWriter(playlistFilename);

         output.write("Vocal/artist_1/file_4.mp3" + "\n");
         output.write("Vocal/artist_1/file_5.mp3" + "\n");
         output.write("Vocal/artist_2/file_6.mp3" + "\n");
         output.close();

         assertTrue("mkdir smm", new File("tmp/smm/").mkdir());
         output = new FileWriter(lastFilename);
         output.write("Vocal/artist_1/file_5.mp3" + "\n");
         output.close();

         SyncUtils.editPlaylist(playlistFilename, lastFilename);

         {
            LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

            assertTrue(input.readLine().equals("Vocal/artist_2/file_6.mp3"));
            assertTrue(input.readLine() == null);
         }
      }
      catch (java.io.IOException e){assertTrue(e.toString(), false);}
   }
}

// end of file
