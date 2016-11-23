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

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import org.apache.commons.io.FileUtils;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class TestSyncUtils
{
   private void cleanup(boolean ignoreError)
   {
      File tmp = new File("tmp");

      if (tmp.exists())
         try
         {
            FileUtils.deleteDirectory(tmp);
         }
         catch (java.io.IOException e)
         {
            // deletion failed
            if (!ignoreError)
               assertTrue("cleanup failed; some file is locked", false);
         }
   }

   private void createTestFile(String filename)
   {
      // Assumes directory exists

      try
      {
         FileWriter output = new FileWriter(filename);
         output.write("body" + filename + "\n");
         output.close();
      }
      catch (IOException e)
      {
         assertTrue(e.toString(), false);
      }
   }

   private void checkExists(String filename, Boolean expected)
   {
      assertTrue(filename + " exists = " + expected.toString(), expected == new File(filename).exists());
   }

   @Test
   public void editPlaylistNominal()
   {
      //  Create the test environment; a playlist, a .last
      String     playlistFilename = "tmp/playlists/Vocal.m3u";
      String     lastFilename     = "tmp/smm/Vocal.last";
      FileWriter output;

      cleanup(false);

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

   @Test
   public void firstPassNominal()
   {
      // Translated from smm test_first_pass_with_last.adb Nominal

      //  Create the test environment; a playlist, a .last, *.mp3, *.jpg, *.pdf files
      String     playlistFilename = "tmp/playlists/Vocal.m3u";
      String     lastFilename     = "tmp/smm/Vocal.last";
      FileWriter output;

      // Can't delete Vocal.m3u; not clear why it's locked.
      cleanup(true);

      try
      {
         FileUtils.forceMkdir(new File("tmp/playlists/Vocal/artist_1"));
         createTestFile("tmp/playlists/Vocal/artist_1/file_4.mp3");
         createTestFile("tmp/playlists/Vocal/artist_1/file_5.mp3");

         FileUtils.forceMkdir(new File("tmp/playlists/Vocal/artist_2"));
         createTestFile("tmp/playlists/Vocal/artist_2/file_6.mp3");

         output = new FileWriter(playlistFilename);

         output.write("Vocal/artist_1/file_4.mp3" + "\n");
         output.write("Vocal/artist_1/file_5.mp3" + "\n");
         output.write("Vocal/artist_2/file_6.mp3" + "\n");
         output.close();

         assertTrue("mkdir smm", new File("tmp/smm/").mkdir());
         output = new FileWriter(lastFilename);
         output.write("Vocal/artist_1/file_5.mp3" + "\n");
         output.close();

         SyncUtils.firstPass("Vocal", (new File("tmp\\playlists\\")).getAbsolutePath(), (new File("tmp\\smm\\")).getAbsolutePath());

         // Check that the played files are deleted, but the others are not.
         checkExists("tmp/playlists/Vocal/artist_1/file_4.mp3", false);
         checkExists("tmp/playlists/Vocal/artist_1/file_5.mp3", false);
         checkExists("tmp/playlists/Vocal/artist_1/", false);
         checkExists("tmp/playlists/Vocal/artist_2/file_6.mp3", true);

         // Check that the playlist is updated
         {
            LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

            assertTrue(input.readLine().equals("Vocal/artist_2/file_6.mp3"));
            assertTrue(input.readLine() == null);
         }

         // Check that .last file is deleted
         checkExists("tmp/playlists/Vocal.last", false);

      }
      catch (java.io.IOException e){assertTrue(e.toString(), false);}
   }
}

// end of file
