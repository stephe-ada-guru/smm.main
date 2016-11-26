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
import java.lang.Process;
import java.lang.ProcessBuilder;
import java.util.List;
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

   private void check(String[] computed, String[] expected)
   {
      assertTrue("computed.length " + computed.length + "; expected.length " + expected.length,
                 computed.length == expected.length);
      for(int i = 0; i < computed.length; i++)
         assertTrue(Integer.toString(i), computed[i].equals(expected[i]));
   }

   @Test
   public void editPlaylistNominal()
   {
      //  Create the test environment; a playlist, a .last
      String     playlistFilename = "tmp/playlists/vocal.m3u";
      String     lastFilename     = "tmp/smm/vocal.last";
      FileWriter output;

      cleanup(false);

      try
      {
         assertTrue("mkdir tmp", new File("tmp").mkdir());
         assertTrue("mkdir playlists", new File("tmp/playlists/").mkdir());
         output = new FileWriter(playlistFilename);

         output.write("vocal/artist_1/file_4.mp3" + "\n");
         output.write("vocal/artist_1/file_5.mp3" + "\n");
         output.write("vocal/artist_2/file_6.mp3" + "\n");
         output.close();

         assertTrue("mkdir smm", new File("tmp/smm/").mkdir());
         output = new FileWriter(lastFilename);
         output.write("vocal/artist_1/file_5.mp3" + "\n");
         output.close();

         SyncUtils.editPlaylist(playlistFilename, lastFilename);

         {
            LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

            assertTrue(input.readLine().equals("vocal/artist_2/file_6.mp3"));
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
      String     playlistFilename = "tmp/playlists/vocal.m3u";
      String     lastFilename     = "tmp/smm/vocal.last";
      FileWriter output;

      // Can't delete vocal.m3u; not clear why it's locked.
      cleanup(true);

      try
      {
         FileUtils.forceMkdir(new File("tmp/playlists/vocal/artist_1"));
         createTestFile("tmp/playlists/vocal/artist_1/file_4.mp3");
         createTestFile("tmp/playlists/vocal/artist_1/file_5.mp3");

         FileUtils.forceMkdir(new File("tmp/playlists/vocal/artist_2"));
         createTestFile("tmp/playlists/vocal/artist_2/file_6.mp3");

         output = new FileWriter(playlistFilename);

         output.write("vocal/artist_1/file_4.mp3" + "\n");
         output.write("vocal/artist_1/file_5.mp3" + "\n");
         output.write("vocal/artist_2/file_6.mp3" + "\n");
         output.close();

         assertTrue("mkdir smm", new File("tmp/smm/").mkdir());
         output = new FileWriter(lastFilename);
         output.write("vocal/artist_1/file_5.mp3" + "\n");
         output.close();

         SyncUtils.firstPass("vocal", (new File("tmp/playlists/")).getAbsolutePath(), (new File("tmp/smm/")).getAbsolutePath());

         // Check that the played files are deleted, but the others are not.
         checkExists("tmp/playlists/vocal/artist_1/file_4.mp3", false);
         checkExists("tmp/playlists/vocal/artist_1/file_5.mp3", false);
         checkExists("tmp/playlists/vocal/artist_1/", false);
         checkExists("tmp/playlists/vocal/artist_2/file_6.mp3", true);

         // Check that the playlist is updated
         {
            LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

            assertTrue(input.readLine().equals("vocal/artist_2/file_6.mp3"));
            assertTrue(input.readLine() == null);
         }

         // Check that .last file is deleted
         checkExists("tmp/playlists/vocal.last", false);

      }
      catch (java.io.IOException e){assertTrue(e.toString(), false);}
   }

   @Test
   public void getNewSongsNominal()
   {
      // Translated from smm test_server.adb Set_Up_Case, Test_Playlist, Test_Meta, Test_Get_File
      final String serverIP         = "192.168.1.83"; // must match smm-server.config
      final String dbFilename       = "tmp/smm.db";
      final String playlistFilename = "tmp/playlists/vocal.m3u";
      FileWriter   output;
      Process      server           = null;
      String[]     newSongs;
      String[]     expectedSongs    =
         {"artist_2/album_1/1 - song_1.mp3",
          "artist_1/album_1/1 - song_1.mp3",
          "artist_2/album_1/2 - song_2.mp3",
          "artist_1/album_1/2 - song_2.mp3",
          "artist_2/album_1/3 - song_3.mp3"};

      // Can't delete vocal.m3u; not clear why it's locked.
      cleanup(true);

      try
      {
         FileUtils.forceMkdir(new File("tmp"));
         output = new FileWriter(dbFilename);
         output.write("Root = " + new File("tmp/source").getAbsolutePath() + "\n");
         output.write("Songs. 1.File = artist_1/album_1/1 - song_1.mp3" + "\n");
         output.write("Songs. 1.Category = vocal" + "\n");
         output.write("Songs. 2.File = artist_1/album_1/2 - song_2.mp3" + "\n");
         output.write("Songs. 2.Category = vocal" + "\n");
         output.write("Songs. 3.File = artist_1/album_1/3 - song_3.mp3" + "\n");
         output.write("Songs. 3.Category = instrumental" + "\n");
         output.write("Songs. 4.File = artist_2/album_1/1 - song_1.mp3" + "\n");
         output.write("Songs. 4.Category = vocal" + "\n");
         output.write("Songs. 5.File = artist_2/album_1/2 - song_2.mp3" + "\n");
         output.write("Songs. 5.Category = vocal" + "\n");
         output.write("Songs. 6.File = artist_2/album_1/3 - song_3.mp3" + "\n");
         output.write("Songs. 6.Category = vocal" + "\n");
         output.close();

         FileUtils.forceMkdir(new File("tmp/playlists/vocal/artist_3"));
         createTestFile("tmp/playlists/vocal/artist_3/file_4.mp3");

         output = new FileWriter(playlistFilename);
         output.write("vocal/artist_3/file_4.mp3" + "\n");
         output.close();

         // Spawn smm server
         server = new ProcessBuilder("smm-server_driver", "smm-server.config").start();

         newSongs = SyncUtils.getNewSongsList(serverIP, "vocal", 5, 0);

         check(newSongs, expectedSongs);

         // FIXME: later
         // SyncUtils.getSongs(newSongs, playlistFilename);

         // // Check that the new song files and meta files are present
         // checkExists("tmp/playlists/vocal/artist_1/album_1/1 - song_1.mp3", true);
         // checkExists("tmp/playlists/vocal/artist_1/album_1/2 - song_2.mp3", true);
         // checkExists("tmp/playlists/vocal/artist_1/album_1/Album_Art_1.jpg", true);
         // checkExists("tmp/playlists/vocal/artist_1/album_1/liner_notes.pdf", true);
         // checkExists("tmp/playlists/vocal/artist_2/album_1/1 - song_1.mp3", true);
         // checkExists("tmp/playlists/vocal/artist_2/album_1/2 - song_2.mp3", true);
         // checkExists("tmp/playlists/vocal/artist_2/album_1/3 - song_3.mp3", true);

         // // Check that new songs are added to playlist
         // {
         //    LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

         //    assertTrue(input.readLine().equals("vocal/artist_3/file_4.mp3")); // previously there
         //    assertTrue(input.readLine().equals("vocal/artist_2/album_1/1 - song_1.mp3"));
         //    assertTrue(input.readLine().equals("vocal/artist_1/album_1/1 - song_1.mp3"));
         //    assertTrue(input.readLine().equals("vocal/artist_2/album_1/2 - song_2.mp3"));
         //    assertTrue(input.readLine().equals("vocal/artist_1/album_1/2 - song_2.mp3"));
         //    assertTrue(input.readLine().equals("vocal/artist_2/album_1/3 - song_3.mp3"));
         //    assertTrue(input.readLine() == null);
         // }
      }
      catch (IOException e) {assertTrue(e.getMessage(), false);}
      finally
      {
         if (null != server) server.destroy();
      }
   }
}
// end of file
