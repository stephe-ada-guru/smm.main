//  Abstract :
//
//  Test functions in DownloadUtils
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
import java.net.URISyntaxException;
import java.lang.Process;
import java.lang.ProcessBuilder;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.junit.FixMethodOrder;
import org.junit.runners.MethodSorters;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

// We need to fix the order of running unit tests, since the last
// one leaves some files locked that prevents the first one from
// running. Test names are prefixed with a sort letter.
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TestDownloadUtils
{
   final String serverIP = "192.168.1.83"; // must match smm-server.config

   // to start server externally for debugging, run this test with -DstartServer=false
   final boolean startServer = new String("true").equals(System.getProperty("startServer", "true"));

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

   ////////// Tests

   @Test
   public void a_editPlaylistNominal()
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

         DownloadUtils.editPlaylist(null, playlistFilename, lastFilename);

         {
            LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

            assertTrue(input.readLine().equals("vocal/artist_2/file_6.mp3"));
            assertTrue(input.readLine() == null);
         }
      }
      catch (java.io.IOException e){assertTrue(e.toString(), false);}
   }

   @Test
   public void b_editPlaylistNoLast()
   {
      //  Create the test environment; a playlist, no .last
      String     playlistFilename = "tmp/playlists/vocal.m3u";
      String     lastFilename     = "tmp/smm/vocal.last";
      FileWriter output;

      cleanup(true);

      try
      {
         output = new FileWriter(playlistFilename);

         output.write("vocal/artist_1/file_4.mp3" + "\n");
         output.write("vocal/artist_1/file_5.mp3" + "\n");
         output.write("vocal/artist_2/file_6.mp3" + "\n");
         output.close();

         assertTrue("mkdir smm", new File("tmp/smm/").mkdir());

         DownloadUtils.editPlaylist(null, playlistFilename, lastFilename);

         {
            LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

            assertTrue(input.readLine().equals("vocal/artist_1/file_4.mp3"));
            assertTrue(input.readLine().equals("vocal/artist_1/file_5.mp3"));
            assertTrue(input.readLine().equals("vocal/artist_2/file_6.mp3"));
            assertTrue(input.readLine() == null);
         }
      }
      catch (java.io.IOException e){assertTrue(e.toString(), false);}
   }

   @Test
   public void c_firstPassNominal()
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

         DownloadUtils.firstPass
            (null,
             "vocal",
             (new File("tmp/playlists/")).getAbsolutePath(),
             (new File("tmp/smm/")).getAbsolutePath());

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
   public void d_getNewSongsNominal()
   {
      // Translated from smm test_server.adb Set_Up_Case, Test_Playlist, Test_Meta, Test_Get_File
      final String dbFilename       = "tmp/smm.db";
      final String playlistFilename = "tmp/playlists/vocal.m3u";
      final String category         = "vocal";
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
         FileUtils.forceMkdir(new File("tmp/source/artist_1/album_1"));

         createTestFile("tmp/source/artist_1/album_1/1 - song_1.mp3");
         createTestFile("tmp/source/artist_1/album_1/2 - song_2.mp3");
         createTestFile("tmp/source/artist_1/album_1/3 - song_3.mp3");
         createTestFile("tmp/source/artist_1/album_1/AlbumArt_1.jpg");
         createTestFile("tmp/source/artist_1/album_1/liner_notes.pdf");

         FileUtils.forceMkdir(new File("tmp/source/artist_2/album_1"));
         createTestFile("tmp/source/artist_2/album_1/1 - song_1.mp3");
         createTestFile("tmp/source/artist_2/album_1/2 - song_2.mp3");
         createTestFile("tmp/source/artist_2/album_1/3 - song_3.mp3");

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

         if (startServer)
            server = new ProcessBuilder("smm-server_driver", "smm-server.config", "1").start();

         newSongs = DownloadUtils.getNewSongsList(null, serverIP, category, 5, 0);

         check(newSongs, expectedSongs);

         DownloadUtils.getSongs(null, serverIP, newSongs, category, "tmp/playlists", null);

         // Check that the new song files and meta files are present
         checkExists("tmp/playlists/vocal/artist_1/album_1/1 - song_1.mp3", true);
         checkExists("tmp/playlists/vocal/artist_1/album_1/2 - song_2.mp3", true);
         checkExists("tmp/playlists/vocal/artist_1/album_1/AlbumArt_1.jpg", true);
         checkExists("tmp/playlists/vocal/artist_1/album_1/liner_notes.pdf", true);
         checkExists("tmp/playlists/vocal/artist_2/album_1/1 - song_1.mp3", true);
         checkExists("tmp/playlists/vocal/artist_2/album_1/2 - song_2.mp3", true);
         checkExists("tmp/playlists/vocal/artist_2/album_1/3 - song_3.mp3", true);

         // Check that new songs are added to playlist
         {
            LineNumberReader input = new LineNumberReader(new FileReader(playlistFilename));

            assertTrue(input.readLine().equals("vocal/artist_3/file_4.mp3")); // previously there
            assertTrue(input.readLine().equals("vocal/artist_2/album_1/1 - song_1.mp3"));
            assertTrue(input.readLine().equals("vocal/artist_1/album_1/1 - song_1.mp3"));
            assertTrue(input.readLine().equals("vocal/artist_2/album_1/2 - song_2.mp3"));
            assertTrue(input.readLine().equals("vocal/artist_1/album_1/2 - song_2.mp3"));
            assertTrue(input.readLine().equals("vocal/artist_2/album_1/3 - song_3.mp3"));
            assertTrue(input.readLine() == null);
         }
      }
      catch (IOException e) {assertTrue(e.getMessage(), false);}
      finally
      {
         if (null != server) server.destroy();
      }
   }

   @Test
   public void e_sendNotesNominal()
   {
      final String localNotesFilename  = "tmp/smm/vocal.note";
      final String serverNotesFilename = "tmp/source/remote_cache/vocal.note";

      File    smmDir = new File("tmp/smm/");
      Process server = null;

      try
      {
         FileWriter   output;

         FileUtils.forceMkdir(smmDir);
         output = new FileWriter(localNotesFilename);
         output.write("\"vocal/artist_1/file_5.mp3\" Category\n");
         output.write("\"vocal/artist_1/file_6.mp3\" Don\'t Play\n");
         output.close();

         if (startServer)
            server = new ProcessBuilder("smm-server_driver", "smm-server.config", "1").start();

         DownloadUtils.sendNotes(null, serverIP, "vocal", smmDir.getAbsolutePath());

         checkExists(serverNotesFilename, true);
         {
            LineNumberReader input = new LineNumberReader(new FileReader(serverNotesFilename));

            assertTrue(input.readLine().equals("\"vocal/artist_1/file_5.mp3\" Category"));
            assertTrue(input.readLine().equals("\"vocal/artist_1/file_6.mp3\" Don\'t Play"));
            assertTrue(input.readLine() == null);
         }
      }
      catch (java.io.IOException e){assertTrue(e.toString(), false);}
      finally
      {
         if (null != server) server.destroy();
      }
   }
}
// end of file
