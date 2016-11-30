//  Abstract :
//
//  Utilities for downloading from smm_server
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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.io.filefilter.FalseFileFilter;
import org.apache.commons.io.filefilter.SuffixFileFilter;
import org.apache.commons.io.filefilter.TrueFileFilter;

import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class DownloadUtils
{
   private static final int BUFFER_SIZE = 8 * 1024;

   private static String       playlistDir;
   private static List<String> mentionedFiles;
   private static OkHttpClient httpClient = null;

   private static List<String> readPlaylist(String playlistFilename, boolean lowercase)
      throws IOException
   {
      // Read playlist file, return list of files (lowercase) in it.
      File         playlistFile = new File(playlistFilename);
      LineIterator i;

      LinkedList<String> result = new LinkedList<>();

      for (i = FileUtils.lineIterator(playlistFile); i.hasNext();)
      {
         String line = i.next();
         result.addLast(lowercase ? line.toLowerCase() : line);
      }
      i.close();
      return result;
   }

   public static void editPlaylist(String playlistFilename, String lastFilename)
      throws IOException
   // Delete lines from start of playlist file up to and including
   // line in last file. Delete last file.
   {
      List<String> lines = readPlaylist(playlistFilename, false);

      try
      {
         FileWriter       output     = new FileWriter(playlistFilename);
         LineNumberReader input      = new LineNumberReader(new FileReader(lastFilename));
         String           lastPlayed = input.readLine();
         boolean          found      = false;

         input.close();

         if (null == lastPlayed)
         {
            // last file is empty; nothing to delete
            output.close();
            new File(lastFilename).delete();
            return;
         }

         for (String line : lines)
         {
            if (found)
               output.write(line + "\n");
            else
               found = line.equals(lastPlayed);
         }
         output.close();

         new File(lastFilename).delete();
      }
      catch (FileNotFoundException e)
      { // last file not found; same as empty; do nothing
      }
   }

   private static String normalize(String item)
   {
      char[] temp = item.toCharArray();

      for (int i = 0; i < temp.length; i++)
         if (temp[i] == '\\')
            temp[i] = '/';

      return new String(temp);
   }

   private static String relativeName(String root, String full)
   {
      if (full.startsWith(root))
         return normalize(full.substring(root.length() + 1, full.length()));
      else
         return "";
   }

   private static void processDirEntry(File entry)
      throws IOException
   {
      int entryCount = 0;

      if (entry.isDirectory())
      {
         // listFiles returns "." as the dir name; does not return ".."; cannot filter it!

         // Delete music files not in playlist
         for (File subDir : FileUtils.listFiles(entry, new SuffixFileFilter(".mp3"), FalseFileFilter.FALSE))
            processDirEntry(subDir);

         // Recurse into directories
        for (File subDir : FileUtils.listFilesAndDirs(entry, FalseFileFilter.FALSE, TrueFileFilter.TRUE))
           if (subDir != entry)
              processDirEntry(subDir);

         // Delete dir if there's no music or subdirectories left
         //
         // Count directories
         for (File subDir : FileUtils.listFilesAndDirs(entry, FalseFileFilter.FALSE, TrueFileFilter.TRUE))
            if (subDir != entry)
               entryCount++;

         if (entryCount > 0) return;

         // Count music files
         for (File subDir : FileUtils.listFiles(entry, new SuffixFileFilter(".mp3"), FalseFileFilter.FALSE))
            entryCount++;

         if (0 == entryCount)
            FileUtils.deleteDirectory(entry);
      }
      else if (entry.isFile())
      {
         final String name = relativeName(playlistDir, entry.getAbsolutePath()).toLowerCase();

         if (!mentionedFiles.contains(name))
            entry.delete();
      }
      // else special file; ignored
   }

   public static void firstPass(String category, String playlistDir, String smmDir)
      throws IOException
   {
      //  Delete lines in category.m3u that are before song in
      //  SMM_Dir/category.last. Delete files from Playlist_Dir/Category
      //  that are not mentioned in playlist file category.m3u. Return count
      //  of files remaining in playlist.
      //
      //  Directory names end in '/'

      // We can't declare a File object for playlistFile or lastFile;
      // that prevents rename, delete in editPlaylist.
      final String playlistFilename = FilenameUtils.concat(playlistDir, category + ".m3u");
      final String lastFilename     = FilenameUtils.concat(smmDir, category + ".last");

      DownloadUtils.playlistDir = playlistDir;

      // getPath() returns empty string if file does not exist
      if ("" != FilenameUtils.getPath(playlistFilename))
         if ("" != FilenameUtils.getPath(lastFilename))
            editPlaylist(playlistFilename, lastFilename);

      mentionedFiles = readPlaylist(playlistFilename, true);

      //  Search playlist directory, delete files not in playlist
      {
         File targetDir = new File(playlistDir, category);

         for (File subDir : FileUtils.listFilesAndDirs(targetDir, FalseFileFilter.FALSE, TrueFileFilter.TRUE))
            if (subDir != targetDir)
                processDirEntry(subDir);
      }
   }

   public static String[] getNewSongsList(String serverIP, String category, int count, int randomSeed)
      throws IOException
   // randomSeed = -1 means randomize; other values used in unit tests.
   {
      final String url = "http://" + serverIP + ":8080/download?category=" + category +
         "&count=" + Integer.toString(count) + (-1 == randomSeed ? "" : "&seed=" + Integer.toString(randomSeed));

      Request request = new Request.Builder().url(url).build();

      if (null == httpClient) httpClient = new OkHttpClient();

      try (Response response = httpClient.newCall(request).execute())
      {
         return response.body().string().split("\r\n");
      }
   }

   private static void getFile(String serverIP, String resource, File fileName)
      throws IOException, URISyntaxException
   {
      // Get 'resource' from 'serverIP', store in 'fileName'.
      // 'resource' may have only path and file name.
      //
      // We need to encode spaces but not path separators.
      // addPathSegment encodes both, so we split out the path
      // segments first.
      final String[] pathSegments = resource.split("/");

      HttpUrl.Builder url = new HttpUrl.Builder()
         .scheme("http")
         .host(serverIP)
         .port(8080);

      for (String segment : pathSegments)
         url.addPathSegment(segment);

      fileName.createNewFile();

      try (Response response = httpClient.newCall(new Request.Builder().url(url.build()).build()).execute())
      {
         if (!response.isSuccessful())
            throw new IOException(response.code() + " " + response.message() + " " + url.toString());

         BufferedInputStream in            = new BufferedInputStream(response.body().byteStream());
         FileOutputStream    out           = new FileOutputStream(fileName);
         byte[] buffer                     = new byte[BUFFER_SIZE];
         final String        contentLen    = response.header("Content-Length");
         int                 contentLength = Integer.parseInt(contentLen);
         int                 downloaded    = 0;
         int                 count         = 0;

         while ((count = in.read(buffer)) != -1)
         {
            downloaded += count;
            out.write(buffer, 0, count);
         }
         out.close();
         in.close();

         if (downloaded != contentLength)
            throw new IOException("downloaded " + downloaded + ", expecting " + count);
      }
      catch (NumberFormatException e) {} // from parseInt
   }

   public static String[] getMetaList(String serverIP, String resourcePath)
      throws IOException
   {
      final String  url     = "http://" + serverIP + ":8080/" + resourcePath + "meta";
      final Request request = new Request.Builder().url(url).build();

      try (Response response = httpClient.newCall(request).execute())
      {
         return response.body().string().split("\r\n");
      }
   }

   private static void getMeta(String serverIP, String resourcePath, File destDir)
      throws IOException, URISyntaxException
   {
      String[] files = getMetaList(serverIP, resourcePath);

      if (files.length == 1 && files[0].length() == 0)
         // no meta files for this directory
         return;

      for (String file : files)
         getFile(serverIP, file, new File(destDir, FilenameUtils.getName(file)));
   }

   public static void getSongs(String serverIP, String[] songs, String category, String root)
      throws IOException, URISyntaxException
   {
      // Get 'songs' from 'serverIP', store in 'root/<category>', add
      // to playlist 'root/<category>.m3u'. Also get album art, liner
      // notes for new directories.
      final File songRoot       = new File(new File(root), category);
      File       playlistFile   = new File(new File(root), category + ".m3u");
      FileWriter playlistWriter = new FileWriter(playlistFile, true); // append

      for (String song : songs)
      {
         File destDir     = new File(songRoot, FilenameUtils.getPath(song));
         File destination = new File(songRoot, song);

         if (!destDir.exists())
         {
            destDir.mkdirs();
            getMeta(serverIP, FilenameUtils.getPath(song), destDir);
         }

         getFile(serverIP, song, new File(destDir, FilenameUtils.getName(song)));

         playlistWriter.write(category + "/" + song + "\n");
      }
      playlistWriter.close();

      // File objects hold the corresponding disk file locked; later
      // unit test cannot delete them.
   }
}
