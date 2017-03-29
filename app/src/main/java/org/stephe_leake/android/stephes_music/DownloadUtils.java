//  Abstract :
//
//  Utilities for downloading from smm_server
//
//  Copyright (C) 2016, 2017 Stephen Leake. All Rights Reserved.
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

import android.media.MediaScannerConnection;
import android.content.Context;

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
import java.io.PrintWriter;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.io.filefilter.FalseFileFilter;
import org.apache.commons.io.filefilter.SuffixFileFilter;
import org.apache.commons.io.filefilter.TrueFileFilter;

import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.MediaType;

public class DownloadUtils
{
   private static final int BUFFER_SIZE = 8 * 1024;

   // used in processDirEntry
   private static String       playlistDir;
   private static List<String> mentionedFiles;

   private static OkHttpClient httpClient = null;

   private static String logFileBaseName = "download_log";

   public static String logFileName()
   {
      return utils.smmDirectory + "/" + logFileBaseName + utils.logFileExt;
   }

   public static LogLevel prefLogLevel = LogLevel.Info;

   public static void log(Context context, LogLevel level, String msg)
   {
      if (level.toInt() >= prefLogLevel.toInt() )
      {
         utils.log(context, level, msg, logFileBaseName);
      }
   }

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

   public static int editPlaylist(Context context, String playlistFilename, String lastFilename)
      throws IOException
   // Delete lines from start of playlist file up to but not including
   // line in last file; that song is currently being played.
   //
   // Return delete count.
   //
   // throws IOException if can't read or write playlistFilename
   {
      int deleteCount = 0;

      try
      {
         List<String>     lines       = readPlaylist(playlistFilename, false);
         LineNumberReader input       = new LineNumberReader(new FileReader(lastFilename));
         String           lastPlayed  = input.readLine();
         boolean          found       = false;

         input.close();

         if (null == lastPlayed)
         {
            // last file is empty; nothing to delete
         }
         else
         {
            // Check if lastPlayed is in playlist
            for (String line : lines)
            {
               if (!found)
                  found = line.equals(lastPlayed);
            }

            if (found)
            {
               FileWriter output = new FileWriter(playlistFilename); // Erases file

               found = false;

               for (String line : lines)
               {
                  if (!found)
                  {
                     found = line.equals(lastPlayed);
                  }
                  if (found)
                     output.write(line + "\n");
                  else
                     deleteCount++;
               }
               output.close();
            }
         }
      }
      catch (FileNotFoundException e)
      { // from 'FileReader(lastFilename)'; file not found; same as empty; do nothing
      }

      return deleteCount;
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

   private static int processDirEntry(Context context, File entry)
   // Returns count of files deleted
   {
      int entryCount  = 0;
      int deleteCount = 0;

      if (entry.isDirectory())
      {
         // listFiles returns "." as the dir name; does not return ".."; cannot filter it!

         // Delete music files not in playlist
         for (File subDir : FileUtils.listFiles(entry, new SuffixFileFilter(".mp3"), FalseFileFilter.FALSE))
            deleteCount += processDirEntry(context, subDir);

         // Recurse into directories
        for (File subDir : FileUtils.listFilesAndDirs(entry, FalseFileFilter.FALSE, TrueFileFilter.TRUE))
           if (subDir != entry)
              deleteCount += processDirEntry(context, subDir);

         // Delete dir if there's no music or subdirectories left
         //
         // Count directories
         for (File subDir : FileUtils.listFilesAndDirs(entry, FalseFileFilter.FALSE, TrueFileFilter.TRUE))
            if (subDir != entry)
               entryCount++;

         if (0 == entryCount)
         {
            // Count music files
            for (File subDir : FileUtils.listFiles(entry, new SuffixFileFilter(".mp3"), FalseFileFilter.FALSE))
               entryCount++;
         }

         if (0 == entryCount)
            try
            {
               FileUtils.deleteDirectory(entry);
            }
            catch (IOException e)
            {
               log(context, LogLevel.Error, "cannot delete directory '" + entry.getAbsolutePath() + "'");
            }
      }
      else if (entry.isFile())
      {
         final String name = relativeName(playlistDir, entry.getAbsolutePath()).toLowerCase();

         if (!mentionedFiles.contains(name))
         {
            entry.delete();
            deleteCount++;
         }
      }
      // else special file; ignored

      return deleteCount;
   }

   private static void ensureHttpClient()
   {
      if (null == httpClient)
      {
         httpClient = new OkHttpClient.Builder()
            .retryOnConnectionFailure(true)
            .connectTimeout(5, TimeUnit.MINUTES) // don't have to handle retry at higher level
            .build();
      }
   }

   public static void cleanPlaylist(Context context, String category, String playlistDir, String smmDir)
   {
      //  Delete lines in category.m3u that are before song in
      //  SMM_Dir/category.last.
      //
      //  Directory names end in '/'

      // We can't declare a File object for lastFile; that prevents
      // delete in editPlaylist.
      final String playlistFilename = FilenameUtils.concat(playlistDir, category + ".m3u");
      final String lastFilename     = FilenameUtils.concat(smmDir, category + ".last");

      try
      {
         // getPath() returns empty string if file does not exist
         if ("" != FilenameUtils.getPath(playlistFilename))
            if ("" != FilenameUtils.getPath(lastFilename))
            {
               int deleteCount = editPlaylist(context, playlistFilename, lastFilename);
               log(context, LogLevel.Info, category + " playlist cleaned: " + deleteCount + " songs deleted");
            }
      }
      catch (IOException e)
      {
         // from editPlaylist (which calls readPlaylist)
         log(context, LogLevel.Error, "cannot read/write playlist '" + playlistFilename + "'");
      }
   }

   public static void cleanSongs(Context context, String category, String playlistDir, String smmDir)
   {
      //  Delete files from Playlist_Dir/Category that are not
      //  mentioned in playlist file category.m3u.
      //
      //  Directory names end in '/'

      // We can't declare a File object for playlistFile or lastFile;
      // that prevents rename, delete in editPlaylist.
      final String playlistFilename = FilenameUtils.concat(playlistDir, category + ".m3u");
      final String lastFilename     = FilenameUtils.concat(smmDir, category + ".last");

      DownloadUtils.playlistDir = playlistDir;

      try
      {
         mentionedFiles = readPlaylist(playlistFilename, true);

         //  Search playlist directory, delete files not in playlist
         {
            File targetDir   = new File(playlistDir, category);
            int  deleteCount = 0;

            for (File subDir : FileUtils.listFilesAndDirs(targetDir, FalseFileFilter.FALSE, TrueFileFilter.TRUE))
               if (subDir != targetDir)
                  deleteCount += processDirEntry(context, subDir);

            log(context, LogLevel.Info, category + " song storage cleaned: " + deleteCount + " files deleted");
         }
      }
      catch (IOException e)
      {
         // from editPlaylist (which calls readPlaylist)
         log(context, LogLevel.Error, "cannot read/write playlist '" + playlistFilename + "'");
      }
   }

   public static StatusStrings getNewSongsList(Context context,
                                               String  serverIP,
                                               String  category,
                                               int     count,
                                               int     newCount,
                                               int     randomSeed)
   // randomSeed = -1 means randomize; other values used in unit tests.
   {
      final String url = "http://" + serverIP + ":8080/download?" +
         "category=" + category +
         "&count=" + Integer.toString(count) +
         "&new_count=" + Integer.toString(newCount) +
         (-1 == randomSeed ? "" : "&seed=" + Integer.toString(randomSeed));

      Request       request = new Request.Builder().url(url).build();
      StatusStrings result  = new StatusStrings();

      ensureHttpClient();

      try (Response response = httpClient.newCall(request).execute())
      {
         try
         {
            result.strings = response.body().string().split("\r\n");
         }
         catch (IOException e)
         {
            // From response.body()
            log(context, LogLevel.Error, "getNewSongsList request has no body: " + e.toString());
            result.status = ProcessStatus.Fatal;
         }
      }
      catch (IOException e)
      {
         // From httpClient.newCall; connection failed after retry
         log(context, LogLevel.Error, "getNewSongsList '" + url + "': http request failed: " + e.toString());
         result.status = ProcessStatus.Retry;
      }

      log(context, LogLevel.Info, "getNewSongsList: " + Integer.toString(result.strings.length) + " songs");
      return result;
   }

   static private final String timeOriginString = "1958-001-00:00:00.000";

   private static StatusCount getFile(Context                context,
                                      String                 serverIP,
                                      String                 resource,
                                      File                   fileName,
                                      MediaScannerConnection mediaScanner)
   {
      // Get 'resource' from 'serverIP', store in 'fileName'.
      // 'resource' shall have only path and file name.
      //
      // Return result.status Success if successful, Fatal or Retry
      // for any errors (error messages in log); result.count = 1 if
      // file has never been downloaded before, 0 otherwise..

      StatusCount result = new StatusCount();

      HttpUrl url = new HttpUrl.Builder()
         .scheme("http")
         .host(serverIP)
         .port(8080)
         .addPathSegments(resource)
         .build();

      try
      {
         fileName.createNewFile();
      }
      catch (IOException e)
      {
         log(context, LogLevel.Error, "cannot create file " + fileName.getAbsolutePath());
         result.status = ProcessStatus.Fatal;
         return result;
      }

      try (Response response = httpClient.newCall(new Request.Builder().url(url).build()).execute())
      {
         if (!response.isSuccessful())
         {
            log(context, LogLevel.Error, "getFile '" + url.toString() +"' request failed: " +
                response.code() + " " + response.message());
            result.status = ProcessStatus.Retry;
            return result;
         }

         BufferedInputStream in             = new BufferedInputStream(response.body().byteStream());
         FileOutputStream    out            = new FileOutputStream(fileName);
         byte[] buffer                      = new byte[BUFFER_SIZE];
         final String        contentLen     = response.header("Content-Length");
         final String        prevDownloaded = response.header("X-prev_downloaded"); // only on mp3 files
         int                 contentLength  = Integer.parseInt(contentLen);
         int                 downloaded     = 0;
         int                 count          = 0;

         if ((prevDownloaded != null) &&
             (prevDownloaded.equals(timeOriginString)))
            result.count += 1;

         while ((count = in.read(buffer)) != -1)
         {
            downloaded += count;
            out.write(buffer, 0, count);
         }
         out.close();
         in.close();

         if (downloaded != contentLength)
            log(context, LogLevel.Error, "downloading '" + resource + "'; got " +
                downloaded + "bytes, expecting " + contentLength);
         else
            log(context, LogLevel.Verbose, "downloaded '" + resource + "'");

         {
            String ext = FilenameUtils.getExtension(fileName.toString());
            String mime = "";

            // FIXME: debugging
            if (ext.equals("mp3"))
               log(context, LogLevel.Verbose, "prev_downloaded '" + prevDownloaded + "'");

            if (ext.equals("jpg"))
               mime = "image/jpeg";
            else if (ext.equals("mp3"))
               mime = "audio/mpeg";
            else if (ext.equals("pdf"))
               mime = "application/pdf";

            if (null != mediaScanner)
               mediaScanner.scanFile(fileName.getAbsolutePath(), mime);
         }
      }
      catch (IOException e)
      {
         // From httpClient.newCall; connection failed after retry
         log(context, LogLevel.Error, "http request failed: getFile '" + resource + "': " + e.toString());
         result.status = ProcessStatus.Retry;
      }
      catch (NumberFormatException e)
      {
         // from parseInt; corrupted Internet transmission. 'contentLen' not visible here.
         log(context, LogLevel.Error, "parseInt failed");
         result.status = ProcessStatus.Retry;
      }

      return result;
   }

   public static StatusStrings getMetaList(Context context,
                                           String  serverIP,
                                           String  resource)
   {
      StatusStrings result = new StatusStrings();

      HttpUrl url = new HttpUrl.Builder()
         .scheme("http")
         .host(serverIP)
         .port(8080)
         .addPathSegments(resource)
         .addPathSegment("meta")
         .build();

      final Request request = new Request.Builder().url(url).build();

      ensureHttpClient();

      try (Response response = httpClient.newCall(request).execute())
      {
         try
         {
            result.strings = response.body().string().split("\r\n");
         }
         catch (IOException e)
         {
            // From response.body(); server error possibly due to corrupted file name
            log(context, LogLevel.Error, "getMetaList request has no body: " + e.toString());
            result.status = ProcessStatus.Retry;
         }
      }
      catch (IOException e)
      {
         // From httpClient.newCall; connection failed after retry
         log(context, LogLevel.Error, "http request failed: getMetaList '" + resource + "': " + e.toString());
         result.status = ProcessStatus.Retry;
      }

      return result;
   }

   private static ProcessStatus getMeta(Context                context,
                                        String                 serverIP,
                                        String                 resourcePath,
                                        File                   destDir,
                                        MediaScannerConnection mediaScanner)
   {
      StatusStrings files;
      StatusCount   fileStatus;
      File          objFile;

      files = getMetaList(context, serverIP, resourcePath);

      if (ProcessStatus.Success != files.status)
         return files.status;

      if (files.strings.length == 1 && files.strings[0].length() == 0)
         // no meta files for this directory
         return ProcessStatus.Success;

      for (String file : files.strings)
      {
         objFile = new File(destDir, FilenameUtils.getName(file));

         fileStatus = getFile(context, serverIP, file, objFile, mediaScanner);
         if (ProcessStatus.Success != fileStatus.status)
            return fileStatus.status;
      }
      return ProcessStatus.Success;
   }

   public static StatusCount getSongs(Context                context,
                                      String                 serverIP,
                                      String[]               songs,
                                      String                 category,
                                      String                 root,
                                      MediaScannerConnection mediaScanner)
   {
      // Get 'songs' from 'serverIP', store in 'root/<category>', add
      // to playlist 'root/<category>.m3u'. Also get album art, liner
      // notes for new directories. Add files to mediaScanner, if not
      // null.

      final File    songRoot     = new File(new File(root), category);
      File          playlistFile = new File(new File(root), category + ".m3u");
      FileWriter    playlistWriter;
      StatusCount   result       = new StatusCount();
      ProcessStatus metaStatus;
      StatusCount   fileStatus;
      Integer       newSongs     = 0;

      try
      {
         playlistWriter = new FileWriter(playlistFile, true); // append
      }
      catch (IOException e)
      {
         log(context, LogLevel.Error, "cannot open '" + playlistFile.getAbsolutePath() + "' for append.");
         result.status = ProcessStatus.Fatal;
         return result;
      }

      try
      {
         for (String song : songs)
         {
            File destDir     = new File(songRoot, FilenameUtils.getPath(song));
            File destination = new File(songRoot, song);
            File songFile;

            if (!destDir.exists())
            {
               destDir.mkdirs();

               metaStatus = getMeta(context, serverIP, FilenameUtils.getPath(song), destDir, mediaScanner);
               switch (metaStatus)
               {
               case Success:
                  break;

               case Fatal:
               case Retry:
                  // Delete dir so meta will be downloaded on retry
                  destDir.delete();
                  result.status = metaStatus;
                  break;
               };
            }

            if (result.status == ProcessStatus.Success)
            {
               songFile   = new File(destDir, FilenameUtils.getName(song));
               fileStatus = getFile(context, serverIP, song, songFile, mediaScanner);

               switch (fileStatus.status)
               {
               case Success:
                  playlistWriter.write(category + "/" + song + "\n");
                  result.count++;
                  newSongs = newSongs + fileStatus.count;
                  break;

               case Fatal:
               case Retry:
                  result.status = fileStatus.status;
                  break;
               };
            }
         }
      }
      catch (IOException e)
      {
         // From playlistWriter.write
         log(context, LogLevel.Error, "cannot append to '" + playlistFile.getAbsolutePath() + "'; disk full?");
         result.status = ProcessStatus.Fatal; // non-recoverable
      }
      finally
      {
         try
         {
            playlistWriter.close();
         }
         catch (IOException e)
         {
            // probably from flush cache
            log(context, LogLevel.Error, "cannot close '" + playlistFile.getAbsolutePath() + "'; disk full?");
            result.status = ProcessStatus.Fatal; // non-recoverable
         }
      }

      log(context, LogLevel.Info, result.count + " " + category + " songs downloaded, " + newSongs + " new.");

      return result;

      // File objects hold the corresponding disk file locked; later
      // unit test cannot delete them.
   }

   public static ProcessStatus sendNotes(Context context,
                                         String  serverIP,
                                         String  category,
                                         String  smmDir)
   {
      File          noteFile = new File(smmDir, category + ".note");
      ProcessStatus status   = ProcessStatus.Success;

      if (noteFile.exists())
      {
         final String url  = "http://" + serverIP + ":8080/remote_cache/" + category + ".note";
         String       data = "";

         try
         {
            LineNumberReader noteReader = new LineNumberReader(new FileReader(noteFile));
            String           line       = noteReader.readLine();

            while (line != null)
            {
               // Doc for LineNumberReader says 'line' includes line terminators, but it doesn't.
               data = data + line + "\r\n";
               line = noteReader.readLine();
            };

         }
         catch (FileNotFoundException e){} // from noteReader constructor; can't get here
         catch (IOException e){} // from noteReader.readLine; can't get here

         {
            final TextBody body    = new TextBody(data);
            final Request  request = new Request.Builder()
               .url(url)
               .put(body)
               .build();

            ensureHttpClient();

            try (Response response = httpClient.newCall(request).execute())
            {
               if (200 != response.code())
               {
                  status = ProcessStatus.Fatal; // something wrong with server
                  log(context, LogLevel.Error, "put notes failed " + response.message());
               }
               else
                  log(context, LogLevel.Info, category + " sendNotes");
            }
            catch (IOException e)
            {
               // From httpClient.newCall; connection failed after retry
               log(context, LogLevel.Error, category + " sendNotes http request failed: " + e.toString());
               status = ProcessStatus.Retry; // retry after delay
            }
         }

         noteFile.delete();
      }

      return status;
   }
}
