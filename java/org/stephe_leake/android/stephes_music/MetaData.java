//  Abstract :
//
//  Retrieve metadata from audio files and album art.
//
//  Design:
//
//  An initial design always returned a pointer to a bitmap. But on a
//  change from portrait to landscape, the bitmap is recycled, and
//  then the ImageView attempts to display it again (before it gets
//  the new one, apparently), causing a "recycled bitmap" error.
//
//  So we return a copy  of the bitmap.
//
//  Android provides MediaMetadataRetriever, which requires
//  READ_EXTERNAL_STORAGE permission in AndroidManifest.xml. However,
//  it's broken my Samsung Galaxy Note II and III. So this uses the
//  MediaStore interface.
//
//  Copyright (C) 2013, 2015, 2016 Stephen Leake.  All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or
//  modify it under terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 3, or
//  (at your option) any later version. This program is distributed in
//  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
//  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
//  PARTICULAR PURPOSE. See the GNU General Public License for more
//  details. You should have received a copy of the GNU General Public
//  License distributed with this program; see file COPYING. If not,
//  write to the Free Software Foundation, 51 Franklin Street, Suite
//  500, Boston, MA 02110-1335, USA.

package org.stephe_leake.android.stephes_music;

import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.media.MediaMetadataRetriever;
import android.net.Uri;
import android.net.Uri.Builder;
import android.provider.MediaStore;
import java.io.File;
import java.io.FileInputStream;

public class MetaData
{
   public String title;
   public String album;
   public String artist;
   public String duration; // service needs a string for sendStickyBroadcast, so don't bother with integer conversions
   public Uri uri; // for sharing

   private String albumArtCurrentFileName;
   private Bitmap albumArt;

   public Boolean albumArtValid()
   {
      return albumArt != null;
   }

   public Bitmap getAlbumArt()
   {
      // always return a copy, so the client can recycle it
      if (albumArt == null)
         return null;
      else
         return albumArt.copy(albumArt.getConfig(), false);
   }

   public void setMetaData(Context context, String playlistDirectory, String musicFileName)
   {
      File musicFile = new File(playlistDirectory, musicFileName);
      final String sourceFile = musicFile.getAbsolutePath();

      if (albumArtCurrentFileName != null && albumArtCurrentFileName.contentEquals(sourceFile))
         return;

      // new song file
      ContentResolver resolver = context.getContentResolver();

      String[] mediaFields =
         {
            MediaStore.Audio.Media.TITLE,
            MediaStore.Audio.Media.ALBUM,
            MediaStore.Audio.Media.ARTIST,
            MediaStore.Audio.Media.DURATION,
            MediaStore.Audio.Media.DATA
         };

      String[] playlistFields =
         {
            MediaStore.Audio.Playlists._ID,
            MediaStore.Audio.Playlists.NAME
         };

      uri = new Uri.Builder()
         .scheme("file")
         .path(sourceFile)
         .build();

      try
      {
         // We'd like to just query on sourceFile, but that doesn't
         // work, apparently because String doesn't compare to DATA
         // STREAM. So we manually search the MediaStore.
         //
         // This would be more efficient if we first searched for
         // the playlist, then looped thru the playlist looking for
         // sourceFile, comparing Strings. However, sometimes the
         // media scanner misses a playlist. So we just search the
         // entire MediaStore.
         //
         // EXTERNAL_CONTENT_URI might point to the wrong volume;
         // should use getContentUri(volumeName), but need
         // volumeName. Which is apparently either "internal" or
         // "external", but how do we know which? So far it has not
         // been a problem.
         Cursor cursor = resolver.query
            (MediaStore.Audio.Media.EXTERNAL_CONTENT_URI, mediaFields, null, null, null);

         if (cursor == null ||
             cursor.getCount() < 1)
         {
            utils.debugLog("media query failed");

            title    = "<query failed>";
            album    = "<query failed>";
            artist   = "<query failed>";
            duration = "0";
         }
         else
         {
            // cursor is valid
            cursor.moveToNext();

            while (!cursor.isAfterLast())
            {
               if (0 == sourceFile.compareTo(cursor.getString(4)))
                  break;
               cursor.moveToNext();
            };

            if (cursor.isAfterLast())
            {
               utils.debugLog("file not found: " + sourceFile);
               title    = "<file not found>";
               album    = "<file not found>";
               artist   = "<file not found>";
               duration = "0";
            }
            else
            {
               title    = cursor.getString(0);
               album    = cursor.getString(1);
               artist   = cursor.getString(2);
               duration = cursor.getString(3);
            }
         }
         cursor.close();
      }
      catch (Exception e)
      {
         utils.debugLog("query failed: " + e);
         title    = "<query failed>";
         album    = "<query failed>";
         artist   = "<query failed>";
         duration = "0";
      }

      try
      {
         File albumArtFiles[] = musicFile.getParentFile().listFiles(new FileExtFilter(".jpg"));
         File albumArtFile = null;
         long maxSize = 0;

         if (BuildConfig.DEBUG)
            utils.debugLog
               (Integer.toString(albumArtFiles.length) + ".jpg files found in " +
                musicFile.getParentFile().getAbsolutePath());

         for (File file : albumArtFiles)
         {
            if (file.length() > maxSize)
            {
               albumArtFile = file;
               maxSize = file.length();
            }
         }

         if (albumArtFile != null)
         {
            if (BuildConfig.DEBUG) utils.debugLog ("create bitmap for " + albumArtFile.getAbsolutePath());

            // FIXME: scale to layout.
            albumArt = BitmapFactory.decodeStream(new FileInputStream(albumArtFile));
         }
         else
         {
            // No album art files found
            albumArt = null;
         }
      }
      catch (java.io.FileNotFoundException e)
      {
         if (BuildConfig.DEBUG)
         {
            utils.debugLog("metadata bitmap exception");
            utils.debugLog(e);
         }
         albumArt = null;
      }
   }
}
// end of file
