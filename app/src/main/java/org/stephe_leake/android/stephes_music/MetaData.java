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
//  Copyright (C) 2013, 2015 - 2018 Stephen Leake.  All Rights Reserved.
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

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.media.MediaMetadataRetriever;
import android.net.Uri;
import android.net.Uri.Builder;
import android.support.v4.content.FileProvider;
import java.io.File;
import java.io.FileInputStream;

public class MetaData
{
   public String  title;
   public String  album;
   public String  artist;
   public String  duration;     // service needs a string for sendBroadcast, so don't bother with integer conversions
   public Uri     musicUri;     // for sharing
   public Uri     linerUri;
   private String linerFileName;

   private String currentMusicFileName; // absolute
   private Bitmap albumArt;

   public Boolean albumArtValid()
   {
      return albumArt != null;
   }

   public Boolean linerNotesExist()
   {
      if (linerFileName == null)
         return false;
      else if (linerUri == null)
         return false;
      else
         return new File(linerFileName).exists();
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

      if (currentMusicFileName != null && currentMusicFileName.contentEquals(sourceFile))
         return;

      currentMusicFileName = sourceFile;

      // new song file

      try
      {
         MediaMetadataRetriever retriever = new MediaMetadataRetriever();

         retriever.setDataSource(currentMusicFileName);
         title    = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_TITLE);
         album    = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_ALBUM);
         artist   = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_ARTIST);
         duration = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_DURATION);
      }
      catch (Exception e)
      {
         utils.errorLog(context, "media query failed: " + e);
         title    = "<query failed>";
         album    = "<query failed>";
         artist   = "<query failed>";
         duration = "0";
      }

      // requires FileProvider declaration in AndroidManifest,
      // res/xml/provider_paths.xml, and support-v4:23.4.0 in
      // build.gradle dependencies
      try
      {
         FileProvider fileProvider = new FileProvider();

         musicUri = fileProvider.getUriForFile
            (context, BuildConfig.APPLICATION_ID + ".fileprovider", new File(currentMusicFileName));

         linerFileName = musicFile.getParentFile().getAbsolutePath() + "/liner_notes.pdf";
         linerUri = fileProvider.getUriForFile
            (context, BuildConfig.APPLICATION_ID + ".fileprovider", new File(linerFileName));
      }
      catch (IllegalArgumentException e)
      {
         // probably can't find configured root for fileProvider
         utils.errorLog(context, "fileProvider error: ", e);
         linerUri = null;
      }

      try
      {
         File albumArtFiles[] = musicFile.getParentFile().listFiles(new FileExtFilter(".jpg"));
         File albumArtFile = null;
         long maxSize = 0;

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
            utils.debugLog(e.toString());
         }
         albumArt = null;
      }
   }
}
// end of file
