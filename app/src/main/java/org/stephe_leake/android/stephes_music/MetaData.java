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
//  Copyright (C) 2013, 2015 - 2019 Stephen Leake.  All Rights Reserved.
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
import androidx.core.content.FileProvider;
import java.io.File;
import java.io.FileInputStream;

public class MetaData
{
   public String album;
   public String albumArtist;
   public String artist;
   public String composer;
   public String duration;     // service needs a string for sendBroadcast, so don't bother with integer conversions
   public String title;
   public String year;

   public Uri linerUri;
   public Uri musicUri;     // for sharing

   private String linerFileName;
   private String currentMusicFileName; // absolute
   private Bitmap[] albumArt;

   public Boolean linerNotesExist()
   {
      if (linerFileName == null)
         return false;
      else if (linerUri == null)
         return false;
      else
         return new File(linerFileName).exists();
   }

   public int getAlbumArtCount()
   {
      if (albumArt == null)
         return 0;
      else
         return albumArt.length;
   }

   public Bitmap getAlbumArt(int index)
   {
      // Always return copies, so the client can recycle them.
      if (albumArt == null)
         return null;
      else
         try
         {
            return albumArt[index].copy(albumArt[index].getConfig(), false);
         }
         catch (java.lang.OutOfMemoryError e)
         {
            //  Image too big, or too many images; just ignore
            return null;
         }
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

         album       = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_ALBUM);
         albumArtist = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_ALBUMARTIST);
         artist      = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_ARTIST);
         composer    = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_COMPOSER);
         duration    = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_DURATION);
         title       = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_TITLE);
         year        = retriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_YEAR);
      }
      catch (Exception e)
      {
         utils.errorLog(context, "media query failed: " + e);

         album       = null;
         albumArtist = null;
         artist      = null;
         composer    = null;
         duration    = "0";
         title       = "<query failed>";
         year        = null;
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

         albumArt = new Bitmap[albumArtFiles.length];
         for (int i = 0; i < albumArtFiles.length; i++)
         {
            albumArt[i] = BitmapFactory.decodeStream(new FileInputStream(albumArtFiles[i]));
         }
      }
      catch (java.io.FileNotFoundException e)
      {
         if (BuildConfig.DEBUG)
         {
            utils.debugLog("metadata bitmap exception" + e.toString());
         }
         albumArt = null;
      }
   }

   public Uri searchUri(String serverIP)
   {
      Uri.Builder B = new Uri.Builder()
         .scheme("http")
         .encodedAuthority(serverIP + ":8080")
         .appendPath("search");

      if (title != null)
         B = B.appendQueryParameter("title", title);

      if (album != null)
         B = B.appendQueryParameter("album", album);

      if (artist != null)
         B = B.appendQueryParameter("artist", artist);

      return B.build();
   }
}
// end of file
