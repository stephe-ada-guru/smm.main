//  Abstract :
//
//  Provides abstract interface to retrieve metadata from audio files.
//
//  Android provides MediaMetadataRetriever, but that's broken for
//  song title, album title, and artist on Samsung Galaxy Note II
//  Android 4.1.2. So this uses the MediaStore interface.
//
//  Copyright (C) 2013 Stephen Leake.  All Rights Reserved.
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

import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore;
import android.content.ContentResolver;
import android.content.Context;

public class MetaData
{
   public String title;
   public String album;
   public String artist;
   public String duration; // service needs a string for sendStickyBroadcast, so don't bother with integer conversions

   public MetaData(Context context, String playlistName, String sourceFile)
   {
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

      try
      {
         // We'd like to just query on sourceFile, but that doesn't
         // work, apparently because String doesn't compare to DATA
         // STREAM. So we manually search the MediaStore, which is
         // produced by a scanner.
         //
         // This would be more efficient if we first searched for the
         // playlist, then looped thru the playlist looking for
         // sourceFile, comparing Strings. However, sometimes the
         // scanner misses a playlist. So we just search the entire
         // MediaStore.
         //
         // FIXME: EXTERNAL_CONTENT_URI might point to the wrong
         // volume; should use getContentUri(volumeName), but need
         // volumeName. Which is apparently either "internal" or
         // "external", but how do we know which?
         Cursor cursor = resolver.query (MediaStore.Audio.Media.EXTERNAL_CONTENT_URI, mediaFields, null, null, null);

         if (cursor == null ||
             cursor.getCount() < 1)
         {
            utils.debugLog("media query failed: " + playlistName);

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
   }
}
// end of file
