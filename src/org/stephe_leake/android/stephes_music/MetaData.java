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
         // STREAM. So we query on playlistName, then loop thru the
         // playlist looking for sourceFile, comparing Strings.
         //
         // FIXME: EXTERNAL_CONTENT_URI might point to the wrong
         // volume; should use getContentUri(volumeName), but need
         // volumeName. Which is apparently either "internal" or
         // "external", but how do we know which?
         Cursor playlistCursor = resolver.query
            (MediaStore.Audio.Playlists.EXTERNAL_CONTENT_URI,
             playlistFields, "\"NAME\" == \"" + playlistName +"\"", null, null);

         if (playlistCursor == null ||
             playlistCursor.getCount() < 1)
         {
            utils.debugLog("playlist query failed: " + playlistName);

            title    = "<playlist not found>";
            album    = "<playlist not found>";
            artist   = "<playlist not found>";
            duration = "0";
         }
         else
         {
            // playlistCursor is valid; get members
            playlistCursor.moveToNext();

            try
            {
               // FIXME: hardcoding "external" as volumeName; see above
               Uri contentUri = MediaStore.Audio.Playlists.Members.getContentUri
                  ("external", playlistCursor.getLong(0));
               Cursor memberCursor = resolver.query (contentUri, mediaFields, null, null, null);

               if (memberCursor == null ||
                   memberCursor.getCount() < 1)
               {
                  utils.debugLog("playlist has no members: " + contentUri + "; " + playlistName);
               }
               else
               {
                  memberCursor.moveToNext();

                  while (!memberCursor.isAfterLast())
                  {
                     if (0 == sourceFile.compareTo(memberCursor.getString(4)))
                        break;
                     memberCursor.moveToNext();
                  };

                  if (memberCursor.isAfterLast())
                  {
                     utils.debugLog("file not found on playlist: " + sourceFile);
                     title    = "<file not found>";
                     album    = "<file not found>";
                     artist   = "<file not found>";
                     duration = "0";
                  }
                  else
                  {
                     title    = memberCursor.getString(0);
                     album    = memberCursor.getString(1);
                     artist   = memberCursor.getString(2);
                     duration = memberCursor.getString(3);
                  }
               }
            }
            catch (Exception e)
            {
               utils.debugLog("member query failed: " + e);
               title    = "<member query failed>";
               album    = "<member query failed>";
               artist   = "<member query failed>";
               duration = "0";
            }
         }
      }
      catch (Exception e)
      {
         utils.debugLog("playlist query failed: " + e);
         title    = "<playlist query failed>";
         album    = "<playlist query failed>";
         artist   = "<playlist query failed>";
         duration = "0";
      }
   }
}
// end of file
