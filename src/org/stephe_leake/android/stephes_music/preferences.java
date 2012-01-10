//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011, 2012 Stephen Leake.  All Rights Reserved.
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

import android.content.SharedPreferences;
import android.content.res.Resources;
import android.preference.ListPreference;
import android.preference.PreferenceManager;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Iterator;
import java.util.LinkedList;

public class preferences extends android.preference.PreferenceActivity
   implements SharedPreferences.OnSharedPreferenceChangeListener
{
   // We don't implement onBuildHeaders because that's not in API 10,
   // and we only have a few preferences anyway.

   private final FileExtFilter playlistFilter = new FileExtFilter(".m3u");

   public class DirFilter implements FilenameFilter
   {
      @Override public boolean accept(File dir, String filename)
      {
         return (new File(dir, filename)).isDirectory();
      }
   }
   private final DirFilter dirFilter = new DirFilter();

   // Return true if 'dir' contains smm playlists
   private String checkDirectory (File root, String dir)
   {
      final File playlistDir   = new File(root, dir);
      final String[] playlists = playlistDir.list(playlistFilter);

      if (playlists == null || playlists.length == 0)
      {
         return null;
      }

      // some playlists found; check that corresponding directories exist
      for (int i = 0; i < playlists.length; i++)
      {
         final String dirName = playlists[i].substring(0, playlists[i].length() - 4); // strip ".m3u"
         if (new File (playlistDir, dirName).isDirectory())
         {
            return playlistDir.getPath();
         }
      }
      return null;
   }

   @Override public void onCreate(android.os.Bundle savedInstanceState)
   {
      Resources res = getResources();

      super.onCreate(savedInstanceState);

      addPreferencesFromResource(R.xml.preferences);

      ListPreference playlistPref = (ListPreference)findPreference(res.getString(R.string.playlist_directory_key));

      // Build list by scanning media for smm playlists.

      LinkedList<String> smmDirs = new LinkedList<String>();

      LinkedList<String> likelyRoots = new LinkedList<String>();

      likelyRoots.add("/sdcard");            // standard Android phone

      // On a Google TV, there is no sdcard, but there are USB ports; search for those
      final File tvRoot = new File("/mnt/media");

      final String[] likelyTvRoots = tvRoot.list(dirFilter);

      if (likelyTvRoots != null)
      {
         for (int i = 0; i < likelyTvRoots.length; i ++)
         {
            likelyRoots.add(tvRoot.getPath() + "/" + likelyTvRoots[i]);
         }
      }

      Iterator<String> likelyRootsI = likelyRoots.iterator();

      do
      {
         final File root           = new File(likelyRootsI.next());
         final String[] likelyDirs = root.list(dirFilter);

         if (likelyDirs != null)
         {
            for (int j = 0; j < likelyDirs.length; j++)
            {
               String dir = checkDirectory(root, likelyDirs[j]);
               if (dir != null)
                  smmDirs.add(dir);
            };
         };
      } while (likelyRootsI.hasNext());

      final String[] smmDirsArray = new String[smmDirs.size()];
      Iterator<String> smmDirsI = smmDirs.iterator();

      for (int i = 0; i < smmDirsArray.length; i++)
      {
         smmDirsArray[i] = smmDirsI.next();
      }

      playlistPref.setEntries(smmDirsArray);
      playlistPref.setEntryValues(smmDirsArray);
   }

   @Override protected void onResume()
   {
      super.onResume();
      PreferenceManager.getDefaultSharedPreferences(this).registerOnSharedPreferenceChangeListener(this);

      setResult(RESULT_OK);
   }

   @Override protected void onPause()
   {
      super.onPause();
      PreferenceManager.getDefaultSharedPreferences(this).unregisterOnSharedPreferenceChangeListener(this);
   }

   @Override public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key)
   {
      if (key.equals(getResources().getString(R.string.text_scale_key)))
      {
         setResult(utils.RESULT_TEXT_SCALE);
      }
      else
      {
         setResult(RESULT_OK);
      }
   };

}
