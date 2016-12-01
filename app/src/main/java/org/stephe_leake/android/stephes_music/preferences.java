//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011 - 2013, 2015 - 2016 Stephen Leake.  All Rights Reserved.
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
import android.os.Environment;
import android.preference.ListPreference;
import android.preference.PreferenceManager;
import android.preference.MultiSelectListPreference;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;

public class preferences extends android.preference.PreferenceActivity
   implements SharedPreferences.OnSharedPreferenceChangeListener
{
   // We don't implement onBuildHeaders because that's not in API 10
   // (current when this was first written), and we only have a few
   // preferences anyway.

   private final FileExtFilter playlistFilter = new FileExtFilter(".m3u");

   public class DirFilter implements FilenameFilter
   {
      @Override public boolean accept(File dir, String filename)
      {
         return (new File(dir, filename)).isDirectory();
      }
   }
   private final DirFilter dirFilter = new DirFilter();

   // Return path to 'dir' if 'dir' contains smm playlists
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

   // addPreferencesFromResource, findPreference are deprecated
   //
   // Waiting until they actually disappear; the fix will
   // probably be different by then.
   @SuppressWarnings("deprecation")
   @Override public void onCreate(android.os.Bundle savedInstanceState)
   {
      Resources res = getResources();

      super.onCreate(savedInstanceState);

      try
      {
         addPreferencesFromResource(R.xml.preferences);
      }
      catch (RuntimeException e)
      {
         // Probably a stored preference value is now incompatible
         // with new code. There doesn't seem to be a way to clear all
         // stored preferences from code; user must delete stored
         // preferences in Settings | Applications
         utils.errorLog(this, "Can't read stored preferences: clear data in Settings | Applications");
      }

      // Build list of top level directories that might contain smm
      // playlists.

      ListPreference playlistPref = (ListPreference)findPreference(res.getString(R.string.playlist_directory_key));

      LinkedList<String> playlistDirs = new LinkedList<String>();
      LinkedList<String> likelyRoots  = new LinkedList<String>();

      // There are several names for internal storage, due to
      // symlinks. We need the one used by the media scanner, since we
      // search for file names in the scanned db. That is _not_ the
      // canonical (symlinks resolved) name.

      likelyRoots.add("/storage/sdcard");     // emulator external; can't see any emulator internal
      likelyRoots.add("/storage/sdcard0");    // Samsung Galaxy note II/III internal
      likelyRoots.add("/storage/emulated/0"); // Samsung Galaxy note Tab S2 internal (as used by media scanner)
      likelyRoots.add("/storage/extSdCard");  // Samsung Galaxy note II/III/Tab S2 external

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
                  playlistDirs.add(dir);
            };
         };
      } while (likelyRootsI.hasNext());

      // On a clean install, user must download new playlists into
      // some directory; this should work.
      File tmp = Environment.getExternalStoragePublicDirectory("Audio");
      playlistDirs.add(tmp.getAbsolutePath());

      final String[] playlistDirsArray = new String[playlistDirs.size()];
      Iterator<String> playlistDirsI = playlistDirs.iterator();

      for (int i = 0; i < playlistDirsArray.length; i++)
      {
         playlistDirsArray[i] = playlistDirsI.next();
      }

      playlistPref.setEntries(playlistDirsArray);
      playlistPref.setEntryValues(playlistDirsArray);


      // Create list of possible smm directories

      ListPreference smmPref = (ListPreference)findPreference(res.getString(R.string.smm_directory_key));

      // WORKAROUND: getExternalCacheDirs is wrong for the sdcard on
      // my Galaxy Note III with Android 5.2, so hardcode that.
      File[] extCacheDirs = this.getExternalCacheDirs();

      String[] smmDirs = new String[extCacheDirs.length + 2];
      int last = 0;

      smmDirs[0] = this.getCacheDir().getAbsolutePath();

      for (int i = 0; i < extCacheDirs.length; i++)
      {
         if (extCacheDirs[i] != null)
         {
            smmDirs[i + 1] = extCacheDirs[i].getAbsolutePath();
            last = last + 1;
         }
      }

      smmDirs[last] = "/storage/extSdCard/Android/data/org.stephe_leake.android.stephes_music/cache";
      last = last + 1;

      smmDirs = Arrays.copyOfRange(smmDirs, 0, last);

      smmPref.setEntries(smmDirs);
      smmPref.setEntryValues(smmDirs);
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

   @Override public void onSharedPreferenceChanged(SharedPreferences prefs, String key)
   {
      Resources res = getResources();

      if (key.equals(res.getString(R.string.text_scale_key)))
      {
         setResult(utils.RESULT_TEXT_SCALE);
      }
      else if (key.equals(res.getString(R.string.smm_directory_key)))
      {
         setResult(utils.RESULT_SMM_DIRECTORY);
      }
      else if (key.equals(res.getString(R.string.playlist_directory_key)))
      {
         // Set list of playlists in auto-download pref
         ListPreference playlistPref = (ListPreference)findPreference(res.getString(R.string.playlist_directory_key));
         File           playlistDir  = new File(playlistPref.getValue());

         MultiSelectListPreference autodownloadPref = (MultiSelectListPreference)
            findPreference(res.getString(R.string.auto_download_playlists_key));

         final String[] playlists = playlistDir.list(playlistFilter);

         autodownloadPref.setEntries(playlists);
         autodownloadPref.setEntryValues(playlists);

         setResult(RESULT_OK);
      }
      else
      {
         setResult(RESULT_OK);
      }
   };

}
