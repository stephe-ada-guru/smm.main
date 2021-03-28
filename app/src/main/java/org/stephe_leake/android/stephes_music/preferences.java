//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011 - 2013, 2015 - 2018 Stephen Leake.  All Rights Reserved.
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

public class preferences extends android.preference.PreferenceActivity
   implements SharedPreferences.OnSharedPreferenceChangeListener
{
   // We don't implement onBuildHeaders because that's not in API 10
   // (current when this was first written), and we only have a few
   // preferences anyway.

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

      // Build list of directories that might contain smm
      // playlists.

      ListPreference logLevelPref = (ListPreference)findPreference(res.getString(R.string.log_level_key));
      String[] logLevelEntries = {"Verbose", "Info"};
      logLevelPref.setEntries(logLevelEntries);
      logLevelPref.setEntryValues(logLevelEntries);
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
      else
      {
         setResult(RESULT_OK);
      }
   }

}
