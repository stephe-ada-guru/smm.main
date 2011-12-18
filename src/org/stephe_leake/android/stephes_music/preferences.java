//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011 Stephen Leake.  All Rights Reserved.
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

public class preferences extends android.preference.PreferenceActivity
{
   // We don't implement onBuildHeaders because that's not in API 10,
   // and we only have a few preferences anyway.

   @Override public void onCreate(android.os.Bundle savedInstanceState)
   {
      super.onCreate(savedInstanceState);

      // FIXME: set default; what is xml syntax for defaults?

      addPreferencesFromResource(R.layout.preferences);
   }
}
