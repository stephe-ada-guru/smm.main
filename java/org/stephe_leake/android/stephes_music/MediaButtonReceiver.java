//  Abstract :
//
//  Handled media buttons
//
//  This is run in the AudioManager, so it must communicate with the
//  service via intents; the same command intents the activity uses.
//
//  Copyright (C) 2015 Stephen Leake.  All Rights Reserved.
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

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.view.KeyEvent;
public class MediaButtonReceiver extends BroadcastReceiver
{
   // This receiver must also be declared in  AndroidManifest.xml to set the intent (ACTION_MEDIA_BUTTON)
   @Override public void onReceive(Context context, Intent intent)
   {
      if (Intent.ACTION_MEDIA_BUTTON.equals(intent.getAction()))
      {
         final KeyEvent event = intent.getParcelableExtra(Intent.EXTRA_KEY_EVENT);
         boolean handled = false;

         if (BuildConfig.DEBUG)
         {
            utils.verboseLog("MediaButton event: " + event);
            utils.debugLog("MediaButton event: " + event);
         }

         if (event == null)
            return;

         final int key = event.getKeyCode();
         final int action = event.getAction();

         if (BuildConfig.DEBUG) utils.verboseLog("MediaButton key: " + Integer.toString(key));

         switch (key)
         {
         case KeyEvent.KEYCODE_MEDIA_NEXT:
            if (action == KeyEvent.ACTION_DOWN)
            {
               handled = true;
               context.sendBroadcast
                  (new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_NEXT));
            }
            break;

         case KeyEvent.KEYCODE_MEDIA_PAUSE:
            if (action == KeyEvent.ACTION_DOWN)
            {
               handled = true;
               context.sendBroadcast
                  (new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PAUSE));
            }
            break;

         case KeyEvent.KEYCODE_MEDIA_PLAY:
            if (action == KeyEvent.ACTION_DOWN)
            {
               handled = true;
               context.sendBroadcast
                  (new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PLAY));
            }
            break;

         case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE:
            if (action == KeyEvent.ACTION_DOWN)
            {
               handled = true;
               context.sendBroadcast

               (new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_TOGGLEPAUSE));
            }
            break;

         case KeyEvent.KEYCODE_MEDIA_PREVIOUS:
            if (action == KeyEvent.ACTION_DOWN)
            {
               handled = true;
               context.sendBroadcast
                  (new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PREVIOUS));
            }
            break;

         };

         if (handled && isOrderedBroadcast())
            abortBroadcast();
      }
   }
};
// end of file
