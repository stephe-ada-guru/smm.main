/*
 * Copyright (C) 2011 Stephen Leake stephen_leake@stephe-leake.org
 * Copyright (C) 2007 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Copied from com.android.music in Android 2.3.3; modified to be in my package, simplify

package org.stephe_leake.android.music_player;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.media.AudioManager;
import android.view.KeyEvent;

public class MediaButtonIntentReceiver extends BroadcastReceiver
{

   private static long mLastClickTime = 0;

   @Override
      public void onReceive(Context context, Intent intent) {
      String intentAction = intent.getAction();

      MusicUtils.debugLog("MediaButtonIntentReceiver: onReceive" + intent);

      if (AudioManager.ACTION_AUDIO_BECOMING_NOISY.equals(intentAction))
      {
         //  User has removed headphones (or car connection), so the
         //  speaker will be activated; assume the user wants to
         //  pause.
         Intent i = new Intent(context, Stephes_Music_Service.class);
         i.setAction(Stephes_Music_Service.SERVICECMD);
         i.putExtra(Stephes_Music_Service.CMDNAME, Stephes_Music_Service.CMDPAUSE);
         context.startService(i);

      } else if (Intent.ACTION_MEDIA_BUTTON.equals(intentAction))
      {
         // User pressed a hardware button, such as on a headphone
         KeyEvent event = (KeyEvent)intent.getParcelableExtra(Intent.EXTRA_KEY_EVENT);

         if (event == null) return;

         int  keycode   = event.getKeyCode();
         int  action    = event.getAction();
         long eventtime = event.getEventTime();

         String command = null;
         switch (keycode)
         {
         case KeyEvent.KEYCODE_MEDIA_STOP:
            command = Stephes_Music_Service.CMDSTOP;
            break;

         case KeyEvent.KEYCODE_HEADSETHOOK:
         case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE:
            command = Stephes_Music_Service.CMDTOGGLEPAUSE;
            break;

         case KeyEvent.KEYCODE_MEDIA_NEXT:
            command = Stephes_Music_Service.CMDNEXT;
            break;

         case KeyEvent.KEYCODE_MEDIA_PREVIOUS:
            command = Stephes_Music_Service.CMDPREVIOUS;
            break;
         }

         if (command != null)
         {
            // HEADSETHOOK actions are special cased (since often that
            // is the only button available):
            //
            // single quick press: pause/resume.
            // double press: next track
            //
            // Apparently detecting double and long press is up to
            // each developer, not supported by the API (dare we site
            // MS Windows as precedent?). Sigh.

            if (action == KeyEvent.ACTION_DOWN)
            {
               // The service may or may not be running, but we need to send it
               // a command. So we use startService, not broadcast intent.
               //
               // Note that BroadcastReceivers survive the shutdown of
               // the service that registered them!

               Intent i = new Intent(context, Stephes_Music_Service.class).setAction(Stephes_Music_Service.SERVICECMD);

               if (keycode == KeyEvent.KEYCODE_HEADSETHOOK &&
                   eventtime - mLastClickTime < 300)
               {
                  // double press
                  i.putExtra(Stephes_Music_Service.CMDNAME, Stephes_Music_Service.CMDNEXT);
                  context.startService(i);
                  mLastClickTime = 0; // not eventtime, for hysteresis
               } else
               {
                  // other keycode, or HEADSETHOOK single press
                  i.putExtra(Stephes_Music_Service.CMDNAME, command);
                  context.startService(i);
                  mLastClickTime = eventtime;
               }
            }

            if (isOrderedBroadcast())
            {
               abortBroadcast();
            }
         }
      }
   }
}
