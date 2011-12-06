//  Abstract :
//
//  Provides background audio playback capabilities, allowing the
//  user to switch between activities without stopping playback.
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

import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.media.AudioManager.OnAudioFocusChangeListener;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.MediaMetadataRetriever;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.os.PowerManager.WakeLock;
import android.os.PowerManager;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.ListIterator;

public class service extends Service
{
   // used to specify whether enqueue() should start playing
   // the new list of files right away, next or once all the currently
   // queued files have been played

   //  Internal Messages used for delays
   private static final int FADEDOWN = 5;
   private static final int FADEUP   = 6;

   enum PlayState
   {
      Idle,
     //  Media_Player has no song Loaded

     Playing,
     //  Media_Player has song, is playing it

     Paused,
     //  Media_Player has song, is not playing it, at request of user

     Paused_Transient
     //  Media_Player has song, is not playing it, at request of
     //  system (phone call, navigation announcement, etc).
   };

   ////////// private methods (alphabetical)

   private void createMediaPlayer()
   {
      mediaPlayer = new MediaPlayer();
      mediaPlayer.setWakeMode(service.this, PowerManager.PARTIAL_WAKE_LOCK);
      mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
      mediaPlayer.setOnCompletionListener(completionListener);
      mediaPlayer.setOnErrorListener(errorListener);

      // FIXME: wake up and recover if mediaPlayer dies
      // handler.sendMessageDelayed(mHandler.obtainMessage(PLAYER_DIED), 2000);
   }

   private String currentFullPath()
   {
      return playlistDirectory + playlistFilename + ".m3u";
   }

   private void next()
   {
      stop();

      if (playlistPos.hasNext())
      {
         play(playlistPos.next());
      }
      else
      {
         // at end of playlist
      };
   }

   private void notifyChange(String what)
   {
      // Notify the activity that something has changed.
      //
      // 'What' must be one of the *_CHANGED constants in utils.java

      if (what.equals(utils.META_CHANGED))
      {
         final MediaMetadataRetriever retriever = new MediaMetadataRetriever();

         try
         {
            retriever.setDataSource(currentFullPath());

            sendStickyBroadcast
               (new Intent (utils.META_CHANGED).
                putExtra ("artist", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_ARTIST)).
                putExtra ("album", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_ALBUM)).
                putExtra ("track", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_TITLE)).
                putExtra ("duration", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_DURATION)).
                putExtra
                ("playlist",
                 playlistFilename + playlistPosInt + " /" + playlist.size()));
         }
         catch (RuntimeException e)
         {
            utils.Error_Log (this, "Notify_Change SetDataSource: " + e.toString());
         };
      }
      else if (what.equals(utils.PLAYSTATE_CHANGED))
      {
         sendStickyBroadcast
            (new Intent (utils.PLAYSTATE_CHANGED).
             putExtra ("playing", playing == PlayState.Playing));
      }
      else
      {
         utils.Error_Log (this, "Notify_Change: unexpected 'what'");
      };
   }

   private void pause(PlayState pausedState)
   {
      if (playing == PlayState.Playing)
      {
         mediaPlayer.pause();

         audioManager.abandonAudioFocus(audioFocusListener);

         playing = pausedState;

         notifyChange(utils.PLAYSTATE_CHANGED);

         saveBookmark();
         writeSMMFile();
      }
   }

   private void play (String path)
   {
      // service.playing must be Idle (call Stop first)
      //
      // FIXME: merge stop here to avoid double notification on next, prev?

      try
      {
         mediaPlayer.reset();
         mediaPlayer.setDataSource (path);
         mediaPlayer.prepare();

         currentFile = path;

         unpause();

         notifyChange(utils.META_CHANGED);
      }
      catch (IOException e)
      {
         // From SetDataSource
         utils.Error_Log (this, "can't play: " + path + ": " + e.toString());
      };
   }

   private void previous()
   {
      stop();
      if (playlistPos.hasPrevious())
      {
         play(playlistPos.previous());
      }
      else
      {
         // at start of playlist
      }
   }

   private void saveBookmark()
   {
      // FIXME: implement
   }

   private void stop()
   {
      if (currentFile != null)
      {
         mediaPlayer.reset();

         currentFile = null;
         playing = PlayState.Idle;

         notifyChange(utils.PLAYSTATE_CHANGED);
      };
   }

   private void unpause()
   {
      final int result = audioManager.requestAudioFocus
         (audioFocusListener,
          android.media.AudioManager.STREAM_MUSIC,
          android.media.AudioManager.AUDIOFOCUS_GAIN);

      if (result != android.media.AudioManager.AUDIOFOCUS_REQUEST_GRANTED)
      {
         utils.Error_Log (this, "can't get audio focus");
      }
      else
      {
         mediaPlayer.start();

         playing = PlayState.Playing;
         notifyChange (utils.PLAYSTATE_CHANGED);
      }
   }

   private void writeSMMFile()
   {
      // FIXME: implement
   }

   ////////// main members

   private MediaPlayer mediaPlayer;

   public service() {}

   ////////// nested classes

   private Handler handler = new Handler()
      {
         float volume = 1.0f;

         @Override public void handleMessage(Message msg)
         {
            utils.debugLog("handleMessage " + msg.what);
            switch (msg.what)
            {
            case FADEDOWN:
               // gradually reduce volume to 0.2
               //
               // changes volume in steps of 0.05 every 10 milliseconds

               volume -= .05f;
               if (volume > 0.2f)
               {
                  sendEmptyMessageDelayed(FADEDOWN, 10);
               }
               else
               {
                  volume = .2f;
               }
               mediaPlayer.setVolume(volume, volume);
               break;

            case FADEUP:
               // gradually increase volume to 1.0
               //
               // changes volume in steps of 0.01 every 10 milliseconds

               volume += .01f;
               if (volume < 1.0f)
               {
                  sendEmptyMessageDelayed(FADEUP, 10);
               }
               else
               {
                  volume = 1.0f;
               }
               mediaPlayer.setVolume(volume, volume);
               break;

            default:
               break;
            }
         }
      };

   private BroadcastReceiver broadcastReceiver = new BroadcastReceiver()
      {
         @Override public void onReceive(Context context, Intent intent)
         {
            final String action = intent.getAction();

            if (action.equals (android.content.Intent.ACTION_MEDIA_EJECT))
            {
               // External storage is being unmounted, probably so smm can
               // manage the playlists.
               //
               // FIXME: should check if we care; is Playlist_Directory on the volume being ejected?
               pause(PlayState.Paused);
            }
            else if (action.equals (android.content.Intent.ACTION_MEDIA_MOUNTED))
            {
               // External storage is being mounted, probably after smm
               // managed the playlists.

               // FIXME: implement
               // resume;
            }
            else if (action.equals (utils.ACTION_TOGGLEPAUSE))
            {
               switch (service.playing)
               {
               case Idle:
                  break;

               case Playing:
                  pause(PlayState.Paused);
                  break;

               case Paused:
                  unpause();
                  break;

               case Paused_Transient:
                  // wait until get audio focus back?
                  break;

               };
            }
            else if (action.equals (utils.ACTION_PAUSE))
            {
               pause(PlayState.Paused);
            }
            else if (action.equals (utils.ACTION_PREVIOUS))
            {
               previous();
            }
            else if (action.equals (utils.ACTION_NEXT))
            {
               next();
            }
            else
            {
               utils.debugLog("broadcastReceiver.onReceive: unkown action: " + action);
            }
         }
      };

    private OnAudioFocusChangeListener audioFocusListener = new OnAudioFocusChangeListener()
       {
          public void onAudioFocusChange(int focusChange)
          {
             if (focusChange == AudioManager.AUDIOFOCUS_LOSS)
             {
                pause(PlayState.Paused);
             }
             else if (focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK)
             {
                // "DUCK" means can just lower volume; not clear when this might happen
                handler.removeMessages(FADEUP);
                handler.sendEmptyMessage(FADEDOWN);
             }
             else if (focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT)
             {
                if (service.playing == PlayState.Playing)
                {
                   pause(PlayState.Paused_Transient);
                }
             }
             else if (focusChange == AudioManager.AUDIOFOCUS_GAIN)
                handler.removeMessages(FADEUP); // in case loss was 'transient can duck'

             if (service.playing == PlayState.Paused_Transient)
             {
                mediaPlayer.start();
             }
             else
             {
                utils.debugLog ("Unknown onAudioFocusChange code " + focusChange);
             }
          }
       };

   MediaPlayer.OnCompletionListener completionListener = new MediaPlayer.OnCompletionListener()
      {
         public void onCompletion(MediaPlayer mp)
         {
            next();
         }
      };

   MediaPlayer.OnErrorListener errorListener = new MediaPlayer.OnErrorListener()
      {
         public boolean onError(MediaPlayer mp, int what, int extra)
         {
            utils.Error_Log(service.this, "MediaPlayer server died: " + what + "," + extra);
            switch (what)
            {
            case MediaPlayer.MEDIA_ERROR_SERVER_DIED:
               mediaPlayer.release();
               createMediaPlayer();
               // Since we don't know why it died, just trying again seems
               // problematic, but it is the most user friendly if it
               // works. This will _not_ be easy to debug!
               if (service.playing == PlayState.Playing)
               {
                  play (currentFile);
               };

               return true;
            default:
               // OnCompletion will be called
               return false;
            }
         }
      };

   private AudioManager audioManager;

   // state
   static private PlayState playing;
   private String    currentFile;
   private String    playlistDirectory;
   // Absolute path to directory where playlist files reside. The
   // list of available playlists consists of all .m3u files in
   // this directory.

   private String playlistFilename;
   // Relative to Playlist_Directory, without extension (suitable
   // for user display). null if no playlist is current.

   java.util.LinkedList<String>   playlist;
   java.util.ListIterator<String> playlistPos;
   int                            playlistPosInt;
   // Current position in PlayList

   ////////// service lifetime methods
   @Override public IBinder onBind(Intent intent)
   {
      return null;
   }

   @Override public void onCreate()
   {
      super.onCreate();

      IntentFilter filter = new IntentFilter();
      filter.addAction(Intent.ACTION_MEDIA_EJECT);
      filter.addAction(Intent.ACTION_MEDIA_MOUNTED);
      filter.addAction(utils.ACTION_NEXT);
      filter.addAction(utils.ACTION_PAUSE);
      filter.addAction(utils.ACTION_PLAYLIST);
      filter.addAction(utils.ACTION_PREVIOUS);
      filter.addAction(utils.ACTION_SEEK);
      filter.addAction(utils.ACTION_TOGGLEPAUSE);
      filter.addAction(utils.ACTION_UPDATE_DISPLAY);
      registerReceiver(broadcastReceiver, filter);

      createMediaPlayer();

      audioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);

      playlist       = new LinkedList<String>();
      playlistPos    = playlist.listIterator (0);
      playlistPosInt = 0;
      playing        = PlayState.Paused;

      // FIXME: implement
      // This.Resume;
   }

   @Override public void onDestroy()
   {
      mediaPlayer.reset();
      mediaPlayer.release();
      mediaPlayer = null;

      audioManager.abandonAudioFocus(audioFocusListener);

      handler.removeCallbacksAndMessages(null);

      unregisterReceiver(broadcastReceiver);

      super.onDestroy();
   }

   @Override public int onStartCommand(Intent intent, int flags, int startId)
   {
      if (intent != null)
      {
         utils.Error_Log(this, "onStartCommand got unexpected intent: " + intent);
      }
      return START_STICKY;
    }

}
