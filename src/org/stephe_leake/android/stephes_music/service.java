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
import java.io.BufferedReader;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedList;

public class service extends Service
{
   // used to specify whether enqueue() should start playing
   // the new list of files right away, next or once all the currently
   // queued files have been played

   //  Internal Messages used for delays
   private static final int FADEDOWN = 5;
   private static final int FADEUP   = 6;

   // misc constants
   private static final long PREV_THRESHOLD = 5000;
   // milliseconds; see previous()
   // FIXME: get from preferences

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

      // FIXME: update position display periodically

      // FIXME: wake up and recover if mediaPlayer dies (do this when update display?)
      // handler.sendMessageDelayed(mHandler.obtainMessage(PLAYER_DIED), 2000);
   }

   private String playlistFullPath()
   // return current playlist file abs path
   {
      return playlistDirectory + "/" + playlistFilename + ".m3u";
   }

   private void next()
   {
      stop();

      if (playlistPos < playlist.size() - 1)
      {
         playlistPos++;
      }
      else
      {
         // at end of playlist; wrap
         playlistPos = 0;
      };

      play(playlist.get(playlistPos));
   }

   private void notifyChange(String what)
   {
      // Notify the activity that something has changed.
      //
      // 'What' must be one of the *_CHANGED constants in utils.java

      if (what.equals(utils.META_CHANGED))
      {
         final MediaMetadataRetriever retriever = new MediaMetadataRetriever();

         final String absFile = playlistDirectory + "/" + currentFile;

         try
         {

            retriever.setDataSource(absFile);

            sendStickyBroadcast
               (new Intent (utils.META_CHANGED).
                putExtra ("artist", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_ARTIST)).
                putExtra ("album", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_ALBUM)).
                putExtra ("track", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_TITLE)).
                putExtra ("duration", retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_DURATION)).
                putExtra
                ("playlist",
                 playlistFilename + " " + playlistPos + " / " + playlist.size()));
         }
         catch (RuntimeException e)
         {
            utils.errorLog (this, "notifyChange SetDataSource (" + absFile + "): " + e.toString());
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
         utils.errorLog (this, "notifyChange: unexpected 'what'");
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
      // path must be relative to playlistDirectory

      // service.playing must be Idle (call Stop first)
      //
      // FIXME: merge stop here to avoid double notification on next, prev?

      try
      {
         final String absFile = playlistDirectory + "/" + path;

         mediaPlayer.reset();
         mediaPlayer.setDataSource (absFile);
         mediaPlayer.prepare();

         currentFile = path;

         unpause();

         notifyChange(utils.META_CHANGED);
      }
      catch (IOException e)
      {
         // From SetDataSource
         utils.errorLog (this, "can't play: " + path + ": " + e.toString());
      }
      catch (RuntimeException e)
      {
         utils.errorLog (this, "play: " + e.toString());
      }
   }

   private void playList(String filename)
   {
      final File playlistFile = new File(filename);

      if (!playlistFile.canRead())
      {
         utils.errorLog(this, "can't read " + filename);
         return;
      }

      try
      {
         BufferedReader               in          = new BufferedReader (new FileReader (filename));
         String                       line        = in.readLine();
         java.util.LinkedList<String> tmpPlaylist = new LinkedList<String>();
         int                          lineCount   = 0;

         while (line != null)
         {
            // We don't check for readable now, because that might
            // change by the time we get to actually playing a song.
            //
            // In SMM playlists all lines are song filepaths, relative
            // to the directory filename is in.
            tmpPlaylist.add(line);
            line = in.readLine();
            lineCount++;
         }

         in.close();

         if (0 == tmpPlaylist.size())
         {
            utils.errorLog (this, "no songs found in playlist file " + filename);
            return;
         }

         playlist          = tmpPlaylist;
         playlistPos       = 0;
         playlistDirectory = playlistFile.getParent();

         // Activity only sends filenames that end in .m3u; strip that
         playlistFilename  = playlistFile.getName();
         playlistFilename  = playlistFilename.substring(0, playlistFilename.length() - 4);

         play(playlist.get(playlistPos));
      }
      catch (java.io.FileNotFoundException e)
      {
         utils.errorLog (this, "playlist file not found: " + filename);
      }
      catch (java.io.IOException e)
      {
         utils.errorLog (this, "error reading playlist file: "  + filename + ": " + e.toString());
      }
      catch (RuntimeException e)
      {
         utils.errorLog (this, "playList: " + e.toString());
      }
   }

   private void previous()
   {
      final long currentPos = mediaPlayer.getCurrentPosition();

      stop();

      if (currentPos > PREV_THRESHOLD)
      {
         // not near beginning of current track; move to beginning
         mediaPlayer.seekTo(0);
      }
      else if (playlistPos > 0)
      {
         playlistPos--;
      }
      else
      {
         // at start of playlist; wrap
         playlistPos = playlist.size() - 1;
      }

      play(playlist.get(playlistPos));
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
         utils.errorLog (this, "can't get audio focus");
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
            else if (action.equals (utils.ACTION_NEXT))
            {
               next();
            }
            else if (action.equals (utils.ACTION_PAUSE))
            {
               pause(PlayState.Paused);
            }
            else if (action.equals (utils.ACTION_PLAYLIST))
            {
               playList(intent.getStringExtra("playlist"));
            }
            else if (action.equals (utils.ACTION_PREVIOUS))
            {
               previous();
            }
            else if (action.equals (utils.ACTION_SEEK))
            {
               // FIXME: implement
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
            else if (action.equals (utils.ACTION_UPDATE_DISPLAY))
            {
               notifyChange(utils.META_CHANGED);
               notifyChange (utils.PLAYSTATE_CHANGED);
            }
            else
            {
               utils.errorLog(service.this, "broadcastReceiver.onReceive: unkown action: " + action);
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
                utils.errorLog (service.this, "Unknown onAudioFocusChange code " + focusChange);
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
            utils.errorLog(service.this, "MediaPlayer server died: " + what + "," + extra);
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
   private String           playlistDirectory;
   // Absolute path to directory where playlist files reside. The
   // list of available playlists consists of all .m3u files in
   // this directory.
   private String           currentFile;
   // relative to playlistDirectory. null if no file is current.

   private String playlistFilename;
   // Relative to Playlist_Directory, without extension (suitable
   // for user display). null if no playlist is current.

   java.util.LinkedList<String> playlist;
   int                          playlistPos;
   // Current position in PlayList (0 indexed; -1 if none)

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

      playlist    = new LinkedList<String>();
      playlistPos = -1;
      playing     = PlayState.Paused;

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
      if (intent.getAction() != null)
      {
         utils.errorLog(this, "onStartCommand got unexpected intent: " + intent);
      }
      return START_STICKY;
    }

   @Override protected void dump(FileDescriptor fd, PrintWriter writer, String[] args)
   {
      writer.println("playlist   :" + playlistFullPath());
      writer.println("currentFile:" + currentFile);

      if (playlist.size() > 0)
      {
         for (int i = 0; i < playlist.size(); i++)
         {
            writer.println(i + ": " + playlist.get(i));
         }
      }
      utils.debugDump(writer);
    }

}
