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
import android.content.SharedPreferences.Editor;
import android.content.SharedPreferences;
import android.media.AudioManager.OnAudioFocusChangeListener;
import android.media.AudioManager;
import android.media.MediaMetadataRetriever;
import android.media.MediaPlayer;
import android.os.Environment;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.os.PowerManager.WakeLock;
import android.os.PowerManager;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.Integer;
import java.util.LinkedList;

public class service extends Service
{
   // used to specify whether enqueue() should start playing
   // the new list of files right away, next or once all the currently
   // queued files have been played

   //  Internal Messages used for delays
   private static final int UNPAUSE        = 3;
   private static final int UPDATE_DISPLAY = 4;

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
   }

   private String playlistFullPath()
   // return current playlist file abs path
   {
      return playlistDirectory + "/" + playlistFilename + ".m3u";
   }

   private void next()
   {
      stop();

      // Not clear how we get here with playlist empty, but it seems
      // to have happened once; onCompletionListener was called before
      // anything started playing.
      if (playlist.size() == 0)
      {
         playlistPos        = -1;
         highestPlaylistPos = playlistPos;
      }
      else
      {
         if (playlistPos < playlist.size() - 1)
         {
            playlistPos++;
            if (playlistPos > highestPlaylistPos)
            {
               highestPlaylistPos = playlistPos;
            }
         }
         else
         {
            // at end of playlist; wrap
            playlistPos = 0;
         };

         play(playlist.get(playlistPos), 0);
      }
   }

   private void notifyChange(String what)
   {
      // Notify the activity that something has changed.
      //
      // 'What' must be one of the *_CHANGED constants in utils.java

      if (what.equals(utils.META_CHANGED))
      {
         if (playing == PlayState.Idle)
         {
            sendStickyBroadcast
               (new Intent (utils.META_CHANGED).
                putExtra ("artist", "").
                putExtra ("album", "").
                putExtra ("track", "").
                putExtra ("duration", "").
                putExtra ("playlist", getResources().getString(R.string.null_playlist)));
         }
         else
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
                   putExtra
                   ("duration", new Integer(retriever.extractMetadata (MediaMetadataRetriever.METADATA_KEY_DURATION))).
                   putExtra
                   ("playlist",
                    playlistFilename + " " + playlistPos + " / " + playlist.size()));
            }
            catch (RuntimeException e)
            {
               utils.errorLog (this, "notifyChange SetDataSource (" + absFile + "): " + e);
            };
         }
      }
      else if (what.equals(utils.PLAYSTATE_CHANGED))
      {
         sendStickyBroadcast
            (new Intent (utils.PLAYSTATE_CHANGED).
             putExtra ("playing", playing == PlayState.Playing).
             putExtra ("position", mediaPlayer.getCurrentPosition()));
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
         utils.verboseLog("pause while playing");
         mediaPlayer.pause();

         if (pausedState == PlayState.Paused)
         {
            // don't abandon for Paused_Transient
            audioManager.abandonAudioFocus(audioFocusListener);
         }

         playing = pausedState;

         notifyChange(utils.PLAYSTATE_CHANGED);
         handler.removeMessages(UPDATE_DISPLAY);
      }
      else
      {
         utils.verboseLog("pause while not playing");
      }
   }

   private void play (final String path, final int pos)
   {
      // path must be relative to playlistDirectory

      // service.playing must be Idle (call Stop first)
      //
      // FIXME: merge stop here to avoid double notification on next, prev?

      try
      {
         final String absFile = playlistDirectory + "/" + path;

         mediaPlayer.reset();
         playing = PlayState.Idle;

         try
         {
            mediaPlayer.setDataSource (absFile);
            mediaPlayer.prepare();
         }
         catch (IOException e)
         {
            // From SetDataSource
            currentFile = null;

            // We consider this a programmer error, because it
            // probably indicates an SMM sync bug. It could also be a
            // failing sdcard.
            utils.errorLog(service.this, "can't play " + path, e);

            notifyChange(utils.META_CHANGED);
            return;
         }

         currentFile = path;

         unpause();

         notifyChange(utils.META_CHANGED);

         if (pos != 0)
         {
            mediaPlayer.seekTo(pos);
            notifyChange(utils.PLAYSTATE_CHANGED);
         }
      }
      catch (RuntimeException e)
      {
         utils.errorLog (this, "play failed", e);
      }
   }

   class Fail extends RuntimeException {}

   private void playList(final String filename, String currentFile, int pos, PlayState newState)
      throws Fail
   {
      // Start playing playlist 'filename' (absolute path).
      //
      // If 'currentFile' (relative to 'filename' directory) is null,
      // lookup the current file for the playlist from
      // SharedPreferences. If non-null, start at that file.

      final File playlistFile        = new File(filename);
      String     tmpPlaylistFilename = playlistFile.getName();

      // Activity only sends filenames that end in .m3u; strip that
      tmpPlaylistFilename  = tmpPlaylistFilename.substring(0, tmpPlaylistFilename.length() - 4);

      if (currentFile == null)
      {
         SharedPreferences storage = getSharedPreferences(utils.serviceClassName, MODE_PRIVATE);

         currentFile = storage.getString(tmpPlaylistFilename + keyCurrentFile, null);

         pos = storage.getInt(tmpPlaylistFilename + keyCurrentPos, 0);
      }

      if (!playlistFile.canRead())
      {
         // This is an SMM error, or failing sdcard
         utils.errorLog(this, "can't read " + filename);
         return;
      }

      try
      {
         BufferedReader               in          = new BufferedReader (new FileReader (filename));
         String                       line        = in.readLine();
         java.util.LinkedList<String> tmpPlaylist = new LinkedList<String>();
         int                          lineCount   = 0;
         int                          startAt     = 0;

         while (line != null)
         {
            // We don't check for readable now, because that might
            // change by the time we get to actually playing a song.
            //
            // In SMM playlists all lines are song filepaths, relative
            // to the directory filename is in.

            if (currentFile != null && startAt == 0 && line.equals(currentFile))
            {
               // If 'currentFile' is in the playlist multiple times,
               // this finds the first one. That's a bug in SMM, so it
               // doesn't much matter what we do; this ensures that
               // all of the playlist is played at least once.
               startAt = lineCount;
            }

            tmpPlaylist.add(line);
            line = in.readLine();
            lineCount++;
         }

         in.close();

         if (0 == tmpPlaylist.size())
         {
            utils.errorLog (this, "no songs found in playlist file " + filename);
            throw new Fail();
         }

         // User will want to resume the current playlist at some point.
         saveState();

         playlist          = tmpPlaylist;
         playlistPos       = startAt;
         playlistDirectory = playlistFile.getParent();
         playlistFilename  = tmpPlaylistFilename;

         highestPlaylistPos = playlistPos;
         // There could be a higher song in the smm file, but we'll
         // ignore that until it becomes a problem.

         switch (newState)
         {
         case Idle:
            playing = PlayState.Idle;
            break;

         case Playing:
            play(playlist.get(playlistPos), pos);
            break;

         case Paused:
            pause (PlayState.Paused);
            break;

         case Paused_Transient:
            pause (PlayState.Paused_Transient);
            break;
         }

      }
      catch (Fail e)
      {
         throw e;
      }
      catch (java.io.FileNotFoundException e)
      {
         utils.errorLog (this, "playlist file not found: " + filename, e);
         throw new Fail();
      }
      catch (java.io.IOException e)
      {
         utils.errorLog (this, "error reading playlist file: "  + filename, e);
         throw new Fail();
      }
      catch (RuntimeException e)
      {
         utils.errorLog (this, "playList failed", e);
         throw new Fail();
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
         notifyChange(utils.PLAYSTATE_CHANGED);
      }
      else if (playlistPos > 0)
      {
         playlistPos--;
         play(playlist.get(playlistPos), 0);
      }
      else
      {
         // at start of playlist; indicate that to the user by not playing
      }
   }

   // save/restore keys; global
   private static final String keyPlaying           = "playing";
   private static final String keyPlaylistDirectory = "playlistDirectory";
   private static final String keyPlaylistFilename  = "playlistFilename";

   // per-playlist: actual key is prefixed by the playlist filename
   // (sans directory, sans extension)
   private static final String keyCurrentFile = "_currentFile";
   private static final String keyCurrentPos  = "_currentPos";

   private void restoreState()
   {
      if (!Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState()))
      {
         utils.infoLog(this, "external storage not mounted; can't restore");

         playing           = PlayState.Idle;
         playlistDirectory = null;
         playlistFilename  = null;
         currentFile       = null;

         return;
      }

      // External storage may have changed since saveState() was
      // called. In particular, we assume SMM has edited the playlist
      // files and the SMM file. So we don't store playlistPos; we
      // search for the current file in the playlist.

      SharedPreferences storage = getSharedPreferences(utils.serviceClassName, MODE_PRIVATE);

      playing           = PlayState.valueOf(storage.getString(keyPlaying, PlayState.Paused.toString()));
      playlistDirectory = storage.getString(keyPlaylistDirectory, null);
      playlistFilename  = storage.getString(keyPlaylistFilename, null);
      currentFile       = storage.getString(playlistFilename + keyCurrentFile, null);

      utils.verboseLog
         ("restoreState: " + playing + ", " +
          playlistDirectory + ", " + playlistFilename + ", " + currentFile);

      if (playlistDirectory != null && playlistFilename != null)
      {
         try
         {
            playList
               (playlistDirectory + "/" + playlistFilename + ".m3u",
                currentFile,
                storage.getInt(playlistFilename + keyCurrentPos, 0),
                playing);
         }
         catch (Fail e)
         {
            playing           = PlayState.Idle;
            playlistDirectory = null;
            playlistFilename  = null;
            currentFile       = null;
         }
      }
      else
      {
         playing           = PlayState.Idle;
         playlistDirectory = null;
         playlistFilename  = null;
         currentFile       = null;

         notifyChange(utils.META_CHANGED);
         notifyChange(utils.PLAYSTATE_CHANGED);
      }
   }

   private void saveState()
   {
      if (!Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState()))
      {
         utils.infoLog(this, "external storage not mounted; can't save");
         return;
      }

      SharedPreferences storage = getSharedPreferences(utils.serviceClassName, MODE_PRIVATE);
      Editor            editor  = storage.edit();

      editor.putString(keyPlaying, playing.toString());
      editor.putString(keyPlaylistDirectory, playlistDirectory);
      editor.putString(keyPlaylistFilename, playlistFilename);
      editor.putString(playlistFilename + keyCurrentFile, currentFile);
      editor.putInt
         (playlistFilename + keyCurrentPos,
          (playing == PlayState.Idle) ? 0 : mediaPlayer.getCurrentPosition());

      editor.commit();

      writeSMMFile();
   }

   private void stop()
   {
      if (currentFile != null)
      {
         mediaPlayer.reset();

         currentFile = null;
         playing     = PlayState.Idle;

         notifyChange(utils.PLAYSTATE_CHANGED);
         handler.removeMessages(UPDATE_DISPLAY);
      };
   }

   private void unpause()
   {
      if (!haveAudioFocus)
      {
         final int result = audioManager.requestAudioFocus
            (audioFocusListener,
             android.media.AudioManager.STREAM_MUSIC,
             android.media.AudioManager.AUDIOFOCUS_GAIN);

         if (result != android.media.AudioManager.AUDIOFOCUS_REQUEST_GRANTED)
         {
            utils.errorLog (this, "can't get audio focus");
            return;
         }
      }

      mediaPlayer.start();

      playing = PlayState.Playing;
      notifyChange (utils.PLAYSTATE_CHANGED);

      handler.sendEmptyMessageDelayed(UPDATE_DISPLAY, 1000);
   }

   private void writeSMMFile()
   {
      // Tell smm what tracks from the current playlist have been
      // played and can therefore be deleted.
      //
      // Which we can do by writing the filename of the previous
      // track, which has been completed.

      if (playlistFilename == null)
         return;

      final String smmFileName = playlistDirectory + "/" + playlistFilename + ".last";

      try
      {
         BufferedWriter writer = new BufferedWriter(new FileWriter(smmFileName));

         if (highestPlaylistPos < 1)
         {
            // 0 or -1; none played yet; write empty file
         }
         else
         {
            writer.write(playlist.get(highestPlaylistPos - 1));
         }
         writer.newLine();
         writer.close();
      }
      catch (IOException e)
      {
         utils.errorLog(this, "can't write smm file: " + e);
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "writeSMMFile: " + e);
      }
   }

   public service() {}

   ////////// nested classes

   private Handler handler = new Handler()
      {
         @Override public void handleMessage(Message msg)
         {
            switch (msg.what)
            {
            case UNPAUSE:
               utils.debugLog("handler: UNPAUSE");
               unpause();

            case UPDATE_DISPLAY:
               notifyChange(utils.PLAYSTATE_CHANGED);
               handler.sendEmptyMessageDelayed(UPDATE_DISPLAY, 1000);

            default:
               break;
            }
         }
      };

   private BroadcastReceiver broadcastReceiverFile = new BroadcastReceiver()
      {
         @Override public void onReceive(Context context, Intent intent)
         {
            final String action = intent.getAction();

            if (action.equals (android.content.Intent.ACTION_MEDIA_EJECT))
            {
               // External storage is being unmounted, probably so smm
               // can manage the playlists. Save state now, since
               // restoreState has the logic for processing smm
               // changes.
               saveState();
               pause(PlayState.Paused);
            }
            else if (action.equals (android.content.Intent.ACTION_MEDIA_MOUNTED))
            {
               // External storage is being mounted, probably after smm
               // managed the playlists.

               restoreState();
            }
            else
            {
               utils.errorLog(service.this, "broadcastReceiverFile.onReceive: unknown action: " + action);
            }
         }
      };

   private BroadcastReceiver broadcastReceiverCommand = new BroadcastReceiver()
      {
         @Override public void onReceive(Context context, Intent intent)
         {
            final String action = intent.getAction();

            if (action.equals (utils.ACTION_COMMAND))
            {
               final String command = intent.getStringExtra("command");

               utils.verboseLog(command);

               if (command.equals(utils.COMMAND_DUMP_LOG))
               {
                  dumpLog();
               }
               else if (command.equals (utils.COMMAND_NEXT))
               {
                  next();
               }
               else if (command.equals (utils.COMMAND_PAUSE))
               {
                  pause(PlayState.Paused);
               }
               else if (command.equals (utils.COMMAND_PLAYLIST))
               {
                  try
                  {
                     playList(intent.getStringExtra("playlist"), null, 0, PlayState.Playing);
                  }
                  catch (Fail e)
                  {
                     // nothing to do here.
                  }
               }
               else if (command.equals (utils.COMMAND_PREVIOUS))
               {
                  previous();
               }
               else if (command.equals (utils.COMMAND_SEEK))
               {
                  final int pos = intent.getIntExtra("position", 0);
                  mediaPlayer.seekTo(pos);
                  notifyChange(utils.PLAYSTATE_CHANGED);
               }
               else if (command.equals (utils.COMMAND_TOGGLEPAUSE))
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
                     // user wants to override
                     unpause();
                     break;

                  };
               }
               else if (command.equals (utils.COMMAND_UPDATE_DISPLAY))
               {
                  notifyChange(utils.META_CHANGED);
                  notifyChange (utils.PLAYSTATE_CHANGED);
               }
               else
               {
                  utils.errorLog(service.this, "broadcastReceiverCommand.onReceive: unknown command: " + command);
               }
            }
            else
            {
               utils.errorLog(service.this, "broadcastReceiverCommand.onReceive: unknown action: " + action);
            }
         }
      };

    private OnAudioFocusChangeListener audioFocusListener = new OnAudioFocusChangeListener()
       {
          public void onAudioFocusChange(int focusChange)
          {
             utils.verboseLog
                ("onAudioFocusChange focusChange => " + focusChange +
                 ", haveAudioFocus => " + haveAudioFocus +
                 ", playing => " + playing);

             if (focusChange == AudioManager.AUDIOFOCUS_LOSS)
             {
                haveAudioFocus = false;
                pause(PlayState.Paused);
             }
             else if (focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK ||
                      focusChange == AudioManager.AUDIOFOCUS_LOSS_TRANSIENT)
             {
                haveAudioFocus = false;

                pause(PlayState.Paused_Transient);
             }
             else if (focusChange == AudioManager.AUDIOFOCUS_GAIN)
             {
                haveAudioFocus = true;

                switch (playing)
                {
                case Idle:
                   break;

                case Playing:
                   break;

                case Paused:
                   unpause();

                case Paused_Transient:
                   // Most likely after a Navigator message; give
                   // listener time to process it.
                   utils.debugLog("onAudioFocusChange: Paused_Transient AUDIOFOCUS_GAIN");
                   handler.sendEmptyMessageDelayed(UNPAUSE, 1000);
                }
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
            utils.verboseLog("MediaPlayer onError: " + what + "," + extra);

            switch (what)
            {
            case MediaPlayer.MEDIA_ERROR_SERVER_DIED: // = 100
               utils.verboseLog("recreating MediaPlayer");

               mediaPlayer.release();
               createMediaPlayer();
               // Since we don't know why it died, just trying again seems
               // problematic, but it is the most user friendly if it
               // works. This will _not_ be easy to debug!
               if (service.playing == PlayState.Playing)
               {
                  play (currentFile, 0);
               };

               return true;

            default:
               utils.verboseLog("unknown MediaPlayer error code");
               // onCompletion will _not_ be called
               return true;
            }
         }
      };

   private AudioManager audioManager;

   private MediaPlayer mediaPlayer;

   ////////// state
   static private PlayState playing;
   private String           playlistDirectory;
   // Absolute path to directory where playlist files reside. The
   // list of available playlists consists of all .m3u files in
   // this directory.

   private String playlistFilename;
   // Relative to Playlist_Directory, without extension (suitable
   // for user display). null if no playlist is current.

   private String currentFile;
   // File name of currently playing track, relative to
   // playlistDirectory. null if no file is current.

   java.util.LinkedList<String> playlist;
   int                          playlistPos;
   // Current position in PlayList (0 indexed; -1 if none)

   int highestPlaylistPos;
   // highest position played on this playlist; for writeSMMFile, to
   // handle wrap and prev.

   boolean haveAudioFocus;

   ////////// service lifetime methods
   @Override public IBinder onBind(Intent intent)
   {
      return null;
   }

   @Override public void onCreate()
   {
      super.onCreate();

      // We need two broadcast recievers because we can't wild card
      // all of the filter criteria.
      IntentFilter filter = new IntentFilter();
      filter.addAction(Intent.ACTION_MEDIA_EJECT);
      filter.addAction(Intent.ACTION_MEDIA_MOUNTED);
      filter.addDataScheme("file");
      registerReceiver(broadcastReceiverFile, filter);

      filter = new IntentFilter();
      filter.addAction(utils.ACTION_COMMAND);
      registerReceiver(broadcastReceiverCommand, filter);

      createMediaPlayer();

      audioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);

      playlist           = new LinkedList<String>();
      playlistPos        = -1;
      highestPlaylistPos = playlistPos;
      playing            = PlayState.Paused;
      haveAudioFocus     = false;

      restoreState();
   }

   @Override public void onDestroy()
   {
      utils.verboseLog("onDestroy");

      saveState();

      mediaPlayer.reset();
      mediaPlayer.release();
      mediaPlayer = null;

      audioManager.abandonAudioFocus(audioFocusListener);

      handler.removeCallbacksAndMessages(null);

      unregisterReceiver(broadcastReceiverFile);
      unregisterReceiver(broadcastReceiverCommand);

      super.onDestroy();
   }

   @Override public int onStartCommand(Intent intent, int flags, int startId)
   {
      if (intent == null)
      {
         // intent is null if the service is restarted by Android
         // after a crash.
         utils.verboseLog("onStartCommand null intent");
      }
      else if (intent.getAction() != null)
      {
         utils.errorLog(this, "onStartCommand got unexpected intent: " + intent);
      }
      return START_STICKY;
    }

   private void dumpLog()
   {
      final String logFilename = playlistDirectory + "/" + playlistFilename + ".log";

      try
      {
         PrintWriter writer = new PrintWriter(new FileWriter(logFilename));

         dump(null, writer, null);
         utils.debugClear();
         writer.close();
         utils.infoLog(this, "log written to " + logFilename);
      }
      catch (java.io.IOException e)
      {
         utils.errorLog(this, "can't write log to " + logFilename);
      }
   }

   @Override protected void dump(FileDescriptor fd, PrintWriter writer, String[] args)
   {
      writer.println("playing            : " + playing);
      writer.println("playlistDirectory  : " + playlistDirectory);
      writer.println("playlistFilename   : " + playlistFilename);
      writer.println("playlist size      : " + playlist.size());
      writer.println("currentFile        : " + currentFile);
      writer.println("playlistPos        : " + playlistPos);
      writer.println("highestPlaylistPos : " + highestPlaylistPos);

      SharedPreferences storage = getSharedPreferences(utils.serviceClassName, MODE_PRIVATE);

      writer.println("storage." + keyPlaying + ": " + storage.getString(keyPlaying, ""));
      writer.println("storage." + keyPlaylistDirectory + ": " + storage.getString(keyPlaylistDirectory, ""));
      writer.println("storage." + keyPlaylistFilename + ": " + storage.getString(keyPlaylistFilename, ""));

      // FIXME: figure out how to dump all of storage
      writer.println("storage.vocal" + keyCurrentFile + ": " +
                     storage.getString("vocal" + keyCurrentFile, ""));
      writer.println("storage.vocal" + keyCurrentPos + ": " +
                     storage.getInt("vocal" + keyCurrentPos, 0));
      writer.println("storage.instrumental" + keyCurrentFile + ": " +
                     storage.getString("instrumental" + keyCurrentFile, ""));
      writer.println("storage.instrumental" + keyCurrentPos + ": " +
                     storage.getInt("instrumental" + keyCurrentPos, 0));

      utils.debugDump(writer);

      utils.debugClear();
    }
}
