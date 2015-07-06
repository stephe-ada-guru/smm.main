//  Abstract :
//
//  Provides background audio playback capabilities, allowing the
//  user to switch between activities without stopping playback.
//
//  Copyright (C) 2011 - 2013, 2015 Stephen Leake.  All Rights Reserved.
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

import android.app.PendingIntent;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences.Editor;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.media.AudioManager.OnAudioFocusChangeListener;
import android.media.AudioManager;
import android.media.MediaMetadataRetriever;
import android.media.MediaPlayer;
import android.media.RemoteControlClient;
import android.os.Environment;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.os.PowerManager.WakeLock;
import android.os.PowerManager;
import android.preference.PreferenceManager;
import android.view.KeyEvent;
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
   //  Internal Messages used for delays
   private static final int UNPAUSE        = 3;
   private static final int UPDATE_DISPLAY = 4;

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
      // Not clear how we get here with playlist empty, but it seems
      // to have happened once; onCompletionListener was called before
      // anything started playing.
      if (playlist.size() == 0)
      {
         playlistPos        = -1;
         highestPlaylistPos = playlistPos;
         stop();
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
            play(playlist.get(playlistPos), 0);
         }
         else
         {
            // at end of playlist; don't wrap, because then we just
            // end up repeating the whole playlist. There isn't a good
            // way to indicate we've wrapped; it looks like there is
            // lots of music left, so we don't sync.
            highestPlaylistPos = playlistPos;
            stop();
         };
      }

      // for some crashes, onDestroy is not called, so we don't
      // saveState properly. So do it here.
      saveState();
   }

   private void notifyChange(String what)
   {
      // Notify the activity and the remote control that something has changed.
      //
      // 'What' must be one of the *_CHANGED constants in utils.java

      if (what.equals(utils.META_CHANGED))
      {
         if (playing == PlayState.Idle)
         {
            if (playlistDirectory == null)
            {
               sendStickyBroadcast
                  (new Intent (utils.META_CHANGED).
                   putExtra ("artist", "").
                   putExtra ("album", "").
                   putExtra ("track", "").
                   putExtra ("duration", 0).
                   putExtra ("playlist", getResources().getString(R.string.null_playlist_directory)));

            }
            else
            {
               sendStickyBroadcast
                  (new Intent (utils.META_CHANGED).
                   putExtra ("artist", "").
                   putExtra ("album", "").
                   putExtra ("track", "").
                   putExtra ("duration", 0).
                   putExtra ("playlist", getResources().getString(R.string.null_playlist)));
            }

            remoteControlClient.editMetadata(true)
                  .putString(MediaMetadataRetriever.METADATA_KEY_TITLE, "")
                  .putString(MediaMetadataRetriever.METADATA_KEY_ALBUM, "")
                  .putString(MediaMetadataRetriever.METADATA_KEY_ARTIST, "")
                  .putLong(MediaMetadataRetriever.METADATA_KEY_DURATION, 0)
                  .apply();
         }
         else
         {
            final String absFile = playlistDirectory + "/" + playlist.get(playlistPos);

            final MetaData retriever = new MetaData(this, playlistFilename, absFile);

            sendStickyBroadcast
               (new Intent (utils.META_CHANGED).
                putExtra ("artist", retriever.artist).
                putExtra ("album", retriever.album).
                putExtra ("track", retriever.title).
                putExtra ("duration", retriever.duration).
                putExtra
                ("playlist",
                 playlistFilename + " " + (playlistPos + 1) + " / " + playlist.size()));

            remoteControlClient.editMetadata(true)
               .putString(MediaMetadataRetriever.METADATA_KEY_TITLE, retriever.title)
               .putString(MediaMetadataRetriever.METADATA_KEY_ALBUM, retriever.album)
               .putString(MediaMetadataRetriever.METADATA_KEY_ARTIST, retriever.artist)
               .putLong(MediaMetadataRetriever.METADATA_KEY_DURATION, Integer.parseInt(retriever.duration))
               .apply();
         }
      }
      else if (what.equals(utils.PLAYSTATE_CHANGED))
      {
         sendStickyBroadcast
            (new Intent (utils.PLAYSTATE_CHANGED).
             putExtra ("playing", playing == PlayState.Playing).
             putExtra ("position", mediaPlayer.getCurrentPosition()));

         switch (playing)
         {
         case Idle:
            remoteControlClient.setPlaybackState
               (remoteControlClient.PLAYSTATE_STOPPED, mediaPlayer.getCurrentPosition(), 1.0f);
            break;

         case Playing:
            remoteControlClient.setPlaybackState
               (remoteControlClient.PLAYSTATE_PLAYING, mediaPlayer.getCurrentPosition(), 1.0f);
            break;

         case Paused:
         case Paused_Transient:
            remoteControlClient.setPlaybackState
               (remoteControlClient.PLAYSTATE_PAUSED, mediaPlayer.getCurrentPosition(), 1.0f);
            break;
         }
      }
      else
      {
         utils.debugLog("notifyChange: unexpected 'what'");
      };
   }

   private void pause(PlayState pausedState)
   {
      if (playing == PlayState.Playing)
      {
         mediaPlayer.pause();

         if (pausedState == PlayState.Paused)
         {
            // don't abandon for Paused_Transient
            audioManager.abandonAudioFocus(audioFocusListener);
         }

         playing = pausedState;

         notifyChange(utils.PLAYSTATE_CHANGED);
         handler.removeMessages(UPDATE_DISPLAY);

         // for some crashes, onDestroy is not called, so we don't
         // saveState properly. So do it here.
         saveState();
      }
      else
      {
         utils.verboseLog("pause while not playing");
      }
   }

   private void play (final String path, final int pos)
   {
      // path must be relative to playlistDirectory

      utils.verboseLog("play " + path + "; at " + pos);

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

            // We consider this a programmer error, because it
            // probably indicates an SMM sync bug. It could also be a
            // failing sdcard.
            utils.debugLog("can't play '" + path + "' :"+ e.toString());

            notifyChange(utils.META_CHANGED);
            return;
         }

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
         utils.debugLog("play failed" + e.toString());
      }
   }

   class Fail extends RuntimeException {}

   private void playList(final String filename, PlayState newState)
      throws Fail
   {
      // Start playing playlist 'filename' (absolute path).
      //
      // Lookup the current file for the playlist from
      // SharedPreferences. If non-null, start at that file.

      final File playlistFile = new File(filename);
      if (!playlistFile.canRead())
      {
         // This is an SMM error, or failing sdcard
         utils.debugLog("can't read " + filename);
         throw new Fail();
      }

      // There are two use cases where the playlist file absolute path
      // changes, but we want to treat it as the same file:
      //
      // 1) root directory moved on a phone
      //    - external sdcard to internal storage
      //
      // 2) USB stick moved to a different device
      //    - Google TV to Neo
      //    - back to Google TV in different USB port
      //
      // getName returns just the basename of a file, throwing away
      // the absolute path.
      String tmpPlaylistFilename = playlistFile.getName();

      // Activity only sends filenames that end in .m3u; strip that
      tmpPlaylistFilename  = tmpPlaylistFilename.substring(0, tmpPlaylistFilename.length() - 4);

      SharedPreferences storage = getSharedPreferences(utils.serviceClassName, MODE_PRIVATE);

      // This fails in case 2); use the .last smm file
      String currentFile = storage.getString(tmpPlaylistFilename + keyCurrentFile, null);
      int          pos         = 0;

      if (currentFile == null)
      {
         final String smmFileName = playlistFile.getParent() + "/" + tmpPlaylistFilename + ".last";

         try
         {
            BufferedReader reader = new BufferedReader(new FileReader(smmFileName));

            currentFile = reader.readLine();
            reader.close();
         }
         catch (IOException e)
         {
            utils.debugLog("can't read smm file: " + e);
         }
      }

      utils.debugLog("start file: " + currentFile);

      try
      {
         BufferedReader               in          = new BufferedReader (new FileReader (playlistFile));
         String                       line        = in.readLine();
         java.util.LinkedList<String> tmpPlaylist = new LinkedList<String>();
         int                          lineCount   = 0;
         int                          startAt     = -1;

         while (line != null)
         {
            // We don't check for .mp3 readable now, because that
            // might change by the time we get to actually playing a
            // song.
            //
            // In SMM playlists all lines are song filepaths, relative
            // to the directory filename is in.

            if (currentFile != null && line.equals(currentFile))
            {
               if (startAt == -1)
               {
                  // If 'currentFile' is in the playlist multiple times,
                  // this finds the first one. That's a bug in SMM, so it
                  // doesn't much matter what we do; this ensures that
                  // all of the playlist is played at least once.
                  startAt = lineCount;
                  pos     = storage.getInt(tmpPlaylistFilename + keyCurrentPos, 0);
               }
               else
               {
                  utils.debugLog(currentFile + " found more than once in playlist");
               }
            }

            tmpPlaylist.add(line);
            line = in.readLine();
            lineCount++;
         }

         in.close();

         if (0 == tmpPlaylist.size())
         {
            utils.debugLog("no songs found in playlist file " + filename);
            throw new Fail();
         }

         if (-1 == startAt)
         {
            startAt = 0;
         }

         playlistDirectory = playlistFile.getParent();
         playlistFilename  = tmpPlaylistFilename;
         playlist          = tmpPlaylist;
         playlistPos       = startAt;

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
         case Paused_Transient:
            if (playing == PlayState.Idle)
            {
               // can't go directly from Idle to Paused
               play(playlist.get(playlistPos), pos);
            }
            pause (newState);
            break;
         }

      }
      catch (Fail e)
      {
         throw e;
      }
      catch (java.io.FileNotFoundException e)
      {
         utils.debugLog("playlist file not found: " + filename + e.toString());
         throw new Fail();
      }
      catch (java.io.IOException e)
      {
         utils.debugLog("error reading playlist file: "  + filename + e.toString());
         throw new Fail();
      }
      catch (RuntimeException e)
      {
         utils.debugLog("playList failed" + e.toString());
         throw new Fail();
      }
   }

   private void previous()
   {
      // FIXME: get in onCreate, then again only when it changes
      Resources         res   = getResources();
      SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
      final int prevThreshold = Integer.valueOf
         (prefs.getString
          (res.getString(R.string.prev_threshold_key),
           res.getString(R.string.prev_threshold_default)))
         .intValue();

      final int currentPos = mediaPlayer.getCurrentPosition();

      if (currentPos > prevThreshold)
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
         stop();
      }
   }

   // save/restore keys; global
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

         setIdleNull();
         return;
      }

      // External storage may have changed since saveState() was
      // called. In particular, we assume SMM has edited the playlist
      // files and the SMM file. So we don't store playlistPos; we
      // search for the current file in the playlist.

      SharedPreferences storage = getSharedPreferences(utils.serviceClassName, MODE_PRIVATE);

      playlistDirectory = storage.getString(keyPlaylistDirectory, null);
      playlistFilename  = storage.getString(keyPlaylistFilename, null);

      utils.verboseLog("restoreState: " + playlistDirectory + ", " + playlistFilename);

      if (playlistDirectory != null && playlistFilename != null)
      {
         try
         {
            playList (playlistDirectory + "/" + playlistFilename + ".m3u", PlayState.Paused);
         }
         catch (Fail e)
         {
            setIdleNull();

            notifyChange(utils.META_CHANGED);
            notifyChange(utils.PLAYSTATE_CHANGED);
         }
      }
      else
      {
         setIdleNull();

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

      editor.putString(keyPlaylistDirectory, playlistDirectory);
      editor.putString(keyPlaylistFilename, playlistFilename);

      editor.putString
         (playlistFilename + keyCurrentFile,
          (playlistPos == -1) ? null : playlist.get(playlistPos));

      editor.putInt
         (playlistFilename + keyCurrentPos,
          (playing == PlayState.Idle) ? 0 : mediaPlayer.getCurrentPosition());

      editor.commit();

      writeSMMFile();
   }

   private void setIdleNull()
   {
      playing           = PlayState.Idle;
      playlistDirectory = null;
      playlistFilename  = null;
      playlist.clear();
      playlistPos       = -1;
   }

   private void stop()
   {
      mediaPlayer.reset();

      playing = PlayState.Idle;

      notifyChange(utils.PLAYSTATE_CHANGED);
      handler.removeMessages(UPDATE_DISPLAY);
   }

   private void unpause()
   {
      if (!haveAudioFocus)
      {
         utils.verboseLog("unpause requestAudioFocus");

         final int result = audioManager.requestAudioFocus
            (audioFocusListener,
             android.media.AudioManager.STREAM_MUSIC,
             android.media.AudioManager.AUDIOFOCUS_GAIN);

         if (result != android.media.AudioManager.AUDIOFOCUS_REQUEST_GRANTED)
         {
            utils.debugLog("can't get audio focus");
            return;
         }
      }
      else
      {
         utils.verboseLog("unpause haveAudioFocus");
      }

      mediaPlayer.start();

      playing = PlayState.Playing;
      notifyChange (utils.PLAYSTATE_CHANGED);

      handler.sendEmptyMessageDelayed(UPDATE_DISPLAY, 1000);
   }

   private void writeNote(String note)
   {
      if (playlistPos > -1)
      {
         final String noteFileName = smmDirectory + "/" + playlistFilename + ".note";

         try
         {
            BufferedWriter writer = new BufferedWriter(new FileWriter(noteFileName, true)); // append

            writer.write('"' + playlist.get(playlistPos) + '"' + ' ' + note);
            writer.newLine();
            writer.close();
         }
         catch (IOException e)
         {
            utils.debugLog("can't write note file: " + e);
         }
         catch (RuntimeException e)
         {
            utils.debugLog("writeNote: " + e);
         }
      }
   }

   private void setSMMDirectory(Context context)
   {
      Resources         res   = getResources();
      SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
      smmDirectory = prefs.getString
         (res.getString(R.string.smm_directory_key),
          res.getString(R.string.smm_directory_default));
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

      final String smmFileName = smmDirectory + "/" + playlistFilename + ".last";

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
         utils.errorLog(this, "can't write smm file: ", e);
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "writeSMMFile: ", e);
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
               utils.verboseLog("service handler: UNPAUSE");
               unpause();

            case UPDATE_DISPLAY:
               notifyChange(utils.PLAYSTATE_CHANGED);
               utils.verboseLog("service handler: UPDATE_DISPLAY");

               handler.sendEmptyMessageDelayed(UPDATE_DISPLAY, 1000);

            default:
               break;
            }
         }
      };

   // Need a named class for RemoteControlClient.registerMediaButtonEventReceiver
   private class SmBroadcastReceiverButton extends BroadcastReceiver
   {
      @Override public void onReceive(Context context, Intent intent)
      {
         final int key = intent.getIntExtra(Intent.EXTRA_KEY_EVENT, 0);

         switch (key)
         {
         case KeyEvent.KEYCODE_MEDIA_NEXT:
            next();

         case KeyEvent.KEYCODE_MEDIA_PAUSE:
            pause(PlayState.Paused);

         case KeyEvent.KEYCODE_MEDIA_PLAY:
            switch (service.playing)
            {
            case Idle:
               next();
               break;

            case Playing:
               break;

            case Paused:
               unpause();
               break;

            case Paused_Transient:
               // user wants to override
               unpause();
               break;

            };

         case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE:
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

         case KeyEvent.KEYCODE_MEDIA_PREVIOUS:
            previous();

         };
      }
   };
   private SmBroadcastReceiverButton broadcastReceiverButton = new SmBroadcastReceiverButton();

   private BroadcastReceiver broadcastReceiverCommand = new BroadcastReceiver()
      {
         // Intent filter set for ACTION_COMMAND
         @Override public void onReceive(Context context, Intent intent)
         {
            final String command = intent.getStringExtra("command");

            utils.verboseLog(command);

            // command alphabetical order
            if (command.equals(utils.COMMAND_DUMP_LOG))
            {
               dumpLog();
            }
            else if (command.equals (utils.COMMAND_NEXT))
            {
               next();
            }
            else if (command.equals (utils.COMMAND_NOTE))
            {
               writeNote(intent.getStringExtra("note"));
            }
            else if (command.equals (utils.COMMAND_PAUSE))
            {
               pause(PlayState.Paused);
            }
            else if (command.equals (utils.COMMAND_PLAY))
            {
               switch (service.playing)
               {
               case Idle:
                  next();
                  break;

               case Playing:
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
            else if (command.equals (utils.COMMAND_PLAYLIST))
            {
               try
               {
                  // User will want to resume the current playlist at some point.
                  saveState();

                  playList(intent.getStringExtra("playlist"), PlayState.Playing);
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
            else if (command.equals(utils.COMMAND_SAVE_STATE))
            {
               saveState();
            }
            else if (command.equals (utils.COMMAND_SEEK))
            {
               final int pos = intent.getIntExtra("position", 0);
               mediaPlayer.seekTo(pos);
               notifyChange(utils.PLAYSTATE_CHANGED);
            }
            else if (command.equals(utils.COMMAND_SMM_DIRECTORY))
            {
               setSMMDirectory(context);
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
               utils.debugLog("broadcastReceiverCommand.onReceive: unknown command: " + command);
            }
         }
      };

   private RemoteControlClient remoteControlClient;
   // It's not at all clear if we are supposed to override the
   // RemoteControlClient methods; the doc confuses the player and the
   // control. Clearly some user actions on the control generate
   // broadcast events that are handled by broadcastReveiverButton.


   private OnAudioFocusChangeListener audioFocusListener = new OnAudioFocusChangeListener()
      {
          public void onAudioFocusChange(int focusChange)
          {
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
                   handler.sendEmptyMessageDelayed(UNPAUSE, 1000);
                }
             }
             else
             {
                utils.debugLog("Unknown onAudioFocusChange code " + focusChange);
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
               if (playing == PlayState.Playing)
               {
                  play (playlist.get(playlistPos), 0);
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
   private String smmDirectory;
   // Absolute path to directory containing files used to interface
   // with Stephe's Music manager (smm).
   //
   // Set by preferences.

   static private PlayState playing;

   private String playlistDirectory;
   // Absolute path to directory where playlist files reside. The
   // list of available playlists consists of all .m3u files in
   // this directory.
   //
   // Set from playlist file passed to playList

   private String playlistFilename;
   // Relative to Playlist_Directory, without extension (suitable
   // for user display). null if no playlist is current.

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

      setSMMDirectory (this);

      // We need two broadcast recievers because we can't wild card
      // all of the filter criteria.
      IntentFilter filter = new IntentFilter();
      filter.addAction(Intent.ACTION_MEDIA_BUTTON);
      registerReceiver(broadcastReceiverButton, filter);

      filter = new IntentFilter();
      filter.addAction(utils.ACTION_COMMAND);
      registerReceiver(broadcastReceiverCommand, filter);

      createMediaPlayer();

      audioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
      {
         ComponentName receiver = new ComponentName(getPackageName(), SmBroadcastReceiverButton.class.getName());

         audioManager.registerMediaButtonEventReceiver(receiver);

         Intent i = new Intent(Intent.ACTION_MEDIA_BUTTON).setComponent(receiver);

         remoteControlClient = new RemoteControlClient(PendingIntent.getBroadcast(this, 0, i, 0));

         audioManager.registerRemoteControlClient(remoteControlClient);
         remoteControlClient.setTransportControlFlags
            (
               RemoteControlClient.FLAG_KEY_MEDIA_NEXT |
               RemoteControlClient.FLAG_KEY_MEDIA_PAUSE |
               RemoteControlClient.FLAG_KEY_MEDIA_PLAY |
               RemoteControlClient.FLAG_KEY_MEDIA_PLAY_PAUSE |
               RemoteControlClient.FLAG_KEY_MEDIA_PREVIOUS
            );
      }

      playlist           = new LinkedList<String>();
      playlistPos        = -1;
      highestPlaylistPos = playlistPos;
      playing            = PlayState.Idle;
      haveAudioFocus     = false;

      restoreState();
   }

   @Override public void onDestroy()
   {
      utils.verboseLog("onDestroy");

      // Android sometimes restarts this service even though we have
      // quit and the user did not request it. So if we save
      // PlayState.playing, we will start playing when the user did
      // not request it, and without an activity to control us! So
      // force pause.

      pause(PlayState.Paused);

      saveState();

      mediaPlayer.reset();
      mediaPlayer.release();
      mediaPlayer = null;

      audioManager.abandonAudioFocus(audioFocusListener);
      {
         ComponentName receiver = new ComponentName(getPackageName(), SmBroadcastReceiverButton.class.getName());
         audioManager.unregisterMediaButtonEventReceiver(receiver);
      }
      audioManager.unregisterRemoteControlClient(remoteControlClient);

      handler.removeCallbacksAndMessages(null);

      unregisterReceiver(broadcastReceiverButton);
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
         utils.debugLog("onStartCommand got unexpected intent: " + intent);
      }
      return START_STICKY;
    }

   private void dumpLog()
   {
      final String logFilename = smmDirectory + "/stephes_music/" + playlistFilename + ".log";

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
         utils.errorLog(this, "can't write log to " + logFilename, e);
      }
   }

   @Override protected void dump(FileDescriptor fd, PrintWriter writer, String[] args)
   {
      writer.println("playing            : " + playing);
      writer.println("playlistDirectory  : " + playlistDirectory);
      writer.println("playlistFilename   : " + playlistFilename);
      writer.println("playlist size      : " + playlist.size());
      writer.println("currentFile        : " + playlist.get(playlistPos));
      writer.println("playlistPos        : " + playlistPos);
      writer.println("highestPlaylistPos : " + highestPlaylistPos);

      SharedPreferences storage = getSharedPreferences(utils.serviceClassName, MODE_PRIVATE);

      writer.println("storage." + keyPlaylistDirectory + ": " + storage.getString(keyPlaylistDirectory, ""));
      writer.println("storage." + keyPlaylistFilename + ": " + storage.getString(keyPlaylistFilename, ""));

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
