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

package org.stephe_leake.android.music_player;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.media.MediaScannerConnection;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.SystemClock;
import android.provider.MediaStore;
import android.provider.MediaStore.Audio;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.TextView;
import org.stephe_leake.android.stephes_music.utils;
public class activity extends android.app.Activity
{
   // constants
   private static final int Progress_Max = 1000;
   private static final int REFRESH      = 1;

   private static final int DIALOG_PLAYLIST = 1;

   private static final int MENU_QUIT = 0;

   // Main UI members

   private TextView    playlistTitle;
   private TextView    artistTitle;
   private TextView    albumTitle;
   private TextView    songTitle;
   private TextView    Current_Time;
   private TextView    Total_Time;
   private ImageButton Play_Pause_Button;
   private SeekBar     Progress_Bar;

   // Main other members

   // FIXME: need to enumerate available volumes that might have
   // playlists on them.
   private String  playlistVolumeName = "external";
   private boolean connected          = true;

   // Cached values, set by Update_Display

   private long    Duration  = 0; // track duration in milliseconds
   private boolean isPlaying = false;

   ////////// Activity lifetime methods (in lifecycle order)

   @Override public void onCreate(Bundle savedInstanceState)
   {
      // FIXME: get scale from preferences
      final float scale = 1.3f;

      try
      {
         super.onCreate(savedInstanceState);
         setContentView(R.layout.main);

         // Set up displays, top to bottom left to right

         artistTitle = (TextView) findViewById(R.id.artistTitle);
         albumTitle  = (TextView) findViewById(R.id.albumTitle);
         songTitle   = (TextView) findViewById(R.id.songTitle);

         artistTitle.setTextSize(scale * artistTitle.getTextSize());
         albumTitle.setTextSize(scale * albumTitle.getTextSize());
         songTitle.setTextSize(scale * songTitle.getTextSize());

         ((ImageButton)findViewById(R.id.prev)).setOnClickListener(Prev_Listener);

         Current_Time = (TextView)findViewById(R.id.currenttime);

         Play_Pause_Button = (ImageButton)findViewById(R.id.play_pause);
         Play_Pause_Button.setOnClickListener(Play_Pause_Listener);
         Play_Pause_Button.requestFocus();

         ((ImageButton)findViewById(R.id.next)).setOnClickListener(Next_Listener);

         Total_Time = (TextView)findViewById(R.id.totaltime);

         Progress_Bar = (SeekBar) findViewById(android.R.id.progress);
         Progress_Bar.setOnSeekBarChangeListener(Progress_Listener);
         Progress_Bar.setMax(Progress_Max);

         playlistTitle = (TextView) findViewById(R.id.playlistTitle);
         playlistTitle.setTextSize(playlistTitle.getTextSize()); // not as important, save screen space
         playlistTitle.setOnClickListener(playlistListener);

         // We can't call any MusicUtils functions that use the
         // service yet; it won't be bound until this activity is
         // fully created and waiting for user input. So we process
         // the startup intent in onServiceConnected.
      }
      catch (RuntimeException e)
      {
         // From somewhere
         utils.Error_Log(this, "onCreate: That does not compute " + e.toString());
         finish();
      }
   }

   @Override protected void onNewIntent(Intent intent)
   {
      utils.debugLog("onNewIntent: " + intent);
      if (connected)
         try
         {
            handleStartIntent(intent, false);
         }
         catch (RuntimeException e)
         {
            utils.Error_Log(this, "onNewIntent: intent " + intent + ": " + e.toString());
         }
      else
      {
         // Main app has started, but service is not connected.
         // Override original intent with this one, so
         // onServiceConnected will use it.
         //
         // FIXME: should queue them both; someone could be adding
         // songs in a hurry.
         setIntent(intent);
      }
   }

   @Override protected void onResume()
   {
      super.onResume();
      try
      {
         IntentFilter f = new IntentFilter();
         f.addAction(utils.META_CHANGED);
         f.addAction(utils.PLAYSTATE_CHANGED);
         registerReceiver(Service_Listener, f);

         Update_Display();
      }
      catch (RuntimeException e)
      {
         utils.Error_Log(this, "onResume: " + e.toString());
      }
   }

   @Override protected void onPause()
   {
      super.onPause();
      Message_Handler.removeMessages(REFRESH);

      try
      {
         unregisterReceiver(Service_Listener);
      }
      catch (RuntimeException e)
      {
         utils.Error_Log(this, "registerReceiver: " + e.toString());
      }
   }

   //////////
   // Options Menu (main menu)

   @Override public boolean onCreateOptionsMenu(Menu menu)
   {
      super.onCreateOptionsMenu(menu);
      menu.add(0, MENU_QUIT, 0, "quit");
      return true;
   }

   @Override public boolean onOptionsItemSelected(MenuItem item)
   {
      switch (item.getItemId())
      {
      case MENU_QUIT:
         // FIXME: does this cause the service to terminate?
         sendBroadcast(new Intent(utils.INTENT_PAUSE));

         finish();
         break;

      default:
         utils.Error_Log
            (this, "PlayerActivity.onOptionsItemSelected: unknown MenuItemId " + item.getItemId());
      }
      return false;
   }

   ////////// private non-UI members and methods

   private void handleStartIntent(Intent intent, boolean alreadyScanned)
   {
      if (intent == null || // not clear if we can get this
          intent.getAction() == null || // from widget, or destroyed/restored (ie for screen rotate)
          intent.getAction().equals(Intent.ACTION_MAIN)) // launched directly by user
      {
         // Server may be playing or paused
         Update_Display();

         // FIXME: offer file/db browsers
      }
      else if (intent.getAction().equals(Intent.ACTION_VIEW))
      {
         Uri Item_Uri = intent.getData();
         String mimeType = intent.getType();

         // list from /Projects/android/google/frameworks/base/media/java/android/media/MediaFile.java
         // assuming these are the only playlist formats supported by the MediaScanner
         if (mimeType.equals("audio/x-mpegurl") ||
             mimeType.equals("audio/x-scpls") ||
             mimeType.equals("application/vnd.ms-wpl"))
         {
            Play_List (Item_Uri, mimeType, alreadyScanned);
         }
         else
         {
            Add_Song (Item_Uri, mimeType);
         };
      }
      else
      {
         // These are the only actions we declared in our manifest.
         utils.Error_Log(this, "unexpected intent " + intent);
      }
   }

   private class Not_Found extends Exception
   {
      static final long serialVersionUID = 0;

      public Not_Found(String string)
      {
         super(string);
      }
   };

   private long Get_ID (final Uri contentURI, final Uri Item_URI) throws Not_Found
   {
      long         ID;
      final String path        = Item_URI.getPath();
      final String[] columns   = new String[]{Audio.Media._ID};
      final String where       = MediaStore.Audio.Media.DATA + "=?";
      final int    idColumn    = 0;
      final String[] selection = new String[] {path};
      Cursor       cursor      = getContentResolver().query(contentURI, columns, where, selection, null);

      // cursor is before first result, or null
      if (cursor != null && cursor.getCount() > 0)
      {
         if (cursor.getCount() > 1)
            utils.Error_Log(Stephes_Music_PlayerActivity.this, Item_URI + ": multiple matches in database");

         cursor.moveToFirst();
         ID = cursor.getLong(idColumn);
         cursor.close();
      }
      else
      {
         throw new Not_Found(path + ": not found in database");
      }
      return ID;
   }

   private void Add_Song (Uri Song_URI, String mimeType)
   {
      try
      {
         final Uri  contentURI = MediaStore.Audio.Media.getContentUriForPath(Song_URI.getPath());

         utils.addToCurrentPlaylist
            (this,
             new long[]{Get_ID(contentURI, Song_URI)},
             utils.isPlaying() ? utils.LAST : utils.NOW);
      }
      catch (Not_Found e)
      {
         // FIXME: scan here? the media scanner runs when sdcard is
         // unmounted, but there are other ways to get files on the
         // sdcard (via wifi or phone)
         utils.Info_Log(this, e.getMessage());
      }
      catch (RuntimeException e)
      {
         utils.Error_Log(this, "Add_Song: " + e.toString());
      }
    }

   private void Play_List (Uri listURI, String mimeType, boolean alreadyScanned)
   {
      // listURI is the name of a playlist file of MIME type
      // mimeType. If found in the db, send its id to the server.
      // Otherwise scan it, then send its id to the server.

      // To get the proper content Uri for Playlists, we have to call
      // Playlists.getContentUri. But that takes a volume name, and
      // there doesn't seem to be a documented way to get the volume
      // name from anything. So this is an undocumented hack;
      // apparently content Uris have the form:
      //
      // content://media/<volume name>/
      final String tmp         = MediaStore.Audio.Media.getContentUriForPath(listURI.getPath()).toString();
      final int    start       = "content://media/".length();
      final String Volume_Name = tmp.substring(start, tmp.indexOf('/', start));
      final Uri    contentURI = MediaStore.Audio.Playlists.getContentUri(Volume_Name);

      try
      {
         utils.replaceCurrentPlaylist (Volume_Name, Get_ID(contentURI, listURI));
      } catch (Not_Found e)
      {
         if (alreadyScanned)
         {
            utils.Error_Log(this, listURI.toString() + " not found after scan");
         } else
         {
            // A brand new playlist; scan it
            utils.debugLog("new playlist; scanning " + listURI.toString());
            MediaScannerConnection.scanFile
               (this, new String[] {listURI.getPath()}, new String[] {mimeType}, ScanListener);
         }
      }
   }

   private MediaScannerConnection.OnScanCompletedListener ScanListener = new MediaScannerConnection.OnScanCompletedListener()
   {
      public void onScanCompleted (String path, Uri uri)
      {
         // uri is always null for playlists; not clear why. So we
         // have to look for the ID to tell if the scan succeeded.

         // FIXME: could be a new intent while we were waiting for
         // scan; need a queue.
         utils.debugLog("scan complete; got " + uri.toString());
         utils.debugLog(" ... starting " + getIntent().toString());
         handleStartIntent(getIntent(), true);
      }
   };

   ////////// private UI methods

   private void Update_Display()
   {
      if (! utils.isConnected())
      {
         // can't do anything without the service
         playlistTitle.setText ("");
         artistTitle.setText ("");
         albumTitle.setText ("");
         songTitle.setText ("");

         return;
      }

      try
      {
         // FIXME: this data is in the notify change intent; either
         // get it from there, or delete it (see widget)
         playlistTitle.setText
            (utils.getPlaylistName() +
             " (" + utils.getQueuePosition() + "/" + utils.getQueueLength() + ")");
         artistTitle.setText(utils.getArtistName());
         albumTitle.setText(utils.getAlbumName());
         songTitle.setText(utils.getTrackName());

         isPlaying = utils.isPlaying();

         if (isPlaying)
         {
            Play_Pause_Button.setImageResource(android.R.drawable.ic_media_pause);
         }
         else
         {
            Play_Pause_Button.setImageResource(android.R.drawable.ic_media_play);
         }

         Duration = utils.getDuration();
         Total_Time.setText(utils.makeTimeString(this, Duration));

         queueNextRefresh(refreshNow());
      }
      catch (RuntimeException e)
      {
         utils.Error_Log(this, "Update_Display: " + e.toString());
      }
   }

   private BroadcastReceiver Service_Listener = new BroadcastReceiver()
      {
         // see utils.java "void notifyChange(" for
         // list of intents

         @Override public void onReceive(Context context, Intent intent)
         {
            String action = intent.getAction();

            if (! utils.isConnected())
            {
               // startup message from service; just ignore it
               return;
            }

            try
            {
               if (action.equals(utils.META_CHANGED) ||
                   action.equals(utils.PLAYSTATE_CHANGED))
               {
                  Update_Display();
               }
               else
               {
                  // FIXME: register and handle other status intents
               }
            }
            catch (RuntimeException e)
            {
               utils.Error_Log(Stephes_Music_PlayerActivity.this, "Service_Listener: " + e.toString());
            }
         }
      };

   private ImageButton.OnClickListener Prev_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.PREVIOUS_ACTION));
         }
      };

   private ImageButton.OnClickListener Play_Pause_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.TOGGLEPAUSE_ACTION));
         }
      };

   private ImageButton.OnClickListener Next_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.NEXT_ACTION));
         }
      };

    private OnSeekBarChangeListener Progress_Listener = new OnSeekBarChangeListener()
       {
          long LastSeekEventTime = 0;

          public void onStartTrackingTouch(SeekBar bar)
          {
             LastSeekEventTime = SystemClock.elapsedRealtime();
          }

          public void onProgressChanged(SeekBar bar, int progress, boolean fromuser)
          {
             if (!fromuser || !utils.isConnected()) return;

             long now = SystemClock.elapsedRealtime();
             if ((now - LastSeekEventTime) > 250)
             {
                LastSeekEventTime = now;
                utils.seek(Duration * progress / Progress_Max);
             }
          }
          public void onStopTrackingTouch(SeekBar bar)
          {
          }
       };

   @Override protected Dialog onCreateDialog(int id)
   {
      switch (id)
      {
        case DIALOG_PLAYLIST:
           {
              try
              {
                 // Apparently the projection must include the _ID column. However, 'which' is _not_ the id.
                 final Uri    uri            = Audio.Playlists.getContentUri(playlistVolumeName);
                 final String[] columns      = new String[]{Audio.Playlists._ID, Audio.Playlists.NAME};
                 final int    idColumn       = 0;
                 final Cursor playlistCursor = this.getContentResolver().query(uri, columns, null, null, null);

                 return new AlertDialog.Builder(this)
                    .setTitle("Select a Playlist")
                    .setCursor
                    (playlistCursor,
                     new DialogInterface.OnClickListener()
                     {
                        public void onClick(DialogInterface dialog, int which)
                        {
                           try
                           {
                              playlistCursor.moveToPosition(which);
                              utils.replaceCurrentPlaylist (playlistVolumeName, playlistCursor.getLong(idColumn));
                           }
                           catch (Exception e)
                           {
                              utils.debugLog("playlist dialog onClick: " + e.toString());
                           }
                        };
                     },
                     Audio.Playlists.NAME
                     ).create();
              }
              catch (Exception e)
              {
                 utils.debugLog("create playlist dialog " + e.toString());
                 return null;
              }
           }
        default:
           utils.debugLog("unknown dialog id " + id);
           return null;
        }
    }

   private TextView.OnClickListener playlistListener = new TextView.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            showDialog(DIALOG_PLAYLIST);
         }
      };

   private long refreshNow()
   {
      // Update progress bar and time display; return milliseconds until next update
      if (Service == null) return 500;

      long pos = utils.getPosition();

      if (pos >= 0 && Duration > 0)
      {
         Current_Time.setText(utils.makeTimeString(this, pos));
         Progress_Bar.setProgress((int) (Progress_Max * pos / Duration));

      } else
      {
         Current_Time.setText("--:--");
         Progress_Bar.setProgress(Progress_Max);
      }

      // Return the number of milliseconds until the next full second,
      // so the counter can be updated at just the right time.
      return 1000 - (pos % 1000);
    }

    private final Handler Message_Handler = new Handler()
       {
          @Override public void handleMessage(Message msg)
          {
             switch (msg.what)
             {
             case REFRESH:
                queueNextRefresh(refreshNow());
                break;
             default:
                break;
             }
          }
       };

   private void queueNextRefresh(long delay)
   {
      if (isPlaying)
      {
         Message msg = Message_Handler.obtainMessage(REFRESH);
         Message_Handler.removeMessages(REFRESH);
         Message_Handler.sendMessageDelayed(msg, delay);
        }
    }
}
