package org.stephe_leake.android.music_player;

import java.io.File;

import org.stephe_leake.android.music_player.MusicUtils.ServiceToken;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.media.MediaScannerConnection;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.provider.MediaStore.Audio;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

public class Stephes_Music_PlayerActivity extends Activity implements ServiceConnection
{
   // Main UI members

   private TextView Song_Title;
   private TextView Album_Title;
   private TextView Artist_Title;
   private android.widget.Button Play_Button;
   private android.widget.Button Pause_Button;

   // Main other members
   private ServiceToken Service;
   private ContentResolver Content_Resolver;

   ////////// Activity lifetime methods (in lifecycle order)

   @Override public void onCreate(Bundle savedInstanceState)
   {
      try
      {
         super.onCreate(savedInstanceState);
         setContentView(R.layout.main);

         // Bind to the service, so we can send it songs. It stops
         // itself after 60 seconds (MediaPlaybackService.java
         // IDLE_DELAY) if not bound or playing.
         Service = MusicUtils.bindToService(this, (ServiceConnection) this);

         Content_Resolver = getContentResolver();

         // Set up text displays
         Song_Title = (TextView) findViewById(R.id.Song_Title);
         Album_Title = (TextView) findViewById(R.id.Album_Title);
         Artist_Title = (TextView) findViewById(R.id.Artist_Title);

         // Set up buttons.
         Play_Button = (Button)findViewById(R.id.Play);
         Play_Button.setOnClickListener(Play_Listener);
         Pause_Button = (Button)findViewById(R.id.Pause);
         Pause_Button.setOnClickListener(Pause_Listener);

         ((Button)findViewById(R.id.Quit)).setOnClickListener(Quit_Listener);

         // We can't call any MusicUtils functions that use the
         // service yet; it won't be bound until this activity is
         // fully created and waiting for user input. So we process
         // the startup intent in onServiceConnected.
      }
      catch (RuntimeException e)
      {
         // From somewhere
         MusicUtils.Error_Log(this, "onCreate: That does not compute " + e.getMessage());
         finish();
         return;
      }
   }

   @Override protected void onResume()
   {
      super.onResume();
      try
      {
         IntentFilter f = new IntentFilter();
         f.addAction(MediaPlaybackService.META_CHANGED);
         registerReceiver(Service_Listener, f);
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "registerReceiver: " + e.toString() + ": " + e.getMessage());
      }
   }

   @Override protected void onPause()
   {
      super.onPause();
      try
      {
         unregisterReceiver(Service_Listener);
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "registerReceiver: " + e.toString() + ": " + e.getMessage());
      }
   }

   @Override protected void onDestroy()
   {
      MusicUtils.unbindFromService(Service);

      super.onDestroy();
   }

   ////////// ServiceConnection lifetime methods

   public void onServiceConnected(ComponentName className, android.os.IBinder service)
   {
      try
      {
         Intent intent = getIntent();

         if (intent.getAction().equals(Intent.ACTION_VIEW))
         {
            Uri Song_Uri = intent.getData();

            if (intent.getType().equals("audio/x-mpegurl"))
            {
               // .m3u file (http://en.wikipedia.org/wiki/M3U)
               Play_List (Song_Uri);
            }
            else
            {
               Send_To_Server (Song_Uri);
            };
         }
         else if (intent.getAction().equals(Intent.ACTION_MAIN))
         {
            // launched directly by user, or destroyed/restored (ie for screen rotate)
            //
            // service not affected by destroy/restore, so we don't do anything for that case.
            //
            // FIXME: resume last bookmark, or present file or other browser.

            // for initial testing, a hardcoded song
            Send_To_Server (Uri.fromFile(new File("/mnt/sdcard/Audio/Vocal/01 - Fill Me Up.mp3")));
         }
         else
         {
            // These are the only actions we declared in our manifest.
         }
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "onServiceConnected: That does not compute " + e.getMessage());
      }
   }

   public void onServiceDisconnected(ComponentName className)
   {
      // nothing to do here.
   }

   ////////// private non-UI members and methods

   private void Send_To_Server (final Uri Song_Uri)
   {
      // Send a single song to the server to play - it is added to the
      // current playlist.
      //
      // FIXME: change it to a list of songs.
      //
      // Metadata is displayed by Service_Callback_Handler
      // when a song starts playing, not here.

      // The server uses database ids to identify songs; here we
      // convert the Song_Uri to a database id. That also serves
      // to check that it is a readable file. It assumes the
      // MediaScanner has seen it.

      if (Service == null)
      {
         MusicUtils.Error_Log(this, "service not bound");
         return;
      }

      // FIXME: Currently assumes Song_URI is a file:// URI
      // try a webcast

      try
      {
         // We're looking for a single song, so this should not take a
         // long time, so we don't use CursorLoader
         //
         // We don't call open(String path) in
         // MediaPlaybackService.java, because it doesn't scan the
         // file if it's not yet in the database, so it won't be saved
         // in the playlist state. It also plays the song immediately,
         // instead of adding it to the current playlist.
         final String path = Song_Uri.getPath();
         final Uri uri = MediaStore.Audio.Media.getContentUriForPath(path);
         final String[] columns = new String[]{Audio.Media._ID};
         final String[] selection = new String[] { path };
         final String where = MediaStore.Audio.Media.DATA + "=?";
         final int idColumn = 0;
         Cursor cursor = Content_Resolver.query(uri, columns, where, selection, null);
         // cursor is before first result, or null
         if (cursor != null && cursor.getCount() == 1)
            {
               cursor.moveToFirst();
               MusicUtils.addToCurrentPlaylist(this, new long[]{cursor.getLong(idColumn)});
               MusicUtils.play(); // FIXME: should send intent?
               cursor.close();

            }
         else if (cursor == null || cursor.getCount() == 0)
            {
               MusicUtils.Info_Log(this, Song_Uri + ": not found in database; scanning it");

               if (cursor != null) cursor.close();

               MediaScannerConnection.OnScanCompletedListener client =
                  new MediaScannerConnection.OnScanCompletedListener()
                  {
                     @Override public void onScanCompleted(String Path, Uri Scanned_Uri)
                     {
                        try
                        {
                           Cursor cursor = Content_Resolver.query(uri, columns, where, selection, null);
                           if (cursor != null && cursor.getCount() == 1)
                           {
                              cursor.moveToFirst();
                              MusicUtils.Info_Log
                                 (Stephes_Music_PlayerActivity.this,
                                  Song_Uri + ": found after scan; sending it to service");
                              MusicUtils.addToCurrentPlaylist
                                 (Stephes_Music_PlayerActivity.this, new long[]{cursor.getLong(idColumn)});
                              cursor.close();
                           }
                           else
                           {
                              if (cursor != null) cursor.close();
                              MusicUtils.Error_Log
                                 (Stephes_Music_PlayerActivity.this,
                                  Song_Uri + ": not found after scan; giving up");
                           }
                        }
                        catch (RuntimeException e)
                        {
                           MusicUtils.Error_Log
                              (Stephes_Music_PlayerActivity.this, Scanned_Uri + ": database query failed after scan");
                        }
                     }
                  };

               // This hangs if the file is open in some other app (like the server :)
               MediaScannerConnection.scanFile(this, new String[]{path}, null, client);
            }
         else
         {
            MusicUtils.Error_Log(this, Song_Uri + ": multiple matches in database");
            cursor.close();
         }

      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, Song_Uri + ": database query failed before scan: " + e.toString()+ ": " + e.getMessage());
      }
   }

   private void Play_List (Uri List_Uri)
   {
      // List_Uri is the name of a .m3u file

      java.io.BufferedReader reader;
      String line;

      // FIXME: just playing the first entry for now
      try
      {
         reader = new java.io.BufferedReader
            (new java.io.InputStreamReader
             (new java.io.FileInputStream(List_Uri.getPath())), 8192);
      }
      catch (java.io.FileNotFoundException e)
      {
         Song_Title.setText("open file (" + List_Uri + ") error: " + e.getMessage());
         return;
      }

      try
      {
         line = reader.readLine();
      }
      catch (java.io.IOException e)
      {
         Song_Title.setText("readline (" + List_Uri + ") error: " + e.getMessage());
         return;
      }

      // .m3u playlist entries can be relative or absolute; assume relative (FIXME:)
      // Send_To_Server needs an absolute path; add the path to List_Uri

      Send_To_Server (Uri.fromFile(new java.io.File(new java.io.File(List_Uri.getPath()).getParent(), line)));
   }

   ////////// private UI methods

   private BroadcastReceiver Service_Listener = new BroadcastReceiver()
      {
         @Override public void onReceive(Context context, Intent intent)
         {
            String action = intent.getAction();

            if (Service == null)
            {
               MusicUtils.Error_Log
                  (Stephes_Music_PlayerActivity.this, "Service_Listener: service not bound; got intent "
                   + intent.toString());
               return;
            }

            if (action.equals(MediaPlaybackService.META_CHANGED))
            {
               // redraw the artist/title info and
               // set new max for progress bar

               Song_Title.setText(MusicUtils.getTrackName());
               Album_Title.setText(MusicUtils.getAlbumName());
               Artist_Title.setText(MusicUtils.getArtistName());
            }
            else
            {
               // FIXME: register and handle other status intents
            }
         }
      };

   private android.widget.Button.OnClickListener Play_Listener = new android.widget.Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            // MusicUtils.pause();
            Play_Button.setVisibility(android.view.View.GONE);
            Pause_Button.setVisibility(android.view.View.VISIBLE);
         }
      };

   private android.widget.Button.OnClickListener Pause_Listener = new android.widget.Button.OnClickListener()
   {
      @Override public void onClick(View v)
      {
         Pause_Button.setVisibility(android.view.View.GONE);
         Play_Button.setVisibility(android.view.View.VISIBLE);
      }
   };

   private android.widget.Button.OnClickListener Quit_Listener = new android.widget.Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            // FIXME: cancel currently playing song
            finish();
         }
      };
}
