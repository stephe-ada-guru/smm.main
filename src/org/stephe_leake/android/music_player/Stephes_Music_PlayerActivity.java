package org.stephe_leake.android.music_player;

import java.io.File;

import org.stephe_leake.android.music_player.MusicUtils.ServiceToken;

import android.app.Activity;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Intent;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.media.MediaMetadataRetriever;
import android.media.MediaScannerConnection;
import android.media.MediaScannerConnection.MediaScannerConnectionClient;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.MediaStore;
import android.provider.MediaStore.Audio;
import android.view.View;
import android.util.Log;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

public class Stephes_Music_PlayerActivity extends Activity
{
   // Main UI members

   private TextView Song_Title;
   private TextView Album_Title;
   private TextView Artist_Title;
   private android.media.MediaPlayer Player;
   private android.widget.Button Play_Button;
   private android.widget.Button Pause_Button;

   // Main other members
   private ServiceToken Service;
   private ContentResolver Content_Resolver;

   ////////// Activity lifetime methods

   @Override public void onCreate(Bundle savedInstanceState)
   {
      try
      {
         super.onCreate(savedInstanceState);
         setContentView(R.layout.main);

         // Bind to the service, so we can send it songs. It stops
         // itself after 60 seconds (MediaPlaybackService.java
         // IDLE_DELAY) if not bound or playing.
         Service = MusicUtils.bindToService(this);

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

         // Get the song we are supposed to play, if any
         Intent intent = getIntent();

         if (intent.getAction().equals(Intent.ACTION_VIEW))
         {
            Uri Song_Uri = getIntent().getData();

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
            MusicUtils.Error_Log(this, "Unexpected intent action: '" + intent.getAction() + "'");
         }
      }

      catch (RuntimeException e)
      {
         // From somewhere
         MusicUtils.Error_Log(this, "onCreate: That does not compute " + e.getMessage());
         finish();
         return;
      }
   }

   @Override protected void onDestroy()
   {
      MusicUtils.unbindFromService(Service);

      super.onDestroy();
   }

   ////////// private non-UI members and methods

   private void Send_To_Server (Uri Song_Uri)
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
         Toast.makeText(this, "service not bound", Toast.LENGTH_LONG).show();
         return;
      }

      try
      {
         // We're looking for a single song, so this should not take a
         // long time, so we don't use CursorLoader
         //
         // Couldn't find Android documentation that says this should
         // work; see AudioPreview.java
         Cursor cursor = Content_Resolver.query(Song_Uri, new String[]{Audio.Media._ID}, null, null, null);
         // cursor is before first result, or null
         if (cursor != null && cursor.getCount() == 1)
            {
               final int idColumn = cursor.getColumnIndex(Audio.Media._ID);
               cursor.moveToFirst();
               MusicUtils.Info_Log(this, Song_Uri + ": found in database; sending it to service");

               MusicUtils.addToCurrentPlaylist(this, new long[]{cursor.getLong(idColumn)});
               cursor.close();

            }
         else if (cursor == null || cursor.getCount() == 0)
            {
               MusicUtils.Info_Log(this, Song_Uri + ": not found in database; scanning it");

               if (cursor != null) cursor.close();
               
               MediaScannerConnection.OnScanCompletedListener client = new MediaScannerConnection.OnScanCompletedListener() 
               {
				   @Override public void onScanCompleted(String Path, Uri Scanned_Uri)
                     {
                        try
                        {
                           Cursor cursor = Content_Resolver.query(Scanned_Uri, new String[]{Audio.Media._ID}, null, null, null);
                           // cursor is before first result, or null
                           switch (cursor.getCount())
                           {
                           case 1:
                              {
                                 final int idColumn = cursor.getColumnIndex(Audio.Media._ID);
                                 cursor.moveToFirst();

                                 MusicUtils.addToCurrentPlaylist(Stephes_Music_PlayerActivity.this, new long[]{cursor.getLong(idColumn)});
                                 cursor.close();
                              }
                              break;
                           case 0:
                              {
                                 MusicUtils.Error_Log(Stephes_Music_PlayerActivity.this, Scanned_Uri + ": not found in database; giving up");
                              }
                              break;
                           default:
                              MusicUtils.Error_Log(Stephes_Music_PlayerActivity.this, Scanned_Uri + ": multiple matches in database");
                              cursor.close();
                              break;
                           }
                        }
                        catch (RuntimeException e)
                        {
                           MusicUtils.Error_Log(Stephes_Music_PlayerActivity.this, Scanned_Uri + ": database query failed after scan");
                        }
                     }

                  };
               MediaScannerConnection.scanFile(this, new String[]{Song_Uri.getPath()}, null, client);
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

      // FIXME: just playing the first entry; testing intents vs file browser
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

   private void Service_Callback_Handler()
   {
      // FIXME: show current song metadata
      // FIXME: delete previous song from playlist

      // Song_Title.setText(Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_TITLE));

      // Album_Title.setText(Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ALBUM));

      // // FIXME: avoid duplicates and nulls
      // Artist_Title.setText
      //    (Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ALBUMARTIST) +
      //     Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ARTIST) +
      //     Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_AUTHOR) +
      //     Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_COMPOSER));
   }

   ////////// private UI methods
   private android.widget.Button.OnClickListener Play_Listener = new android.widget.Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            Player.start();
            Play_Button.setVisibility(android.view.View.GONE);
            Pause_Button.setVisibility(android.view.View.VISIBLE);
         }

      };

   private android.widget.Button.OnClickListener Pause_Listener = new android.widget.Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            Player.pause();
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
