package org.stephe_leake.android.music_player;

import android.app.Activity;
import android.media.MediaMetadataRetriever;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

public class Stephes_Music_PlayerActivity extends Activity
{
   private TextView Song_Title;
   private TextView Album_Title;
   private TextView Artist_Title;
   private android.media.MediaPlayer Player;
   private android.widget.Button Play_Button;
   private android.widget.Button Pause_Button;

   //   private String Song_PATH = "/mnt/sdcard/Audio/Vocal/01 - Fill Me Up.mp3";
   static private String Default_Song_Path = "/mnt/sdcard/Audio/vocal.m3u";

   /** Called when the activity is first created. */
   @Override public void onCreate(Bundle savedInstanceState)
   {
      try
      {
         super.onCreate(savedInstanceState);
         setContentView(R.layout.main);

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

         // Get the song we are supposed to play
         android.content.Intent Intent = getIntent();

         if (Intent.getAction().equals(android.content.Intent.ACTION_VIEW))
         {
            String Song_Path = getIntent().getData().getPath();

            // mime type audio/x-mpegurl maps to .m3u files (http://en.wikipedia.org/wiki/M3U)
            if (Intent.getType().equals("audio/x-mpegurl"))
            {
               Play_List (Song_Path);
            }
            else
            {
               Play_Single (Song_Path);
            };
         }
         else if (Intent.getAction().equals(android.content.Intent.ACTION_MAIN))
         {
            // launched directly by user
            // FIXME: resume last bookmark, or present file browser, or do nothing.
            // default Song_Path is a playlist
            Play_List (Default_Song_Path);
         }
         else
         {
            Song_Title.setText("Unexpected intent action: '" + Intent.getAction() + "'");
            finish();
            return;
         }
      }
      catch (RuntimeException e)
      {
         // From somewhere
         android.widget.Toast.makeText(this, "That does not compute " + e.getMessage(), 100).show();
         finish();
         return;
      }
   }

   private void Play_Single (String Song_Path)
   {
      MediaMetadataRetriever Meta = new MediaMetadataRetriever();

      try
         {
            Meta.setDataSource(Song_Path.toString());
         }
         catch (RuntimeException e)
         {
            Song_Title.setText("MetaDataRetriever(" + Song_Path + ") error: " + e.getMessage());
            return;
         }

      if (Player == null)
      {
         Player = new android.media.MediaPlayer();
      }

      try
      {
         Player.setDataSource(Song_Path.toString());
         Player.prepare();
      }
      catch (java.io.IOException e)
      {
         Song_Title.setText("setDataSource(" + Song_Path + ") error: " + e.getMessage());
         return;
      }

      Player.start();

      Song_Title.setText(Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_TITLE));

      Album_Title.setText(Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ALBUM));

      // FIXME: avoid duplicates and nulls
      Artist_Title.setText
         (Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ALBUMARTIST) +
          Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ARTIST) +
          Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_AUTHOR) +
          Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_COMPOSER));

      Pause_Button.setVisibility(android.view.View.VISIBLE);
      Play_Button.setVisibility(android.view.View.GONE);
   }

   private void Play_List (String List_Path)
   {
      java.io.BufferedReader reader;
      String line;

      // FIXME: just playing the first entry; testing intents vs file browser
      try
      {
         reader = new java.io.BufferedReader
                         (new java.io.InputStreamReader
                                         (new java.io.FileInputStream(List_Path)), 8192);
      }
      catch (java.io.FileNotFoundException e)
      {
         Song_Title.setText("open file (" + List_Path + ") error: " + e.getMessage());
         return;
      }

      try
      {
         line = reader.readLine();
      }
      catch (java.io.IOException e)
      {
         Song_Title.setText("readline (" + List_Path + ") error: " + e.getMessage());
         return;
      }

      // .m3u playlist entries can be relative or absolute; assume relative (FIXME:)
      // Play_Single needs an absolute path; add the path to List_Path

      Play_Single((new java.io.File(List_Path)).getParent() + "/" + line);
   }


   @Override protected void onDestroy()
   {
      super.onDestroy();
      if (Player != null)
      {
         Player.release();
         Player = null;
      }
   }

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
            finish();
         }
      };
}
