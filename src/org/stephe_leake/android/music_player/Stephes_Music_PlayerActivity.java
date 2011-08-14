package org.stephe_leake.android.music_player;

import java.io.IOException;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;
import android.media.MediaMetadataRetriever;
import android.widget.Button;

public class Stephes_Music_PlayerActivity extends Activity
{
   private TextView Song_Title;
   private TextView Album_Title;
   private TextView Artist_Title;
   private android.media.MediaPlayer Player;
   private android.widget.Button Play_Button;
   private android.widget.Button Pause_Button;

   private String Song_URI = "/mnt/sdcard/Audio/Vocal/01 - Fill Me Up.mp3";

   /** Called when the activity is first created. */
   @Override
      public void onCreate(Bundle savedInstanceState)
   {
      try
      {
         super.onCreate(savedInstanceState);
         setContentView(R.layout.main);

         Song_Title = (TextView) findViewById(R.id.Song_Title);
         Album_Title = (TextView) findViewById(R.id.Album_Title);
         Artist_Title = (TextView) findViewById(R.id.Artist_Title);

         // Set up buttons.
         Play_Button = (Button)findViewById(R.id.Play);
         Play_Button.setOnClickListener(Play_Listener);
         Pause_Button = (Button)findViewById(R.id.Pause);
         Pause_Button.setOnClickListener(Pause_Listener);

         ((Button)findViewById(R.id.Quit)).setOnClickListener(Quit_Listener);

         MediaMetadataRetriever Meta = new MediaMetadataRetriever();

         try
         {
            Meta.setDataSource(Song_URI);
         }
         catch (RuntimeException e)
         {
            // From Meta.setDataSource
            Song_Title.setText(Song_URI + " NOT FOUND");
            return;
         }

         Player = new android.media.MediaPlayer();

         try
         {
            Player.setDataSource(Song_URI);
            Player.prepare();
         }
         catch (IOException e) {
            // from Player.setDataSource
            Song_Title.setText(Song_URI + " CANNOT BE PLAYED : " + e.getMessage());
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
      catch (RuntimeException e)
      {
         // From somewhere
         android.widget.Toast.makeText(this, "That does not compute " + e.getMessage(), 10).show();
         return;
      }
   }

   @Override
      protected void onDestroy()
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
         @Override
            public void onClick(View v)
         {
            Player.start();
            Play_Button.setVisibility(android.view.View.GONE);
            Pause_Button.setVisibility(android.view.View.VISIBLE);
         }

      };

   private android.widget.Button.OnClickListener Pause_Listener = new android.widget.Button.OnClickListener()
      {
         @Override
            public void onClick(View v)
         {
            Player.pause();
            Pause_Button.setVisibility(android.view.View.GONE);
            Play_Button.setVisibility(android.view.View.VISIBLE);
         }
      };

   private android.widget.Button.OnClickListener Quit_Listener = new android.widget.Button.OnClickListener()
      {
         @Override
            public void onClick(View v)
         {
            finish();
         }
      };
}
