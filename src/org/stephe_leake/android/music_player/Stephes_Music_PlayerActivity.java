package org.stephe_leake.android.music_player;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.provider.MediaStore;
import android.provider.MediaStore.Audio;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;
import java.io.File;
import org.stephe_leake.android.music_player.MusicUtils.ServiceToken;

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
         // itself after 60 seconds (Stephes_Music_Service.java
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
      }
   }

   @Override protected void onResume()
   {
      super.onResume();
      try
      {
         IntentFilter f = new IntentFilter();
         f.addAction(Stephes_Music_Service.META_CHANGED);
         f.addAction(Stephes_Music_Service.PLAYSTATE_CHANGED);
         registerReceiver(Service_Listener, f);

         Update_Display();
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "onResume: " + e.toString() + ": " + e.getMessage());
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
      Intent intent = getIntent();
      try
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

            if (intent.getType().equals("audio/x-mpegurl"))
            {
               // .m3u file (http://en.wikipedia.org/wiki/M3U)
               // FIXME: add other MIME types; match MediaScanner
               Play_List (Item_Uri);
            }
            else
            {
               Add_Song (Item_Uri);
            };
         }
         else
         {
            // These are the only actions we declared in our manifest.
            MusicUtils.Error_Log(this, "onServiceConnected: unexpected intent " + intent);
         }
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "onServiceConnected: intent " + intent + ": " + e.getMessage());
      }
   }

   public void onServiceDisconnected(ComponentName className)
   {
      // nothing to do here.
   }

   ////////// private non-UI members and methods

   private class Not_Found extends Exception
   {
      static final long serialVersionUID = 0;

      public Not_Found(String string)
      {
         super(string);
      }
   };

   private long Get_ID (final Uri Content_Uri, final Uri Item_Uri) throws Not_Found
   {
      final long result;

      final String path        = Item_Uri.getPath();
      final String[] columns   = new String[]{Audio.Media._ID};
      final String[] selection = new String[] { path };
      final String where       = MediaStore.Audio.Media.DATA + "=?";
      final int    idColumn    = 0;
      Cursor       cursor      = Content_Resolver.query(Content_Uri, columns, where, selection, null);
      // cursor is before first result, or null
      if (cursor != null && cursor.getCount() > 0)
      {
         if (cursor.getCount() > 1)
            MusicUtils.Error_Log(this, Item_Uri + ": multiple matches in database");

         cursor.moveToFirst();
         result = cursor.getLong(idColumn);
         cursor.close();
      }
      else
      {
         throw new Not_Found(path + ": not found in database");
      }

      return result;
   }

   private void Add_Song (Uri Song_Uri)
   {
      try
      {
         final Uri  Content_Uri = MediaStore.Audio.Media.getContentUriForPath(Song_Uri.getPath());
         final long ID          = Get_ID (Content_Uri, Song_Uri);

         MusicUtils.addToCurrentPlaylist
            (this,
             new long[]{ID},
             MusicUtils.isPlaying() ? Stephes_Music_Service.LAST : Stephes_Music_Service.NOW);
      }
      catch (Not_Found e)
      {
         MusicUtils.Info_Log(this, e.getMessage());
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "Add_Song: " + e.toString()+ ": " + e.getMessage());
      }
    }

   private void Play_List (Uri List_Uri)
   {
      // List_Uri is the name of a .m3u file; we assume the
      // MediaScanner has read it and stored its contents. So just
      // send its id to the server.

      // To get the proper content Uri for Playlists, we have to call
      // Playlists.getContentUri. But that takes a volume name, and
      // there doesn't seem to be a documented way to get the volume
      // name from anything. So this is an undocumented hack;
      // apparently content Uris have the form:
      //
      // content://media/<volume name>/
      final String tmp         = MediaStore.Audio.Media.getContentUriForPath(List_Uri.getPath()).toString();
      final int    start       = "content://media/".length();
      final String Volume_Name = tmp.substring(start, tmp.indexOf('/', start));
      final Uri Content_Uri = MediaStore.Audio.Playlists.getContentUri(Volume_Name);

      try
      {
         MusicUtils.replaceCurrentPlaylist (Volume_Name, Get_ID (Content_Uri, List_Uri));
      } catch (Not_Found e)
      {
         MusicUtils.Info_Log(this, e.getMessage());
         // FIXME: debug, showing playlists that are in database
         MusicUtils.Info_Log(this, "volume: " + Volume_Name);
         MusicUtils.Info_Log(this, "playlists in db:");
         final String[] columns   = new String[]{Audio.Media.DATA};
         final int    File_Column = 0;
         final String[] selection = null;
         final String where       = null;
         Cursor       cursor      = Content_Resolver.query(Content_Uri, columns, where, selection, null);

         if (cursor != null && cursor.getCount() > 0)
         {
            while (!cursor.moveToNext())
            {
               MusicUtils.Info_Log(this, cursor.getString(File_Column));
            }
            cursor.close();
         }
         else
         {
            if (cursor != null) cursor.close();
            MusicUtils.Info_Log(this, "none");
         }
      }
   }

   ////////// private UI methods

   private void Update_Display()
   {
      if (! MusicUtils.isConnected())
      {
         // can't do anything without the service
         Song_Title.setText ("");
         Album_Title.setText ("");
         Artist_Title.setText ("");

         Play_Button.setVisibility(android.view.View.GONE);
         Pause_Button.setVisibility(android.view.View.GONE);
         return;
      }

      try
      {
         Song_Title.setText(MusicUtils.getTrackName());
         Album_Title.setText(MusicUtils.getAlbumName());
         Artist_Title.setText(MusicUtils.getArtistName());

         if (MusicUtils.isPlaying())
         {
            Play_Button.setVisibility(android.view.View.GONE);
            Pause_Button.setVisibility(android.view.View.VISIBLE);
         }
         else
         {
            Pause_Button.setVisibility(android.view.View.GONE);
            Play_Button.setVisibility(android.view.View.VISIBLE);
         }
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "Update_Display: " + e.toString()+ ": " + e.getMessage());
      }
   }

   private BroadcastReceiver Service_Listener = new BroadcastReceiver()
      {
         // see Stephes_Music_Service.java "void notifyChange(" for
         // list of intents

         @Override public void onReceive(Context context, Intent intent)
         {
            String action = intent.getAction();

            if (! MusicUtils.isConnected())
            {
               // startup message from service; just ignore it
               return;
            }

            try
            {
               if (action.equals(Stephes_Music_Service.META_CHANGED) ||
                   action.equals(Stephes_Music_Service.PLAYSTATE_CHANGED))
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
               MusicUtils.Error_Log(Stephes_Music_PlayerActivity.this, "Service_Listener: " + e.getMessage());
            }
         }
      };

   private android.widget.Button.OnClickListener Play_Listener = new android.widget.Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(Stephes_Music_Service.TOGGLEPAUSE_ACTION));
         }
      };

   private android.widget.Button.OnClickListener Pause_Listener = new android.widget.Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(Stephes_Music_Service.TOGGLEPAUSE_ACTION));
         }
      };

   private android.widget.Button.OnClickListener Quit_Listener = new android.widget.Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            if (MusicUtils.isPlaying())
               sendBroadcast(new Intent(Stephes_Music_Service.TOGGLEPAUSE_ACTION));

            finish();
         }
      };
}
