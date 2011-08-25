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
import android.os.Handler;
import android.os.Message;
import android.os.SystemClock;
import android.provider.MediaStore;
import android.provider.MediaStore.Audio;
import android.view.View;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.TextView;
import org.stephe_leake.android.music_player.MusicUtils.ServiceToken;

public class Stephes_Music_PlayerActivity extends Activity implements ServiceConnection
{
   // constants
   private static final int Progress_Max = 1000;
   private static final int REFRESH      = 1;

   // Main UI members

   private TextView    Playlist_Title;
   private TextView    Artist_Title;
   private TextView    Album_Title;
   private TextView    Song_Title;
   private TextView    Current_Time;
   private TextView    Total_Time;
   private ImageButton Play_Pause_Button;
   private SeekBar     Progress_Bar;

   // Main other members
   private ServiceToken    Service;
   private ContentResolver Content_Resolver;

   // Cached values, set by Update_Display

   private long    Duration  = 0; // track duration in milliseconds
   private boolean isPlaying = false;

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

         // Set up displays, top to bottom left to right

         Playlist_Title = (TextView) findViewById(R.id.Playlist_Title);
         Artist_Title   = (TextView) findViewById(R.id.Artist_Title);
         Album_Title    = (TextView) findViewById(R.id.Album_Title);
         Song_Title     = (TextView) findViewById(R.id.Song_Title);

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

         ((Button)findViewById(R.id.Quit)).setOnClickListener(Quit_Listener);

         // We can't call any MusicUtils functions that use the
         // service yet; it won't be bound until this activity is
         // fully created and waiting for user input. So we process
         // the startup intent in onServiceConnected.
      }
      catch (RuntimeException e)
      {
         // From somewhere
         MusicUtils.Error_Log(this, "onCreate: That does not compute " + e.toString());
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
         MusicUtils.Error_Log(this, "onResume: " + e.toString());
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
         MusicUtils.Error_Log(this, "registerReceiver: " + e.toString());
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
      MusicUtils.debugLog("onServiceConnected: " + intent);
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
      final String where       = MediaStore.Audio.Media.DATA + "=?";
      final String[] selection = new String[] { path };
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
         MusicUtils.Error_Log(this, "Add_Song: " + e.toString());
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
         MusicUtils.Info_Log(this, List_Uri + ": " + e.toString());
      }
   }

   ////////// private UI methods

   private void Update_Display()
   {
      if (! MusicUtils.isConnected())
      {
         // can't do anything without the service
         Playlist_Title.setText ("");
         Artist_Title.setText ("");
         Album_Title.setText ("");
         Song_Title.setText ("");

         return;
      }

      try
      {
         // FIXME: this data is in the notify change intent; either
         // get it from there, or delete it (see widget)
         Playlist_Title.setText(MusicUtils.getPlaylistName());
         Artist_Title.setText(MusicUtils.getArtistName());
         Album_Title.setText(MusicUtils.getAlbumName());
         Song_Title.setText(MusicUtils.getTrackName());

         isPlaying = MusicUtils.isPlaying();

         if (isPlaying)
         {
            Play_Pause_Button.setImageResource(android.R.drawable.ic_media_pause);
         }
         else
         {
            Play_Pause_Button.setImageResource(android.R.drawable.ic_media_play);
         }

         Duration = MusicUtils.getDuration();
         Total_Time.setText(MusicUtils.makeTimeString(this, Duration));

         queueNextRefresh(refreshNow());
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "Update_Display: " + e.toString());
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

   private ImageButton.OnClickListener Prev_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(Stephes_Music_Service.PREVIOUS_ACTION));
         }
      };

   private ImageButton.OnClickListener Play_Pause_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(Stephes_Music_Service.TOGGLEPAUSE_ACTION));
         }
      };

   private ImageButton.OnClickListener Next_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(Stephes_Music_Service.NEXT_ACTION));
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
             if (!fromuser || !MusicUtils.isConnected()) return;

             long now = SystemClock.elapsedRealtime();
             if ((now - LastSeekEventTime) > 250)
             {
                LastSeekEventTime = now;
                MusicUtils.seek(Duration * progress / Progress_Max);
             }
          }
          public void onStopTrackingTouch(SeekBar bar)
          {
          }
       };

   private long refreshNow()
   {
      // Update progress bar and time display; return milliseconds until next update
      if (Service == null) return 500;

      long pos = MusicUtils.getPosition();

      // Default return the number of milliseconds until the next
      // full second, so the counter can be updated at just the
      // right time.
      long remaining = 1000 - (pos % 1000);

      if (pos >= 0 && Duration > 0)
      {
         Current_Time.setText(MusicUtils.makeTimeString(this, pos));

         if (isPlaying)
         {
            Current_Time.setVisibility(View.VISIBLE);
         } else
         {
            // blink the counter
            int vis = Current_Time.getVisibility();
            Current_Time.setVisibility(vis == View.INVISIBLE ? View.VISIBLE : View.INVISIBLE);
            remaining = 500;
         }

         Progress_Bar.setProgress((int) (Progress_Max * pos / Duration));

      } else
      {
         Current_Time.setText("--:--");
         Progress_Bar.setProgress(Progress_Max);
      }

      return remaining;
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

   private Button.OnClickListener Quit_Listener = new Button.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            if (MusicUtils.isPlaying())
               sendBroadcast(new Intent(Stephes_Music_Service.TOGGLEPAUSE_ACTION));

            finish();
         }
      };
}
