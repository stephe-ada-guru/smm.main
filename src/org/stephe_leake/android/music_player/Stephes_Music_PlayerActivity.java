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
import android.media.MediaScannerConnection;
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
         handleStartIntent(intent, false);
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
         MusicUtils.Error_Log(this, "unexpected intent " + intent);
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
      Cursor       cursor      = Content_Resolver.query(contentURI, columns, where, selection, null);

      // cursor is before first result, or null
      if (cursor != null && cursor.getCount() > 0)
      {
         if (cursor.getCount() > 1)
            MusicUtils.Error_Log(Stephes_Music_PlayerActivity.this, Item_URI + ": multiple matches in database");

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

         MusicUtils.addToCurrentPlaylist
            (this,
             new long[]{Get_ID(contentURI, Song_URI)},
             MusicUtils.isPlaying() ? Stephes_Music_Service.LAST : Stephes_Music_Service.NOW);
      }
      catch (Not_Found e)
      {
         // FIXME: scan here? the media scanner runs when sdcard is
         // unmounted, but there are other ways to get files on the
         // sdcard (via wifi or phone)
         MusicUtils.Info_Log(this, e.getMessage());
      }
      catch (RuntimeException e)
      {
         MusicUtils.Error_Log(this, "Add_Song: " + e.toString());
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
         MusicUtils.replaceCurrentPlaylist (Volume_Name, Get_ID(contentURI, listURI));
      } catch (Not_Found e)
      {
         if (alreadyScanned)
         {
            MusicUtils.Error_Log(this, listURI.toString() + " not found after scan");
         } else
         {
            // A brand new playlist; scan it
            MusicUtils.debugLog("new playlist; scanning " + listURI.toString());
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
         MusicUtils.debugLog("scan complete; got " + uri.toString());
         MusicUtils.debugLog(" ... starting " + getIntent().toString());
         handleStartIntent(getIntent(), true);
      }
   };

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

      if (pos >= 0 && Duration > 0)
      {
         Current_Time.setText(MusicUtils.makeTimeString(this, pos));
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
