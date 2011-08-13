package org.stephe_leake.music_player;

import java.io.IOException;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;
import android.media.MediaMetadataRetriever;

public class Stephes_Music_PlayerActivity extends Activity 
{
    private TextView Song_Title;
    private TextView Album_Title;
    private TextView Artist_Title;
    private android.media.MediaPlayer Media_Player;

    private String Song_URI = "/mnt/sdcard/Audio/Vocal/01 - Fill Me Up.mp3";

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) 
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        Song_Title = (TextView) findViewById(R.id.Song_Title);
    	Album_Title = (TextView) findViewById(R.id.Album_Title);
    	Artist_Title = (TextView) findViewById(R.id.Artist_Title);
    	
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
    		
    	Media_Player = new android.media.MediaPlayer();
        
    	try
    	{
    		Media_Player.setDataSource(Song_URI);
    		Media_Player.prepare();
    	}
    	catch (IOException e) {
			// from Player.setDataSource
    		Song_Title.setText(Song_URI + " CANNOT BE PLAYED : " + e.getMessage());
    		return;
		}
    	
    	Media_Player.start();
        	
    	Song_Title.setText(Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_TITLE));
    	
    	Album_Title.setText(Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ALBUM));
    	
    	Artist_Title.setText
    	(Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ALBUMARTIST) +
    			Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_ARTIST) +
    			Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_AUTHOR) +
    			Meta.extractMetadata(android.media.MediaMetadataRetriever.METADATA_KEY_COMPOSER));
        	
    }
}