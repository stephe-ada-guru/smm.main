//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011 - 2013, 2015 - 2019, 2021, 2024 Stephen Leake.  All Rights Reserved.
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

package org.stephe_leake.music_player_2

import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.Intent
import android.content.res.Resources
import android.content.SharedPreferences
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.widget.ImageButton
import android.widget.SeekBar
import android.widget.SeekBar.OnSeekBarChangeListener
import android.widget.TextView
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.preference.EditTextPreferenceDialogFragmentCompat

import org.stephe_leake.music_player_2.PrefActivity
import org.stephe_leake.music_player_2.utils.Companion
import org.stephe_leake.music_player_2.utils.Companion.mainActivity

class MainActivity : ComponentActivity()
{
   // Menu constants
   // Alphabetical order here; display order set in onCreateOptionsMenu
   private val MENU_COPY: Int                   = 1
   private val MENU_NEW_PLAYLIST: Int  = 2
   private val MENU_LINER_NOTES: Int                  = 3
   private val MENU_PREFERENCES: Int            = 4
   private val MENU_QUIT: Int                   = 5
   private val MENU_RESET_PLAYLIST: Int         = 6
   private val MENU_SEARCH: Int                 = 7
   private val MENU_SHARE: Int                  = 8
   private val MENU_SHOW_DOWNLOAD_LOG: Int      = 9
   private val MENU_SHOW_ERROR_LOG: Int         = 10
   private val MENU_UPDATE_PLAYLIST: Int        = 11

   // Main UI members
   // FIXME: delete these; use non-null local 'val's.

   private var playPauseButton: ImageButton? = null
   private var progressBar: SeekBar? = null
   private var album: TextView? = null
   private var albumArtist: TextView? = null
   private var artist: TextView? = null
   private var composer: TextView? = null
   private var currentTime: TextView? = null
   private var playlist: TextView? = null
   private var title: TextView? = null
   private var totalTime: TextView? = null
   private var year: TextView? = null

   
   ////////// Activity lifetime methods (in lifecycle order)

   override fun onCreate(savedInstanceState: Bundle?)
   {
       super.onCreate(savedInstanceState)
       enableEdgeToEdge()

       mainActivity = this

       setContentView(R.layout.mainactivity)


       // Set up displays, top to bottom left to right
       
       album       = utils.findTextViewById(this, R.id.album)
       albumArtist = utils.findTextViewById(this, R.id.albumArtist)
       artist      = utils.findTextViewById(this, R.id.artist)
       composer    = utils.findTextViewById(this, R.id.composer)
       title       = utils.findTextViewById(this, R.id.title)
       year        = utils.findTextViewById(this, R.id.year)
   }

   ////////// Menu

   override fun onCreateOptionsMenu(menu: Menu): Boolean
   {
      super.onCreateOptionsMenu(menu)
      menu.add(0, MENU_QUIT, 0, R.string.menu_quit)
      menu.add(0, MENU_SEARCH, 0, R.string.menu_search)
      menu.add(0, MENU_SHARE, 0, R.string.menu_share)
      menu.add(0, MENU_LINER_NOTES, 0, R.string.menu_liner)
      menu.add(0, MENU_COPY, 0, R.string.menu_copy)
      menu.add(0, MENU_UPDATE_PLAYLIST, 0, R.string.menu_update_playlist)
      menu.add(0, MENU_RESET_PLAYLIST, 0, R.string.menu_reset_playlist)
      menu.add(0, MENU_SHOW_DOWNLOAD_LOG, 0, R.string.menu_show_download_log)
      menu.add(0, MENU_SHOW_ERROR_LOG, 0, R.string.menu_show_error_log)
      menu.add(0, MENU_PREFERENCES, 0, R.string.menu_preferences)
      menu.add(0, MENU_NEW_PLAYLIST, 0, R.string.menu_download_new_playlist)
      return true // display menu
   }

   override fun onPrepareOptionsMenu(menu: Menu): Boolean 
   {
      super.onPrepareOptionsMenu(menu)

      //FIXME: don't have utils yet
      // menu.findItem(MENU_LINER_NOTES).setEnabled(utils.retriever.linerNotesExist())

      return true
   }

   override fun onOptionsItemSelected(item: MenuItem): Boolean
   {
      when (item.getItemId())
      {
      // Alphabetical order

      MENU_COPY ->
      {
         var clipManage: ClipboardManager = getSystemService(CLIPBOARD_SERVICE) as ClipboardManager

         val album       : TextView = utils.findTextViewById(this, R.id.album)
         val albumArtist : TextView = utils.findTextViewById(this, R.id.albumArtist)
         val artist      : TextView = utils.findTextViewById(this, R.id.artist)
         val composer    : TextView = utils.findTextViewById(this, R.id.composer)
         val title       : TextView = utils.findTextViewById(this, R.id.title)
         
         val Msg: String = albumArtist.getText().toString() +
                 " " + artist.getText().toString() +
                 " " + album.getText().toString() +
                 " " + title.getText().toString() +
                 " " + composer.getText().toString()

         clipManage.setPrimaryClip (ClipData.newPlainText ("song", Msg))
      }

      MENU_NEW_PLAYLIST ->
      {
         var res: Resources           = getResources()
         var prefs: SharedPreferences = this.getPreferences(Context.MODE_PRIVATE)
         var serverIP: String?        = prefs.getString (res.getString(R.string.server_IP_key), null)

         if (null == serverIP)
            {
             //FIXME: don't have utils yet
             // utils.alertLog(this, "set Server IP in preferences")
            }
         else
            {
               //FIXME: this is not a preference, just a string
               // var diag: EditTextPreferenceDialogFragmentCompat = EditTextPreferenceDialogFragmentCompat()
               // var args: Bundle = Bundle()
               // args.putInt("command", utils.COMMAND_DOWNLOAD)
               // diag.setArguments(args)
               // diag.show(, "enter new playlist category")
            }
      }

      MENU_LINER_NOTES ->
      {
         //FIXME: retriever not there yet
         // var intent: Intent = Intent(Intent.ACTION_VIEW)
         // .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
         // .addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
         // .setDataAndType(utils.retriever.linerUri, "application/pdf")

         // startActivity(intent)
      }

      MENU_PREFERENCES ->
         // We don't need a result
         this.startActivity(Intent(utils.mainActivity, PrefActivity::class.java))

      MENU_QUIT ->
         {//FIXME: don't have play service yet
         // sendBroadcast
         //   (Intent
         //      (utils.ACTION_PLAY_COMMAND)
         //      .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_QUIT))

         // stopService (Intent().setComponent(playServiceComponentName))

         // finish()
         }

      MENU_RESET_PLAYLIST ->
         {  //FIXME: don't have play service yet
         // sendBroadcast(Intent(utils.ACTION_PLAY_COMMAND)
         //                 .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_RESET_PLAYLIST))
         }
      MENU_SEARCH ->
         {
         //FIXME: start search activity, search local database
         }

      MENU_SHARE ->
      {
         //FIXME: don't have utils yet
         // utils.verboseLog("sharing " + utils.retriever.musicUri.toString())

         //  intent: Intent = Intent()
         // .setAction(Intent.ACTION_SEND)
         // .putExtra(Intent.EXTRA_STREAM, utils.retriever.musicUri)
         // .setType("audio/mp3")

         // startActivity(Intent.createChooser(intent, "Share song via ..."))
      }

      MENU_SHOW_DOWNLOAD_LOG ->
         {  //FIXME: don't have utils yet
         // startActivity(utils.showDownloadLogIntent)
         }

      MENU_SHOW_ERROR_LOG ->
         {  //FIXME: don't have utils yet
         // startActivity(utils.showErrorLogIntent)
         }

      MENU_UPDATE_PLAYLIST ->
      {
         //FIXME: don't have this fragment yet
         // var res: Resources           = getResources()
         // var prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(this)
         // var serverIP: String         = prefs.getString (res.getString(R.string.server_IP_key), null)

         // if (null == serverIP)
         //    //FIXME: don't have utils yet
         //    // utils.alertLog(this, "set Server IP in preferences")
         // else
         // {
         //    //  var diag: PickPlaylistDialogFragment = PickPlaylistDialogFragment()
         //    //  var args: Bundle = Bundle()
         //    // args.putInt("command", utils.COMMAND_DOWNLOAD)
         //    // diag.setArguments(args)
         //    // diag.show(getFragmentManager(), "pick update playlist")
         // }
      }

      else ->
         {//FIXME: don't have utils yet
         // utils.errorLog
         //   (this, "activity.onOptionsItemSelected: unknown MenuItemId " + item.getItemId())
         }
      }
      return false // continue menu processing
   }

}
