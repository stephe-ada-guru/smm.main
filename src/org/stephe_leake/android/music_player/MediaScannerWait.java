// Abstract:
//
// Thread to perform MediaScanner opertions, and wait for their completion.
//
// Copyright 2011 Stephen Leake stephen_leake@stephe-leake.org
//
// This program is made available under the GNU GPL version 3.0 or
// greater. See the accompanying file COPYING for details.
//
// This program is distributed WITHOUT ANY WARRANTY; without even the
// implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.

package org.stephe_leake.android.music_player;

import java.util.concurrent.locks.ReentrantLock;

import android.content.Context;
import android.media.MediaScannerConnection;
import android.net.Uri;

class MediaScannerWait
{
   private final ReentrantLock lock = new ReentrantLock();
   private int                 scanCompleteCount;
   private final int           expectedCount;
   private boolean             success;
   private Context             context;

   // Start a scan in the scanner service thread. User should then call
   // scanComplete to wait for completion and get the result.
   public MediaScannerWait(Context context, String[] paths, String[] mimeTypes)
   {
      this.context      = context;
      scanCompleteCount = 0;
      success           = true;
      expectedCount     = paths.length;
      lock.lock();

      try
      {
         MediaScannerConnection.scanFile(context, paths, mimeTypes, Listener);
      }
      catch (Exception e)
      {
         success = false;
         lock.unlock();
         MusicUtils.Error_Log (context, "MediaScannerWait: " + e.toString());
      }
   }

   // Blocked until all paths have been scanned. Returns true if all
   // scans were successful, false otherwise.
   public boolean scanComplete()
   {
      MusicUtils.debugLog("MediaScannerWait.scanComplete: locked " + String.valueOf(lock.isLocked()));
      lock.lock(); // waits until Listener unlocks (or constructor throws an exception)
      lock.unlock();
      MusicUtils.debugLog("MediaScannerWait.scanComplete: count " + String.valueOf(scanCompleteCount));
      return success;
   }

   private MediaScannerConnection.OnScanCompletedListener Listener = new MediaScannerConnection.OnScanCompletedListener()
   {
      public void onScanCompleted (String path, Uri uri)
      {
         if (uri == null)
         {
            success = false;
            MusicUtils.Error_Log (context, "MediaScannerWait.onScanCompleted: null uri from " + path);
         }
         else
         {
            MusicUtils.Error_Log (context, "MediaScannerWait.onScanCompleted: uri from " + path + ": " + uri.toString());
         }

         scanCompleteCount++;

         if (scanCompleteCount == expectedCount)
         {
            MusicUtils.debugLog("MediaScannerWait.onScanCompleted: unlocking");
            lock.unlock();
         }
      }
   };
}
