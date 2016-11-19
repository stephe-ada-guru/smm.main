//  Abstract :
//
//  Utilities for syncing with smm_server
//
//  Copyright (C) 2016 Stephen Leake. All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or
//  modify it under terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 3, or (at
//  your option) any later version. This program is distributed in the
//  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
//  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
//  PURPOSE. See the GNU General Public License for more details. You
//  should have received a copy of the GNU General Public License
//  distributed with this program; see file COPYING. If not, write to
//  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
//  MA 02110-1335, USA.

package org.stephe_leake.android.stephes_music;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.nio.CharBuffer;

public class SyncUtils
{
   static void editPlaylist(String playlistFilename, String lastFilename)
   // Delete lines from start of playlist file up to and including
   // line in last file. Delete last file.
   {
      try
      {
         String           outputFilename = playlistFilename + ".tmp";
         LineNumberReader input          = new LineNumberReader(new FileReader(lastFilename));
         FileWriter       output         = new FileWriter(outputFilename);
         String           lastPlayed     = input.readLine();
         boolean          found          = false;
         boolean          done           = false;

         input.close();

         if (null == lastPlayed)
         {
            // last file is empty; nothing to delete
            output.close();
            new File(lastFilename).delete();
            return;
         }

         input = new LineNumberReader(new FileReader(playlistFilename));

         while (!done)
         {
            String line = input.readLine();

            if (line == null)
               done = true;
            else
            {
               if (found)
                  output.write(line + "\n");
               else
                  found = line.equals(lastPlayed);
            }
         }
         input.close();
         output.close();

         {
            File playlistFile = new File(playlistFilename);
            File outputFile   = new File(outputFilename);

            playlistFile.delete();
            new File(lastFilename).delete();
            outputFile.renameTo(playlistFile);
         }
      }
      catch (FileNotFoundException e)
      { // last file not found; same as empty; do nothing
      }
      catch (IOException e)
      { // something really screwed up
      }
   }
}
