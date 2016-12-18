//  Abstract :
//
//  Simple text body for http 'put' request.
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

import java.io.IOException;
import okhttp3.RequestBody;
import okhttp3.MediaType;

class TextBody extends RequestBody
{
   private String data;

   TextBody(String data)
   // Create an object containing 'data'
   {
      this.data = data;
   }

   @Override
   public MediaType contentType()
   {
      return MediaType.parse("text/plain");
   }

   @Override
   public long contentLength()
   {
      return data.length();
   }

   @Override
   public void writeTo(okio.BufferedSink sink)
      throws IOException
   {
      sink.write(data.getBytes());
   }
}
