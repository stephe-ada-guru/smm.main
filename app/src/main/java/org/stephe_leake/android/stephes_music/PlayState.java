//  Abstract :
//
//  PlayState enum
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

enum PlayState
{
   Idle(0),
   //  Media_Player has no song Loaded

   Playing(1),
   //  Media_Player has song, is playing it

   Paused(2),
   //  Media_Player has song, is not playing it, at request of user

   Paused_Transient(3);
   //  Media_Player has song, is not playing it, at request of
   //  system (phone call, navigation announcement, etc).

   private int index;
   private PlayState(int i) {index = i;}

   int toInt() {return index;}

   static PlayState toPlayState(int i)
   {
      switch(i)
      {
      case 0 : return Idle;
      case 1 : return Playing;
      case 2 : return Paused;
      case 3 : return Paused_Transient;
      default: throw new IllegalArgumentException("invalid PlayState index " + i);
      }
   }
};
