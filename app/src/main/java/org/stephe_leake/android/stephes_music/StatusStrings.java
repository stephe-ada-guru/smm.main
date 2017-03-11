package org.stephe_leake.android.stephes_music;

public class StatusStrings
{
   public ProcessStatus status; // True if caller should proceed to next step in playlist update.
   public String[] strings;

   StatusStrings ()
   {
      String[] empty = {};

      this.status  = ProcessStatus.Success;
      this.strings = empty;
   }
}
