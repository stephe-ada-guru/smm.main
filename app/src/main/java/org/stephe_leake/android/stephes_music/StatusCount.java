package org.stephe_leake.android.stephes_music;

public class StatusCount
{
   public ProcessStatus status; // True if caller should proceed to next step in playlist update.
   public Integer count; // songs cleaned/downloaded/whatever

   StatusCount (ProcessStatus status, Integer count)
   {
      this.status = status;
      this.count = count;
   }
}
