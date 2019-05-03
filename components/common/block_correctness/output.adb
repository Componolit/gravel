
with Cai.Log.Client;

package body Output with
   SPARK_Mode
is

   use type Cai.Timer.Time;

   function Remain (S : Cai.Timer.Time;
                    C : Cai.Timer.Time;
                    P : Long_Integer) return Duration;

   function Remain (S : Cai.Timer.Time;
                    C : Cai.Timer.Time;
                    P : Long_Integer) return Duration
   is
   begin
      if P < 1 or P > 999 then
         return Duration (0);
      end if;
      return Duration (((C - S) / Integer (P)) * Integer (1000 - P));
   end Remain;

   function Byte_Image (Bytes : Long_Integer) return String
   is
   begin
      if
         Bytes < (1024 ** 3) * 10
      then
         return Cai.Log.Image (Bytes / 1024 ** 2) & " MiB";
      else
         return Cai.Log.Image (Bytes / 1024 ** 3) & " GiB";
      end if;
   end Byte_Image;

   procedure Progress (Prefix :        String;
                       Done   :        Long_Integer;
                       Todo   :        Long_Integer;
                       Size   :        Long_Integer;
                       Start  :        Cai.Timer.Time;
                       Now    :        Cai.Timer.Time;
                       Last   : in out Cai.Timer.Time;
                       Log    : in out Cai.Log.Client_Session)
   is
      Progress : Long_Integer := -1;
   begin
      if Duration (Now - Last) > Duration (2) then
         Last     := Now;
         Progress := Done / (Todo / 1000);
         Cai.Log.Client.Info (Log, Prefix & "... ("
                                   & Cai.Log.Image (Progress / 10)
                                   & "."
                                   & Cai.Log.Image (Progress rem 10)
                                   & "%, " & Byte_Image (Done * Size)
                                   & " / " & Byte_Image (Todo * Size)
                                   & ")");
         Cai.Log.Client.Info (Log, "Elapsed: "
                                   & Cai.Log.Image (Duration (Now - Start))
                                   & " Remaining: "
                                   & Cai.Log.Image (Remain (Start, Now, Progress)));
      end if;
   end Progress;

end Output;
