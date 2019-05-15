
package body Output with
   SPARK_Mode
is

   function Remain (S : Cai.Timer.Time;
                    C : Cai.Timer.Time;
                    P : Long_Integer) return Duration;

   function Remain (S : Cai.Timer.Time;
                    C : Cai.Timer.Time;
                    P : Long_Integer) return Duration
   is
      T : Cai.Timer.Time;
   begin
      if P < 1 or P > 999 then
         return 0.0;
      end if;
      if
         (S > 0.0 and then Cai.Timer.Time'First + S <= C)
         or (S < 0.0 and then Cai.Timer.Time'Last + S >= C)
      then
         T := ((C - S) / Integer (P));
         if
            (T > 0.0 and then Cai.Timer.Time'Last / Integer (1000 - P) > T)
            or (T < 0.0 and then Cai.Timer.Time'Last / Integer (1000 - P) < T)
         then
            return Duration (T * Integer (1000 - P));
         else
            return 0.0;
         end if;
      else
         return 0.0;
      end if;
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
      function Safe_Multiply (L, R : Long_Integer) return Long_Integer with
         Pre => L >= 0 and R >= 0;
      function Safe_Multiply (L, R : Long_Integer) return Long_Integer
      is
      begin
         if L = 0 or R = 0 then
            return 0;
         end if;
         if Long_Integer'Last / R > L then
            return R * L;
         else
            return Long_Integer'Last;
         end if;
      end Safe_Multiply;
      P : Long_Integer;
   begin
      if
         (Last > 0.0 and then Cai.Timer.Time'First + Last > Now)
         or (Last < 0.0 and then Cai.Timer.Time'Last + Last < Now)
      then
         Last := Now;
      end if;
      if Duration (Now - Last) > Duration (2) then
         Last     := Now;
         P := (if Todo > 999 then Done / (Todo / 1000) else 0);
         declare
            S : constant String := Prefix & "... ("
                                          & Cai.Log.Image (P / 10)
                                          & "."
                                          & Cai.Log.Image (P rem 10)
                                          & "%, " & Byte_Image (Safe_Multiply (Done, Size))
                                          & " / " & Byte_Image (Safe_Multiply (Todo, Size))
                                          & ")";
         begin
            if S'Length > Cai.Log.Client.Maximum_Message_Length (Log) then
               Cai.Log.Client.Info (Log, S (S'First  .. S'First + Cai.Log.Client.Maximum_Message_Length (Log) - 2));
               Cai.Log.Client.Info (Log, S (S'First + Cai.Log.Client.Maximum_Message_Length (Log) - 1 .. S'Last));
            else
               Cai.Log.Client.Info (Log, S);
            end if;
         end;
         if
            (Start > 0.0 and then Cai.Timer.Time'First + Start <= Now)
            or (Start < 0.0 and then Cai.Timer.Time'Last + Start >= Now)
         then
            Cai.Log.Client.Info (Log, "Elapsed: "
                                      & Cai.Log.Image (Duration (Now - Start))
                                      & " Remaining: "
                                      & Cai.Log.Image (Remain (Start, Now, P)));
         end if;
      end if;
   end Progress;

end Output;
