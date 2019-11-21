
with Gneiss.Log.Client;
with Basalt.Strings;

package body Output with
   SPARK_Mode
is

   function Remain (S : Gneiss.Timer.Time;
                    C : Gneiss.Timer.Time;
                    P : Long_Integer) return Duration;

   function Remain (S : Gneiss.Timer.Time;
                    C : Gneiss.Timer.Time;
                    P : Long_Integer) return Duration
   is
      T : Gneiss.Timer.Time;
   begin
      if P < 1 or P > 999 then
         return 0.0;
      end if;
      if
         (S > 0.0 and then Gneiss.Timer.Time'First + S <= C)
         or (S < 0.0 and then Gneiss.Timer.Time'Last + S >= C)
      then
         T := ((C - S) / Integer (P));
         if
            (T > 0.0 and then Gneiss.Timer.Time'Last / Integer (1000 - P) > T)
            or (T < 0.0 and then Gneiss.Timer.Time'Last / Integer (1000 - P) < T)
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
         return Basalt.Strings.Image (Bytes / 1024 ** 2) & " MiB";
      else
         return Basalt.Strings.Image (Bytes / 1024 ** 3) & " GiB";
      end if;
   end Byte_Image;

   procedure Progress (Prefix :        String;
                       Done   :        Long_Integer;
                       Todo   :        Long_Integer;
                       Size   :        Long_Integer;
                       Start  :        Gneiss.Timer.Time;
                       Now    :        Gneiss.Timer.Time;
                       Last   : in out Gneiss.Timer.Time;
                       Log    : in out Gneiss.Log.Client_Session)
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
         (Last > 0.0 and then Gneiss.Timer.Time'First + Last > Now)
         or (Last < 0.0 and then Gneiss.Timer.Time'Last + Last < Now)
      then
         Last := Now;
      end if;
      if Duration (Now - Last) > Duration (2) then
         Last     := Now;
         P := (if Todo > 999 then Done / (Todo / 1000) else 0);
         Gneiss.Log.Client.Info (Log, Prefix & "... ("
                                          & Basalt.Strings.Image (P / 10)
                                          & "."
                                          & Basalt.Strings.Image (P rem 10)
                                          & "%, " & Byte_Image (Safe_Multiply (Done, Size))
                                          & " / " & Byte_Image (Safe_Multiply (Todo, Size))
                                          & ")");
         if
            (Start > 0.0 and then Gneiss.Timer.Time'First + Start <= Now)
            or (Start < 0.0 and then Gneiss.Timer.Time'Last + Start >= Now)
         then
            Gneiss.Log.Client.Info (Log, "Elapsed: "
                                      & Basalt.Strings.Image (Duration (Now - Start))
                                      & " Remaining: "
                                      & Basalt.Strings.Image (Remain (Start, Now, P)));
         end if;
      end if;
   end Progress;

end Output;
