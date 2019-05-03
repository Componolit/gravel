
with Cai.Log;
with Cai.Timer;

package Output with
   SPARK_Mode
is

   procedure Progress (Prefix :        String;
                       Done   :        Long_Integer;
                       Todo   :        Long_Integer;
                       Size   :        Long_Integer;
                       Start  :        Cai.Timer.Time;
                       Now    :        Cai.Timer.Time;
                       Last   : in out Cai.Timer.Time;
                       Log    : in out Cai.Log.Client_Session);

   function Byte_Image (Bytes : Long_Integer) return String;

end Output;
