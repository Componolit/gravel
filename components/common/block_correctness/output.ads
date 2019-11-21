
with Gneiss.Log;
with Gneiss.Timer;

package Output with
   SPARK_Mode
is
   use type Gneiss.Timer.Time;

   procedure Progress (Prefix :        String;
                       Done   :        Long_Integer;
                       Todo   :        Long_Integer;
                       Size   :        Long_Integer;
                       Start  :        Gneiss.Timer.Time;
                       Now    :        Gneiss.Timer.Time;
                       Last   : in out Gneiss.Timer.Time;
                       Log    : in out Gneiss.Log.Client_Session) with
     Pre  => Gneiss.Log.Initialized (Log)
             and Prefix'Length <= 50
             and Prefix'First = 1
             and Done >= 0
             and Todo >= 0
             and Size >= 0,
     Post => Gneiss.Log.Initialized (Log);

   function Byte_Image (Bytes : Long_Integer) return String with
     Post => Byte_Image'Result'Length < 25;

end Output;
