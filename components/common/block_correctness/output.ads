
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Timer;

package Output with
   SPARK_Mode
is
   package Cai renames Componolit.Gneiss;

   use type Cai.Timer.Time;

   procedure Progress (Prefix :        String;
                       Done   :        Long_Integer;
                       Todo   :        Long_Integer;
                       Size   :        Long_Integer;
                       Start  :        Cai.Timer.Time;
                       Now    :        Cai.Timer.Time;
                       Last   : in out Cai.Timer.Time;
                       Log    : in out Cai.Log.Client_Session) with
     Pre  => Cai.Log.Initialized (Log)
             and Prefix'Length <= 50
             and Prefix'First = 1
             and Done >= 0
             and Todo >= 0
             and Size >= 0,
     Post => Cai.Log.Initialized (Log);

   function Byte_Image (Bytes : Long_Integer) return String with
     Post => Byte_Image'Result'Length < 25;

end Output;
