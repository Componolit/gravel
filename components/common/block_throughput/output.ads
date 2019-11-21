
with Gneiss.Log;
with Gneiss.Timer;

package Output with
   SPARK_Mode
is

   generic
      type T is range <>;
   function Byte_Image (B : T) return String;

   procedure Info (Log   : in out Gneiss.Log.Client_Session;
                   Start :        Gneiss.Timer.Time;
                   Time  :        Gneiss.Timer.Time;
                   Reqs  :        Long_Integer;
                   Size  :        Long_Integer);

   procedure Xml_Start (Log        : in out Gneiss.Log.Client_Session;
                        Req_Size   :        Long_Integer;
                        Total_Size :        Long_Integer;
                        Operation  :        String;
                        Buf_Size   :        Long_Integer;
                        Cache_Size :        Long_Integer);

   procedure Xml_Element (Log   : in out Gneiss.Log.Client_Session;
                          Start :        Gneiss.Timer.Time;
                          Time  :        Gneiss.Timer.Time;
                          Reqs  :        Long_Integer);

   procedure Xml_End (Log : in out Gneiss.Log.Client_Session);

end Output;
