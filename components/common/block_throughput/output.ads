
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Timer;

package Output with
   SPARK_Mode
is

   package Gns renames Componolit.Gneiss;

   generic
      type T is range <>;
   function Byte_Image (B : T) return String;

   procedure Info (Log   : in out Gns.Log.Client_Session;
                   Start :        Gns.Timer.Time;
                   Time  :        Gns.Timer.Time;
                   Reqs  :        Long_Integer;
                   Size  :        Long_Integer);

   procedure Xml_Start (Log        : in out Gns.Log.Client_Session;
                        Req_Size   :        Long_Integer;
                        Total_Size :        Long_Integer;
                        Operation  :        String;
                        Buf_Size   :        Long_Integer;
                        Cache_Size :        Long_Integer);

   procedure Xml_Element (Log   : in out Gns.Log.Client_Session;
                          Start :        Gns.Timer.Time;
                          Time  :        Gns.Timer.Time;
                          Reqs  :        Long_Integer);

   procedure Xml_End (Log : in out Gns.Log.Client_Session);

end Output;
