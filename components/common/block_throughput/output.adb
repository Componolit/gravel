
with Gneiss.Log.Client;
with Basalt.Strings;
with Basalt.Strings_Generic;

package body Output with
   SPARK_Mode
is

   function Byte_Image (B : T) return String
   is
      function Img is new Basalt.Strings_Generic.Image_Ranged (T);
   begin
      if B < 1024 * 10 then
         return Img (B) & " B";
      elsif B < (1024 ** 2) * 10 then
         return Img (B / 1024) & " KiB";
      elsif B < (1024 ** 3) * 10 then
         return Img (B / 1024 ** 2) & " MiB";
      else
         return Img (B / 1024 ** 3) & " GiB";
      end if;
   end Byte_Image;

   procedure Info (Log   : in out Gneiss.Log.Client_Session;
                   Start :        Gneiss.Timer.Time;
                   Time  :        Gneiss.Timer.Time;
                   Reqs  :        Long_Integer;
                   Size  :        Long_Integer)
   is
      use type Gneiss.Timer.Time;
      function Img is new Byte_Image (Long_Integer);
      Secs   : constant Long_Integer := Long_Integer (Time - Start);
      R_Rate : Long_Integer;
      D_Rate : Long_Integer;
   begin
      if Secs > 0 then
         R_Rate := Reqs / Secs;
         D_Rate := (Reqs * Size) / Secs;
      else
         R_Rate := 0;
         D_Rate := 0;
      end if;
      Gneiss.Log.Client.Info (Log, "Rate: " & Basalt.Strings.Image (R_Rate)
                                & " r/s (" & Img (D_Rate) & "/s) "
                                & Img (Reqs * Size));
   end Info;

   procedure Xml_Start (Log        : in out Gneiss.Log.Client_Session;
                        Req_Size   :        Long_Integer;
                        Total_Size :        Long_Integer;
                        Operation  :        String;
                        Buf_Size   :        Long_Integer;
                        Cache_Size :        Long_Integer)
   is
   begin
      Gneiss.Log.Client.Info (Log, "<test request_size="""
                                & Basalt.Strings.Image (Req_Size)
                                & """ total_size="""
                                & Basalt.Strings.Image (Total_Size)
                                & """ operation="""
                                & Operation
                                & """ buffer_size="""
                                & Basalt.Strings.Image (Buf_Size)
                                & """ cache_size="""
                                & Basalt.Strings.Image (Cache_Size)
                                & """>");
   end Xml_Start;

   procedure Xml_Element (Log   : in out Gneiss.Log.Client_Session;
                          Start :        Gneiss.Timer.Time;
                          Time  :        Gneiss.Timer.Time;
                          Reqs  :        Long_Integer)
   is
      use type Gneiss.Timer.Time;
   begin
      Gneiss.Log.Client.Info (Log, "  <sample time="""
                                & Basalt.Strings.Image (Duration (Time - Start))
                                & """ reqs="""
                                & Basalt.Strings.Image (Reqs)
                                & """/>");
   end Xml_Element;

   procedure Xml_End (Log : in out Gneiss.Log.Client_Session)
   is
   begin
      Gneiss.Log.Client.Info (Log, "</test>");
   end Xml_End;

end Output;
