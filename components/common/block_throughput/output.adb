
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Strings;
with Componolit.Gneiss.Strings_Generic;

package body Output with
   SPARK_Mode
is

   function Byte_Image (B : T) return String
   is
      function Img is new Gns.Strings_Generic.Image_Ranged (T);
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

   procedure Info (Log   : in out Gns.Log.Client_Session;
                   Start :        Gns.Timer.Time;
                   Time  :        Gns.Timer.Time;
                   Reqs  :        Long_Integer;
                   Size  :        Long_Integer)
   is
      use type Gns.Timer.Time;
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
      Gns.Log.Client.Info (Log, "Rate: " & Gns.Strings.Image (R_Rate)
                                & " r/s (" & Img (D_Rate) & "/s) "
                                & Img (Reqs * Size));
   end Info;

   procedure Xml_Start (Log        : in out Gns.Log.Client_Session;
                        Req_Size   :        Long_Integer;
                        Total_Size :        Long_Integer;
                        Operation  :        String;
                        Buf_Size   :        Long_Integer;
                        Cache_Size :        Long_Integer)
   is
   begin
      Gns.Log.Client.Info (Log, "<test request_size="""
                                & Gns.Strings.Image (Req_Size)
                                & """ total_size="""
                                & Gns.Strings.Image (Total_Size)
                                & """ operation="""
                                & Operation
                                & """ buffer_size="""
                                & Gns.Strings.Image (Buf_Size)
                                & """ cache_size="""
                                & Gns.Strings.Image (Cache_Size)
                                & """>");
   end Xml_Start;

   procedure Xml_Element (Log   : in out Gns.Log.Client_Session;
                          Start :        Gns.Timer.Time;
                          Time  :        Gns.Timer.Time;
                          Reqs  :        Long_Integer)
   is
      use type Gns.Timer.Time;
   begin
      Gns.Log.Client.Info (Log, "  <sample time="""
                                & Gns.Strings.Image (Duration (Time - Start))
                                & """ reqs="""
                                & Gns.Strings.Image (Reqs)
                                & """/>");
   end Xml_Element;

   procedure Xml_End (Log : in out Gns.Log.Client_Session)
   is
   begin
      Gns.Log.Client.Info (Log, "</test>");
   end Xml_End;

end Output;
