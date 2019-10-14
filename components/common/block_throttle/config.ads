
package Config with
   SPARK_Mode
is

   procedure Parse (D : String);

   function Initialized return Boolean;

   function Device return String;

   function Rate return Natural;

   function Frequency return Natural;

   function Reason return String;

private

   type Reason_Type is (Success,
                        Uninitialized,
                        Invalid_Content,
                        Invalid_Xml,
                        Open_Fail,
                        Invalid_Path,
                        Miss_Attr_Device,
                        Miss_Attr_Reqs,
                        Miss_Attr_Freq);

   function Parse_Natural (S : String) return Natural;

   Ready : Boolean           := False;
   Dev   : String (1 .. 256) := (others => Character'First);
   Freq  : Natural           := 0;
   Reqs  : Natural           := 0;
   Rsn   : Reason_Type       := Uninitialized;

end Config;
