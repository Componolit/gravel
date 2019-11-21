
with Gneiss.Rom;
with Gneiss.Rom.Client;
with Gneiss.Types;

package Config with
   SPARK_Mode
is

   procedure Initialize (Cap     :     Gneiss.Types.Capability;
                         Success : out Boolean);

   type Mode is (Continuous, Sliced);
   type Distribution is (None, Uniform);

   function Get_Delay return Duration;
   function Get_Client_Id return String;
   function Get_Jitter return Duration;
   function Get_Slice return Duration;
   function Get_Jitter_Distribution return Distribution;
   function Get_Mode return Mode;
   function Initialized return Boolean;

private

   procedure Parse (Data : String);

   package Instance is new Gneiss.Rom.Client (Character, Positive, String, Parse);

   function Duration_Value (D : String) return Duration;

   Config_Client  : Gneiss.Rom.Client_Session;
   Request_Delay  : Duration := 0.0;
   Client_Id      : String (1 .. 160) := (others => Character'First);
   Is_Initialized : Boolean;
   Jitter         : Duration;
   J_Distribution : Distribution;
   Op_Mode        : Mode;
   Slice          : Duration;

end Config;
