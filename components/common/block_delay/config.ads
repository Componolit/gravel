
with Componolit.Gneiss.Rom;
with Componolit.Gneiss.Rom.Client;
with Componolit.Gneiss.Types;

package Config with
   SPARK_Mode
is

   package Cai renames Componolit.Gneiss;

   procedure Initialize (Cap     :     Cai.Types.Capability;
                         Success : out Boolean);

   type Distribution is (None, Uniform);

   function Get_Delay return Duration;
   function Get_Client_Id return String;
   function Get_Jitter return Duration;
   function Get_Jitter_Distribution return Distribution;
   function Initialized return Boolean;

private

   procedure Parse (Data : String);

   package Instance is new Cai.Rom.Client (Character, Positive, String, Parse);

   function Duration_Value (D : String) return Duration;

   Config_Client  : Cai.Rom.Client_Session;
   Request_Delay  : Duration := 0.0;
   Client_Id      : String (1 .. 160) := (others => Character'First);
   Is_Initialized : Boolean;
   Jitter         : Duration;
   J_Distribution : Distribution;

end Config;
