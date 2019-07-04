
with Componolit.Interfaces.Rom;
with Componolit.Interfaces.Rom.Client;
with Componolit.Interfaces.Types;

package Config with
   SPARK_Mode
is

   package Cai renames Componolit.Interfaces;

   procedure Initialize (Cap     :     Cai.Types.Capability;
                         Success : out Boolean);

   function Get_Delay return Duration;
   function Get_Client_Id return String;
   function Initialized return Boolean;

private

   Request_Delay  : Duration := 0.0;
   Client_Id      : String (1 .. 160) := (others => Character'First);
   Is_Initialized : Boolean;

   procedure Parse (Data : String);

   package Instance is new Cai.Rom.Client (Character, Positive, String, Parse);

   Config_Client : Cai.Rom.Client_Session := Instance.Create;

end Config;
