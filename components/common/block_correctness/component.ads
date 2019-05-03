
with Cai.Types;
with Cai.Component;

package Component with
   SPARK_Mode
is

   procedure Construct (Cap : Cai.Types.Capability);
   procedure Destruct;

   package Correctness_Test is new Cai.Component (Construct, Destruct);

   procedure Event;

end Component;
