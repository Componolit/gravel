
with Cai.Types;
with Cai.Component;

package Component with
   SPARK_Mode
is

   Initialized : Boolean with Ghost;

   procedure Construct (Cap : Cai.Types.Capability);
   procedure Destruct;

   package Correctness_Test is new Cai.Component (Construct, Destruct);

   procedure Event with
      Pre => Initialized;

end Component;
