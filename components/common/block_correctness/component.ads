
with Gneiss.Types;
with Gneiss.Component;

package Component with
   SPARK_Mode
is

   Initialized : Boolean with Ghost;

   procedure Construct (Cap : Gneiss.Types.Capability);
   procedure Destruct;

   package Main is new Gneiss.Component (Construct, Destruct);

   procedure Event with
      Pre => Initialized;

end Component;
