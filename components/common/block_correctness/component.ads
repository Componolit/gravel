
with Componolit.Gneiss.Types;
with Componolit.Gneiss.Component;

package Component with
   SPARK_Mode
is
   package Cai renames Componolit.Gneiss;

   Initialized : Boolean with Ghost;

   procedure Construct (Cap : Cai.Types.Capability);
   procedure Destruct;

   package Main is new Cai.Component (Construct, Destruct);

   procedure Event with
      Pre => Initialized;

end Component;
