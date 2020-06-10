
with Gneiss;
with Gneiss.Component;

package Socat.Component with
   SPARK_Mode
is

   procedure Construct (Cap : Gneiss.Capability);

   procedure Destruct;

   package Main is new Gneiss.Component (Construct, Destruct);

end Socat.Component;
