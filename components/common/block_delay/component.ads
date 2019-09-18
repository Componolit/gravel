
with Componolit.Gneiss.Component;
with Componolit.Gneiss.Types;

package Component with
   SPARK_Mode
is
   package Cai renames Componolit.Gneiss;

   procedure Construct (Cap : Cai.Types.Capability);
   procedure Destruct;

   package Main is new Cai.Component (Construct, Destruct);

end Component;
