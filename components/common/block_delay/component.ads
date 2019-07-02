
with Componolit.Interfaces.Component;
with Componolit.Interfaces.Types;

package Component with
   SPARK_Mode
is
   package Cai renames Componolit.Interfaces;

   procedure Construct (Cap : Cai.Types.Capability);
   procedure Destruct;

   package Main is new Cai.Component (Construct, Destruct);

end Component;
