
with Componolit.Interfaces.Block;
with Componolit.Interfaces.Component;
with Componolit.Interfaces.Types;

package Component with
   SPARK_Mode
is

   type Request_Id is mod 2 ** 6;

   type Byte is mod 256;
   type Index is new Positive;
   type Buffer is array (Index range <>) of Byte;

   package Cai renames Componolit.Interfaces;

   procedure Construct (C : Cai.Types.Capability);
   procedure Destruct;
   procedure Event;

   package Main is new Cai.Component (Construct, Destruct);
   package Block is new Componolit.Interfaces.Block (Byte, Index, Buffer);

end Component;
