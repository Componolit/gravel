
with Gneiss.Block;
with Gneiss.Component;
with Gneiss.Types;

package Component with
   SPARK_Mode
is

   type Request_Id is mod 2 ** 6;
   type Session_Id is new Boolean;

   type Byte is mod 256;
   type Index is new Positive;
   type Buffer is array (Index range <>) of Byte;

   procedure Construct (C : Gneiss.Types.Capability);
   procedure Destruct;
   procedure Event;

   package Main is new Gneiss.Component (Construct, Destruct);
   package Block is new Gneiss.Block (Byte, Index, Buffer, Session_Id, Request_Id);

end Component;
