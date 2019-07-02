with Componolit.Interfaces.Types;
with Componolit.Interfaces.Block;
with Componolit.Interfaces.Block.Client;
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Timer;

generic
   with package Block is new Componolit.Interfaces.Block (<>);
   with package Client is new Block.Client (<>);
   Request_Count : Block.Count;
   Operation     : Block.Request_Kind;
package Iteration
is
   package Cai renames Componolit.Interfaces;

   use all type Block.Id;
   use all type Block.Count;

   type Request is record
      Start   : Cai.Timer.Time;
      Finish  : Cai.Timer.Time;
      Success : Boolean;
   end record;

   type Burst is array (Long_Integer range <>) of Request;

   type Test is limited record
      Sent      : Long_Integer;
      Received  : Long_Integer;
      Offset    : Block.Count;
      Finished  : Boolean;
      Sync      : Boolean;
      Data      : Burst (0 .. Long_Integer (Request_Count - 1));
   end record;

   procedure Initialize (T      : out Test;
                         Offset :     Block.Count;
                         S      :     Boolean;
                         Cap    :     Cai.Types.Capability);

   procedure Send (C   : in out Block.Client_Session;
                   T   : in out Test;
                   Log : in out Cai.Log.Client_Session);

   procedure Receive (C   : in out Block.Client_Session;
                      T   : in out Test;
                      Log : in out Cai.Log.Client_Session);

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session;
                  B       :        Burst;
                  Offset  :        Block.Count);

end Iteration;
