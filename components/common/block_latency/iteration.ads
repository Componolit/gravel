with Gneiss.Types;
with Gneiss.Block;
with Gneiss.Block.Client;
with Gneiss.Log;
with Gneiss.Timer;

generic
   with package Block is new Gneiss.Block (<>);
   with package Client is new Block.Client (<>);
   Request_Count : Block.Count;
   Operation     : Block.Request_Kind;
package Iteration
is

   use all type Block.Id;
   use all type Block.Count;

   type Request is record
      Start   : Gneiss.Timer.Time;
      Finish  : Gneiss.Timer.Time;
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

   type Request_Cache is array (Block.Request_Id'Range) of Client.Request;

   Cache : Request_Cache;

   procedure Initialize (T      : out Test;
                         Offset :     Block.Count;
                         S      :     Boolean;
                         Cap    :     Gneiss.Types.Capability);

   procedure Send (C   : in out Block.Client_Session;
                   T   : in out Test;
                   Log : in out Gneiss.Log.Client_Session);

   procedure Receive (C   : in out Block.Client_Session;
                      T   : in out Test;
                      Log : in out Gneiss.Log.Client_Session);

   procedure Xml (Xml_Log : in out Gneiss.Log.Client_Session;
                  B       :        Burst;
                  Offset  :        Block.Count);

end Iteration;
