
with Gneiss.Types;
with Gneiss.Block;
with Gneiss.Block.Client;
with Gneiss.Log;
with Iteration;

generic
   with package Block is new Gneiss.Block (<>);
   with package Client is new Block.Client (<>);
   Request_Count : Block.Count;
   Run_Count     : Positive;
   Operation     : Block.Request_Kind;
package Run is

   package Iter is new Iteration (Block, Client, Request_Count, Operation);

   type Run_Type is array (1 .. Run_Count) of Iter.Test;

   procedure Initialize (R : out Run_Type;
                         S :     Boolean;
                         C :     Gneiss.Types.Capability);

   procedure Run (C   : in out Block.Client_Session;
                  R   : in out Run_Type;
                  Log : in out Gneiss.Log.Client_Session);

   function Finished (R : Run_Type) return Boolean;

   procedure Xml (Xml_Log : in out Gneiss.Log.Client_Session;
                  R       :        Run_Type;
                  Cold    :        Boolean;
                  Log     : in out Gneiss.Log.Client_Session);

end Run;
