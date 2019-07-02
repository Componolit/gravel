
with Componolit.Interfaces.Types;
with Componolit.Interfaces.Block;
with Componolit.Interfaces.Block.Client;
with Componolit.Interfaces.Log;
with Run;

generic
   with package Block is new Componolit.Interfaces.Block (<>);
   with package Client is new Block.Client (<>);
   Request_Count : Block.Count;
   Iterations    : Positive;
package Rwr is

   package Cai renames Componolit.Interfaces;

   package RR1 is new Run (Block, Client, Request_Count, Iterations, Block.Read);
   package WR is new Run (Block, Client, Request_Count, Iterations, Block.Write);
   package RR2 is new Run (Block, Client, Request_Count, Iterations, Block.Read);

   type Rwr_Run is limited record
      R1 : RR1.Run_Type;
      W : WR.Run_Type;
      R2 : RR2.Run_Type;
   end record;

   procedure Initialize (R : out Rwr_Run;
                         C :     Cai.Types.Capability);

   procedure Run (C   : in out Block.Client_Session;
                  R   : in out Rwr_Run;
                  Log : in out Cai.Log.Client_Session);

   function Finished (R : Rwr_Run) return Boolean;

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session;
                  R       : Rwr_Run;
                  Log     : in out Cai.Log.Client_Session);

end Rwr;
