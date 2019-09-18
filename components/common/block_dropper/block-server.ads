
with Componolit.Gneiss.Types;
with Componolit.Gneiss.Block.Client;
with Componolit.Gneiss.Block.Server;
with Interfaces;

package Block.Server with
   SPARK_Mode
is

   package Cai renames Componolit.Gneiss;

   procedure Eager_Initialize (Capability :     Cai.Types.Capability;
                               Device     :     String;
                               Modulo     :     Interfaces.Unsigned_8;
                               Part       :     Interfaces.Unsigned_8;
                               Count      :     Interfaces.Unsigned_64;
                               Drop       :     Boolean;
                               Success    : out Boolean);

   procedure Event;
   function Block_Count (S : Types.Server_Session) return Types.Count;
   function Block_Size (S : Types.Server_Session) return Types.Size;
   function Writable (S : Types.Server_Session) return Boolean;
   function Initialized (S : Types.Server_Session) return Boolean;
   procedure Initialize (S : in out Types.Server_Session;
                         L :        String;
                         B :        Types.Byte_Length);
   procedure Finalize (S : in out Types.Server_Session);

   package Instance is new Types.Server (Event,
                                         Block_Count,
                                         Block_Size,
                                         Writable,
                                         Initialized,
                                         Initialize,
                                         Finalize);

   Server : Types.Server_Session;

private

   procedure Write (C : in out Types.Client_Session;
                    I :         Request_Id;
                    D :    out Buffer);

   procedure Read (C : in out Types.Client_Session;
                   I :        Request_Id;
                   D :        Buffer);

   package Instance_Client is new Types.Client (Event, Read, Write);

end Block.Server;
