
with Componolit.Interfaces.Types;
with Componolit.Interfaces.Block.Client;
with Componolit.Interfaces.Block.Server;
with Interfaces;

package Block.Server with
   SPARK_Mode
is

   package Cai renames Componolit.Interfaces;

   procedure Eager_Initialize (Capability :     Cai.Types.Capability;
                               Device     :     String;
                               Modulo     :     Interfaces.Unsigned_8;
                               Part       :     Interfaces.Unsigned_8;
                               Count      :     Interfaces.Unsigned_64;
                               Drop       :     Boolean;
                               Success    : out Boolean);

   procedure Event;
   function Block_Count (S : Types.Server_Instance) return Types.Count;
   function Block_Size (S : Types.Server_Instance) return Types.Size;
   function Writable (S : Types.Server_Instance) return Boolean;
   function Maximum_Transfer_Size (S : Types.Server_Instance) return Types.Byte_Length;
   procedure Initialize (S : Types.Server_Instance;
                         L : String;
                         B : Types.Byte_Length);
   procedure Finalize (S : Types.Server_Instance);

   package Instance is new Types.Server (Event,
                                         Block_Count,
                                         Block_Size,
                                         Writable,
                                         Maximum_Transfer_Size,
                                         Initialize,
                                         Finalize);

   Server : Types.Server_Session := Block.Server.Instance.Create;

private

   subtype Request_Id is Positive range 1 .. 64;

   procedure Write (C :     Types.Client_Instance;
                    I :     Request_Id;
                    D : out Buffer);

   procedure Read (C : Types.Client_Instance;
                   I : Request_Id;
                   D : Buffer);

   package Instance_Client is new Types.Client (Request_Id, Event, Read, Write);

end Block.Server;
