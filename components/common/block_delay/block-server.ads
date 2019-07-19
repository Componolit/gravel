
with Componolit.Interfaces.Types;
with Componolit.Interfaces.Timer;
with Componolit.Interfaces.Timer.Client;
with Componolit.Interfaces.Block.Client;
with Componolit.Interfaces.Block.Server;

package Block.Server with
   SPARK_Mode
is

   procedure Set_Capability (Cap : Componolit.Interfaces.Types.Capability);

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

   procedure Write (C :     Types.Client_Instance;
                    I :     Request_Id;
                    D : out Buffer);

   procedure Read (C : Types.Client_Instance;
                   I : Request_Id;
                   D : Buffer);

   package Instance_Client is new Types.Client (Request_Id, Event, Read, Write);

   package Time is new Componolit.Interfaces.Timer.Client (Event);

   Client : Types.Client_Session := Instance_Client.Create;
   Timer  : Componolit.Interfaces.Timer.Client_Session := Time.Create;

   --  Free := Status (S_Request) = Raw and Status (C_Request) = Raw
   --  Ready := Status (C_Request) = Ok | Error
   --  Processed := Status (S_Request) = Pending and Status (C_Request) = Pending
   type Cache_Entry is record
      S_Request : Instance.Request;
      C_Request : Instance_Client.Request;
      Send_Time : Componolit.Interfaces.Timer.Time;
   end record;

   type Cache is array (Request_Id'Range) of Cache_Entry;

end Block.Server;
