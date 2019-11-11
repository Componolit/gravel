
with Componolit.Gneiss.Types;
with Componolit.Gneiss.Timer;
with Componolit.Gneiss.Timer.Client;
with Componolit.Gneiss.Block.Client;
with Componolit.Gneiss.Block.Server;

package Block.Server with
   SPARK_Mode
is

   procedure Set_Capability (Cap : Componolit.Gneiss.Types.Capability);

   procedure Event;
   function Block_Count (S : Types.Server_Session) return Types.Count;
   function Block_Size (S : Types.Server_Session) return Types.Size;
   function Writable (S : Types.Server_Session) return Boolean;
   function Initialized (S : Types.Server_Session) return Boolean;
   procedure Initialize (S : in out Types.Server_Session;
                         L :        String;
                         B :        Types.Byte_Length);
   procedure Finalize (S : in out Types.Server_Session);
   procedure Read (S : in out Types.Server_Session;
                   I :        Request_Id;
                   B :    out Buffer);
   procedure Write (S : in out Types.Server_Session;
                    I :        Request_Id;
                    B :        Buffer);

   package Instance is new Types.Server (Event,
                                         Block_Count,
                                         Block_Size,
                                         Writable,
                                         Initialized,
                                         Initialize,
                                         Finalize,
                                         Read,
                                         Write);

   Server : Types.Server_Session;

private

   function Round_Up (Time  : Componolit.Gneiss.Timer.Time;
                      Slice : Duration) return Componolit.Gneiss.Timer.Time with
      Pre => Slice /= 0.0;

   procedure Write (C : in out Types.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer);

   procedure Read (C : in out Types.Client_Session;
                   I :        Request_Id;
                   D :        Buffer);

   package Instance_Client is new Types.Client (Event, Read, Write);

   package Time is new Componolit.Gneiss.Timer.Client (Event);

   Client : Types.Client_Session;
   Timer  : Componolit.Gneiss.Timer.Client_Session;

   --  Free := Status (S_Request) = Raw and Status (C_Request) = Raw
   --  Ready := Status (C_Request) = Ok | Error
   --  Processed := Status (S_Request) = Pending and Status (C_Request) = Pending
   type Cache_Entry is record
      S_Request : Instance.Request;
      C_Request : Instance_Client.Request;
      Send_Time : Componolit.Gneiss.Timer.Time := 0.0;
   end record;

   type Cache is array (Request_Id'Range) of Cache_Entry;

end Block.Server;
