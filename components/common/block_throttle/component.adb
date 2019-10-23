
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Rom;
with Componolit.Gneiss.Rom.Client;
with Componolit.Gneiss.Timer;
with Componolit.Gneiss.Timer.Client;
with Componolit.Gneiss.Strings_Generic;
with Componolit.Gneiss.Containers.Fifo;
with Config;

package body Component is

   use type Block.Request_Kind;
   use type Block.Request_Status;

   package Rom is new Gns.Rom.Client (Character, Positive, String, Config.Parse);
   package Queue is new Gns.Containers.Fifo (Request_Index);
   procedure Timer_Event;
   package Timer_Client is new Gns.Timer.Client (Timer_Event);

   Dispatcher : Block.Dispatcher_Session;
   Client     : Block.Client_Session;
   Server     : Block.Server_Session;
   Conf_Rom   : Gns.Rom.Client_Session;
   Capability : Gns.Types.Capability;
   Log        : Gns.Log.Client_Session;
   Timer      : Gns.Timer.Client_Session;
   Rate       : Natural;
   Current    : Natural := 0;
   Interval   : Duration;
   Ack_Queue  : Queue.Queue (2 ** 8);

   function Image is new Gns.Strings_Generic.Image_Ranged (Natural);

   procedure Construct (Cap : Gns.Types.Capability)
   is
   begin
      Queue.Initialize (Ack_Queue, Request_Index'First);
      Capability := Cap;
      if not Gns.Log.Initialized (Log) then
         Gns.Log.Client.Initialize (Log, Cap, "log_block_throttle");
      end if;
      if Gns.Log.Initialized (Log) then
         if not Gns.Timer.Initialized (Timer) then
            Timer_Client.Initialize (Timer, Cap);
         end if;
         if not Gns.Timer.Initialized (Timer) then
            Gns.Log.Client.Error (Log, "Failed to initialize timer");
            Main.Vacate (Capability, Main.Failure);
            return;
         end if;
         if not Gns.Rom.Initialized (Conf_Rom) then
            Rom.Initialize (Conf_Rom, Cap);
         end if;
         if not Gns.Rom.Initialized (Conf_Rom) then
            Gns.Log.Client.Error (Log, "Failed to initialize rom session");
            Main.Vacate (Capability, Main.Failure);
            return;
         end if;
         Rom.Load (Conf_Rom);
         if not Config.Initialized then
            Gns.Log.Client.Error (Log, "Failed to parse config: "
                                       & Config.Reason);
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;
         if not Block.Initialized (Dispatcher) then
            Block_Dispatcher.Initialize (Dispatcher, Cap, 42);
         end if;
         if Block.Initialized (Dispatcher) then
            Rate     := Config.Rate / Config.Frequency;
            Interval := 1.0 / Duration (Config.Frequency);
            Block_Dispatcher.Register (Dispatcher);
            Gns.Log.Client.Info (Log, "Throttle ready.");
            Gns.Log.Client.Info (Log, "Device: " & Config.Device);
            Gns.Log.Client.Info (Log, "Request rate: " & Image (Config.Rate) & "/s");
            Gns.Log.Client.Info (Log, "Ack frequency: " & Image (Config.Frequency) & "/s");
         else
            Gns.Log.Client.Error (Log, "Failed to initialize Dispatcher");
            Main.Vacate (Capability, Main.Failure);
         end if;
      else
         Main.Vacate (Capability, Main.Failure);
      end if;
   end Construct;

   procedure Destruct
   is
   begin
      if Gns.Log.Initialized (Log) then
         Gns.Log.Client.Finalize (Log);
      end if;
      if Block.Initialized (Dispatcher) then
         Block_Dispatcher.Finalize (Dispatcher);
      end if;
   end Destruct;

   type Cache_Entry is record
      C : Block_Client.Request;
      S : Block_Server.Request;
      A : Boolean := False;
      Q : Boolean := False;
   end record;

   type Registry is array (Request_Index'Range) of Cache_Entry;

   Cache : Registry;

   procedure Write (C : in out Block.Client_Session;
                    I :        Request_Index;
                    D :    out Buffer)
   is
      pragma Unreferenced (C);
   begin
      if
         Block_Server.Status (Cache (I).S) = Block.Pending
         and then Block_Server.Kind (Cache (I).S) = Block.Write
         and then Initialized (Server)
         and then Block.Initialized (Server)
         and then Block_Server.Assigned (Server, Cache (I).S)
         and then D'Length = Block_Size (Server) * Block_Server.Length (Cache (I).S)
      then
         Block_Server.Write (Server, Cache (I).S, D);
      else
         Cache (I).A := True;
      end if;
   end Write;

   procedure Read (C : in out Block.Client_Session;
                   I :        Request_Index;
                   D :        Buffer)
   is
      pragma Unreferenced (C);
   begin
      if
         Block_Server.Status (Cache (I).S) = Block.Pending
         and then Block_Server.Kind (Cache (I).S) = Block.Read
         and then Initialized (Server)
         and then Block.Initialized (Server)
         and then Block_Server.Assigned (Server, Cache (I).S)
         and then D'Length = Block_Size (Server) * Block_Server.Length (Cache (I).S)
      then
         Block_Server.Read (Server, Cache (I).S, D);
      else
         Cache (I).A := True;
      end if;
   end Read;

   procedure Event
   is
      Re : Block.Result;
      Pr : Boolean := True;
   begin
      if
         Block.Initialized (Client)
         and then Initialized (Server)
         and then Block.Initialized (Server)
      then
         pragma Assert (Block.Initialized (Server));
         while Pr loop
            Pr := False;
            for I in Cache'Range loop
               pragma Loop_Invariant (Block.Initialized (Client));
               pragma Loop_Invariant (Block.Initialized (Server));
               pragma Loop_Invariant (Initialized (Server));
               case Block_Server.Status (Cache (I).S) is
                  when Block.Raw =>
                     case Block_Client.Status (Cache (I).C) is
                        when Block.Raw =>
                           Block_Server.Process (Server, Cache (I).S);
                           Pr := Pr or else Block_Server.Status (Cache (I).S) = Block.Pending;
                        when Block.Ok | Block.Error =>
                           if Block_Client.Assigned (Client, Cache (I).C) then
                              Block_Client.Release (Client, Cache (I).C);
                              Cache (I).A := False;
                           end if;
                        when others =>
                           null;
                     end case;
                  when Block.Pending =>
                     case Block_Client.Status (Cache (I).C) is
                        when Block.Raw =>
                           Block_Client.Allocate_Request (Client,
                                                          Cache (I).C,
                                                          Block_Server.Kind (Cache (I).S),
                                                          Block_Server.Start (Cache (I).S),
                                                          Block_Server.Length (Cache (I).S),
                                                          I,
                                                          Re);
                           case Re is
                              when Block.Success =>
                                 Block_Client.Enqueue (Client, Cache (I).C);
                              when Block.Retry =>
                                 null;
                              when others =>
                                 Cache (I).A := True;
                           end case;
                        when Block.Allocated =>
                           if Block_Client.Assigned (Client, Cache (I).C) then
                              Block_Client.Enqueue (Client, Cache (I).C);
                           end if;
                        when Block.Pending =>
                           if Block_Client.Assigned (Client, Cache (I).C) then
                              Block_Client.Update_Request (Client, Cache (I).C);
                              Pr := Pr or else Block_Client.Status (Cache (I).C) /= Block.Pending;
                           end if;
                        when Block.Ok | Block.Error =>
                           if
                              Block_Client.Assigned (Client, Cache (I).C)
                              and then not Cache (I).Q
                           then
                              if
                                 Block_Client.Status (Cache (I).C) = Block.Ok
                                 and then Block_Client.Kind (Cache (I).C) = Block.Read
                              then
                                 Block_Client.Read (Client, Cache (I).C);
                              end if;
                              Queue.Put (Ack_Queue, I);
                              Cache (I).Q := True;
                              Pr := True;
                           end if;
                     end case;
                  when Block.Error =>
                     if Block_Server.Assigned (Server, Cache (I).S) then
                        Block_Server.Acknowledge (Server, Cache (I).S, Block.Error);
                        Pr := Pr or else Block_Server.Status (Cache (I).S) = Block.Raw;
                     end if;
                  when others =>
                     null;
               end case;
            end loop;
            Block_Client.Submit (Client);
         end loop;
         Block_Server.Unblock_Client (Server);
      end if;
   end Event;

   procedure Timer_Event
   is
      Index   : Request_Index;
   begin
      Current := 0;
      while Current < Rate and then Queue.Count (Ack_Queue) > 0 loop
         Queue.Peek (Ack_Queue, Index);
         Block_Server.Acknowledge (Server, Cache (Index).S, Block_Client.Status (Cache (Index).C));
         exit when Block_Server.Status (Cache (Index).S) /= Block.Raw;
         Current := Current + 1;
         Cache (Index).Q := False;
         Queue.Drop (Ack_Queue);
      end loop;
      if Initialized (Server) then
         Timer_Client.Set_Timeout (Timer, Interval);
         Block_Server.Unblock_Client (Server);
      end if;
   end Timer_Event;

   procedure Dispatch (I : in out Block.Dispatcher_Session;
                       C :        Block.Dispatcher_Capability)
   is
   begin
      if Block_Dispatcher.Valid_Session_Request (I, C)
         and then not Initialized (Server)
         and then not Block.Initialized (Server)
      then
         Block_Dispatcher.Session_Initialize (I, C, Server, 42);
         if Initialized (Server) and then Block.Initialized (Server) then
            Block_Dispatcher.Session_Accept (I, C, Server);
         end if;
      end if;
      Block_Dispatcher.Session_Cleanup (I, C, Server);
   end Dispatch;

   procedure Initialize_Server (S : in out Block.Server_Session;
                                L :        String;
                                B :        Block.Byte_Length)
   is
      use type Block.Byte_Length;
   begin
      if not Block.Initialized (Client) then
         --  The client label of a proxy should be determined by a policy depending on the server label.
         --  In this simple test the policy is hardcoded. If the server receives a connection request with
         --  label "blockdev2" the client will use the label "blockdev1", otherwise it will use the label
         --  provided by the server. This policy is to prevent the proxy to connect to itself on Muen.
         if L = "blockdev2" then  --  Muen
            Block_Client.Initialize (Client, Capability, "blockdev1", 42, B);
         else  --  Genode
            Block_Client.Initialize (Client, Capability, L, 42, B * 2);
         end if;
         Timer_Client.Set_Timeout (Timer, Interval);
      end if;
      if Block.Initialized (Client) and then not Initialized (S) then
         Block_Client.Finalize (Client);
      end if;
   end Initialize_Server;

   procedure Finalize_Server (S : in out Block.Server_Session)
   is
      pragma Unreferenced (S);
   begin
      Block_Client.Finalize (Client);
   end Finalize_Server;

   function Block_Count (S : Block.Server_Session) return Block.Count
   is
      pragma Unreferenced (S);
   begin
      return Block.Block_Count (Client);
   end Block_Count;

   function Block_Size (S : Block.Server_Session) return Block.Size
   is
      pragma Unreferenced (S);
   begin
      return Block.Block_Size (Client);
   end Block_Size;

   function Writable (S : Block.Server_Session) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return Block.Writable (Client);
   end Writable;

   function Initialized (S : Block.Server_Session) return Boolean is
      (Block.Initialized (Client)
       and then Block.Block_Size (Client) in 512 | 1024 | 2048 | 4096
       and then Block.Block_Count (Client) > 0
       and then Block.Block_Count (Client) < Block.Count'Last / (Block.Count (Block.Block_Size (Client)) / 512));

end Component;
