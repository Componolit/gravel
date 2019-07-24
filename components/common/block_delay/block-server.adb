
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Log.Client;
with Config;
with Jitter;

package body Block.Server with
   SPARK_Mode
is
   package Cai renames Componolit.Interfaces;

   use type Types.Request_Status;

   Capability : Cai.Types.Capability;
   Log        : Cai.Log.Client_Session := Cai.Log.Client.Create;

   Request_Cache : Cache := (others => Cache_Entry'(S_Request => Instance.Null_Request,
                                                    C_Request => Instance_Client.Null_Request,
                                                    Send_Time => 0.0));

   function Free (E : Cache_Entry) return Boolean
   is
      (Instance_Client.Status (E.C_Request) = Types.Raw
       and Instance.Status (E.S_Request) = Types.Raw);

   function Ready (E : Cache_Entry) return Boolean
   is
      (Instance_Client.Status (E.C_Request) in Types.Ok | Types.Error);

   function Processed (E : Cache_Entry) return Boolean
   is
      (Ready (E) or else (Instance_Client.Status (E.C_Request) = Types.Pending
                          and Instance.Status (E.S_Request) = Types.Pending));

   function Get_Next_Ready_Time return Cai.Timer.Time;

   function Get_Next_Ready_Time return Cai.Timer.Time
   is
      use type Cai.Timer.Time;
      Next : Cai.Timer.Time := Cai.Timer.Time'Last;
   begin
      for C of Request_Cache loop
         if Ready (C) and then C.Send_Time < Next then
            Next := C.Send_Time;
         end if;
      end loop;
      return Next;
   end Get_Next_Ready_Time;

   procedure Get_Free_Cache_Entry (Index   : out Request_Id;
                                   Success : out Boolean);

   procedure Get_Free_Cache_Entry (Index   : out Request_Id;
                                   Success : out Boolean)
   is
   begin
      Success := False;
      Index   := 0;
      for I in Request_Cache'Range loop
         if Free (Request_Cache (I)) then
            Success := True;
            Index   := I;
            exit;
         end if;
      end loop;
   end Get_Free_Cache_Entry;

   procedure Set_Capability (Cap : Componolit.Interfaces.Types.Capability)
   is
   begin
      Capability := Cap;
   end Set_Capability;

   procedure Handle_Incoming_Response (Stop : out Boolean);

   procedure Handle_Incoming_Response (Stop : out Boolean)
   is
      Handle : Instance_Client.Request_Handle;
      Ident  : Request_Id;
   begin
      Instance_Client.Update_Response_Queue (Client, Handle);
      Stop := not Instance_Client.Valid (Handle);
      if Stop then
         return;
      end if;
      Ident := Instance_Client.Identifier (Handle);
      Instance_Client.Update_Request (Client, Request_Cache (Ident).C_Request, Handle);
      if Instance_Client.Status (Request_Cache (Ident).C_Request) = Types.Ok then
         Instance_Client.Read (Client, Request_Cache (Ident).C_Request);
      end if;
   end Handle_Incoming_Response;

   procedure Acknowledge_Outstanding_Requests;

   procedure Acknowledge_Outstanding_Requests
   is
      use type Componolit.Interfaces.Timer.Time;
      Current_Time : constant Componolit.Interfaces.Timer.Time := Time.Clock (Timer);
   begin
      for I in Request_Cache'Range loop
         if
            Ready (Request_Cache (I))
            and then Request_Cache (I).Send_Time > 0.0
            and then Current_Time > Request_Cache (I).Send_Time
         then
            Instance.Acknowledge (Server, Request_Cache (I).S_Request,
                                  Instance_Client.Status (Request_Cache (I).C_Request));
            if Instance.Status (Request_Cache (I).S_Request) = Types.Raw then
               Request_Cache (I).Send_Time := 0.0;
               Instance_Client.Release (Client, Request_Cache (I).C_Request);
            end if;
         end if;
      end loop;
   end Acknowledge_Outstanding_Requests;

   procedure Handle_Incoming_Request (Stop : out Boolean);

   procedure Handle_Incoming_Request (Stop : out Boolean)
   is
      use type Componolit.Interfaces.Timer.Time;
      Recv_Time  : constant Componolit.Interfaces.Timer.Time := Time.Clock (Timer);
      Index      : Request_Id;
      Send_Delay : Duration := Config.Get_Delay;
   begin
      Get_Free_Cache_Entry (Index, Stop);
      Stop := not Stop;
      if Stop then
         return;
      end if;
      Instance.Process (Server, Request_Cache (Index).S_Request);
      Stop := Instance.Status (Request_Cache (Index).S_Request) = Types.Raw;
      if Stop then
         return;
      end if;
      case Config.Get_Jitter_Distribution is
         when Config.Uniform =>
            Jitter.Apply (Send_Delay);
         when others =>
            null;
      end case;
      Request_Cache (Index).Send_Time := Recv_Time + Send_Delay;
   end Handle_Incoming_Request;

   procedure Process_Requests;

   procedure Process_Requests
   is
   begin
      for I in Request_Cache'Range loop
         if
            not Free (Request_Cache (I))
            and then not Processed (Request_Cache (I))
         then
            Instance_Client.Allocate_Request (Client,
                                              Request_Cache (I).C_Request,
                                              Instance.Kind (Request_Cache (I).S_Request),
                                              Instance.Start (Request_Cache (I).S_Request),
                                              Instance.Length (Request_Cache (I).S_Request),
                                              I);
            if Instance_Client.Status (Request_Cache (I).C_Request) = Types.Allocated then
               Instance_Client.Enqueue (Client, Request_Cache (I).C_Request);
            end if;
         end if;
      end loop;
      Instance_Client.Submit (Client);
   end Process_Requests;

   procedure Event
   is
      use type Cai.Timer.Time;
      Stop : Boolean;
      Next_Interrupt : Duration;
   begin
      if
         Componolit.Interfaces.Log.Client.Initialized (Log)
         and then Time.Initialized (Timer)
         and then Instance_Client.Initialized (Client)
      then
         loop
            Handle_Incoming_Response (Stop);
            exit when Stop;
         end loop;
         Acknowledge_Outstanding_Requests;
         loop
            Handle_Incoming_Request (Stop);
            exit when Stop;
         end loop;
         Process_Requests;
         Next_Interrupt := Duration (Get_Next_Ready_Time - Time.Clock (Timer));
         Time.Set_Timeout (Timer, Next_Interrupt);
         --  FIXME: too short delays or removing this output cause the program to hang
         --  when no request is in flight and no time is scheduled
         --  Cai.Log.Client.Info (Log, "Event time: " & Cai.Log.Image (Duration (Get_Next_Ready_Time))
         --                            & " " & Cai.Log.Image (Duration (Time.Clock (Timer)))
         --                            & " " & Cai.Log.Image (Next_Interrupt));
         Instance.Unblock_Client (Server);
      end if;
   end Event;

   function Block_Count (S : Types.Server_Instance) return Types.Count
   is
      pragma Unreferenced (S);
   begin
      return Instance_Client.Block_Count (Client);
   end Block_Count;

   function Block_Size (S : Types.Server_Instance) return Types.Size
   is
      pragma Unreferenced (S);
   begin
      return Instance_Client.Block_Size (Client);
   end Block_Size;

   function Writable (S : Types.Server_Instance) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return Instance_Client.Writable (Client);
   end Writable;

   function Maximum_Transfer_Size (S : Types.Server_Instance) return Types.Byte_Length
   is
      pragma Unreferenced (S);
      use type Types.Byte_Length;
   begin
      if Instance_Client.Maximum_Transfer_Size (Client) < 65536 then
         return Instance_Client.Maximum_Transfer_Size (Client);
      else
         return 65536;
      end if;
   end Maximum_Transfer_Size;

   procedure Initialize (S : Types.Server_Instance;
                         L : String;
                         B : Types.Byte_Length)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (B);
      Seed : Duration;
   begin
      if not Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, Capability, L);
      end if;
      if Cai.Log.Client.Initialized (Log) then
         if not Time.Initialized (Timer) then
            Time.Initialize (Timer, Capability);
         end if;
         if Time.Initialized (Timer) then
            if not Instance_Client.Initialized (Client) then
               Instance_Client.Initialize (Client, Capability, Config.Get_Client_Id);
               Seed := Duration (Time.Clock (Timer));
               case Config.Get_Jitter_Distribution is
                  when Config.Uniform =>
                     Jitter.Seed (Seed, Config.Get_Jitter);
                  when others =>
                     null;
               end case;
            else
               Cai.Log.Client.Warning (Log, "Client connection unexpectedly already initialized.");
            end if;
            if Instance_Client.Initialized (Client) then
               Cai.Log.Client.Info (Log, "Client connection to " & Config.Get_Client_Id & " established.");
            else
               Cai.Log.Client.Error (Log, "Failed to initialize client connection to "
                                          & Config.Get_Client_Id & ".");
            end if;
         else
            Cai.Log.Client.Error (Log, "Failed to initialize timer.");
         end if;
      end if;
   end Initialize;

   function Initialized (S : Types.Server_Instance) return Boolean
   is
   begin
      return Instance_Client.Initialized (Client);
   end Initialized;

   procedure Finalize (S : Types.Server_Instance)
   is
      pragma Unreferenced (S);
   begin
      if Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Finalize (Log);
      end if;
      if Instance_Client.Initialized (Client) then
         Instance_Client.Finalize (Client);
      end if;
   end Finalize;

   procedure Write (C :     Types.Client_Instance;
                    I :     Request_Id;
                    D : out Buffer)
   is
      pragma Unreferenced (C);
   begin
      Instance.Write (Server, Request_Cache (I).S_Request, D);
   end Write;

   procedure Read (C : Types.Client_Instance;
                   I : Request_Id;
                   D : Buffer)
   is
      pragma Unreferenced (C);
   begin
      Instance.Read (Server, Request_Cache (I).S_Request, D);
   end Read;

end Block.Server;
