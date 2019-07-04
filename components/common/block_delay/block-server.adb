
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Log.Client;
with Config;

package body Block.Server with
   SPARK_Mode
is
   package Cai renames Componolit.Interfaces;

   Capability : Cai.Types.Capability;
   Log        : Cai.Log.Client_Session := Cai.Log.Client.Create;

   Null_Cache_Entry : constant Cache_Entry := (Instance.Request'(Kind => Types.None,
                                                                     Priv => Types.Null_Data),
                                               Instance_Client.Request'(Kind => Types.None,
                                                                        Priv => Types.Null_Data),
                                               0.0,
                                               False,
                                               False,
                                               True);

   Request_Cache : Cache (1 .. 256) := (others => Null_Cache_Entry);

   function Get_Next_Ready_Time return Cai.Timer.Time;

   function Get_Next_Ready_Time return Cai.Timer.Time
   is
      use type Cai.Timer.Time;
      Next : Cai.Timer.Time := Cai.Timer.Time'Last;
   begin
      for C of Request_Cache loop
         if C.Ready and then C.Send_Time < Next then
            Next := C.Send_Time;
         end if;
      end loop;
      return Next;
   end Get_Next_Ready_Time;

   procedure Get_Free_Cache_Entry (Index   : out Integer;
                                   Success : out Boolean);

   procedure Get_Free_Cache_Entry (Index   : out Integer;
                                   Success : out Boolean)
   is
   begin
      Success := False;
      Index   := 0;
      for I in Request_Cache'Range loop
         if Request_Cache (I).Free then
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
      use type Types.Request_Kind;
      use type Types.Request_Status;
      use type Types.Id;
      use type Types.Count;
      R : Instance_Client.Request := Instance_Client.Next (Client);
   begin
      Stop := R.Kind = Types.None;
      for I in Request_Cache'Range loop
         if
            not Request_Cache (I).Free
            and then Request_Cache (I).Processed
            and then not Request_Cache (I).Ready
            and then Request_Cache (I).C_Request.Kind = R.Kind
            and then Request_Cache (I).C_Request.Start = R.Start
            and then Request_Cache (I).C_Request.Length = R.Length
         then
            if R.Status = Types.Ok then
               Instance_Client.Read (Client, R);
            end if;
            Request_Cache (I).C_Request.Status := R.Status;
            Request_Cache (I).Ready            := True;
            exit;
         end if;
      end loop;
      Instance_Client.Release (Client, R);
   end Handle_Incoming_Response;

   procedure Acknowledge_Outstanding_Requests;

   procedure Acknowledge_Outstanding_Requests
   is
      use type Types.Request_Status;
      use type Componolit.Interfaces.Timer.Time;
      Current_Time : constant Componolit.Interfaces.Timer.Time := Time.Clock (Timer);
   begin
      for I in Request_Cache'Range loop
         if Request_Cache (I).Ready
            and then Request_Cache (I).Send_Time > 0.0
            and then Current_Time > Request_Cache (I).Send_Time
         then
            Instance.Acknowledge (Server, Request_Cache (I).S_Request);
            if Request_Cache (I).S_Request.Status = Types.Acknowledged then
               Request_Cache (I) := Null_Cache_Entry;
            end if;
         end if;
      end loop;
   end Acknowledge_Outstanding_Requests;

   procedure Handle_Incoming_Request (Stop : out Boolean);

   procedure Handle_Incoming_Request (Stop : out Boolean)
   is
      use type Componolit.Interfaces.Timer.Time;
      R         : constant Instance.Request                 := Instance.Head (Server);
      Recv_Time : constant Componolit.Interfaces.Timer.Time := Time.Clock (Timer);
      Index     : Integer;
   begin
      Get_Free_Cache_Entry (Index, Stop);
      Stop := not Stop;
      if not Stop then
         case R.Kind is
            when Types.None =>
               Stop := True;
            when Types.Undefined =>
               null;
            when Types.Read =>
               Request_Cache (Index) :=
                  Cache_Entry'(S_Request => R,
                               C_Request => Instance_Client.Request'(Kind   => Types.Read,
                                                                     Priv   => Types.Null_Data,
                                                                     Start  => R.Start,
                                                                     Length => R.Length,
                                                                     Status => Types.Raw),
                               Processed => False,
                               Free      => False,
                               Ready     => False,
                               Send_Time => Recv_Time + Config.Get_Delay);
            when Types.Write =>
               Request_Cache (Index) :=
                  Cache_Entry'(S_Request => R,
                               C_Request => Instance_Client.Request'(Kind   => Types.Write,
                                                                     Priv   => Types.Null_Data,
                                                                     Start  => R.Start,
                                                                     Length => R.Length,
                                                                     Status => Types.Raw),
                               Processed => False,
                               Free      => False,
                               Ready     => False,
                               Send_Time => Recv_Time + Config.Get_Delay);
            when Types.Sync =>
               Request_Cache (Index) :=
                  Cache_Entry'(S_Request => R,
                               C_Request => Instance_Client.Request'(Kind   => Types.Sync,
                                                                     Priv   => Types.Null_Data,
                                                                     Start  => R.Start,
                                                                     Length => R.Length,
                                                                     Status => Types.Raw),
                               Processed => False,
                               Free      => False,
                               Ready     => False,
                               Send_Time => Recv_Time + Config.Get_Delay);
            when Types.Trim =>
               Request_Cache (Index) :=
                  Cache_Entry'(S_Request => R,
                               C_Request => Instance_Client.Request'(Kind   => Types.Trim,
                                                                     Priv   => Types.Null_Data,
                                                                     Start  => R.Start,
                                                                     Length => R.Length,
                                                                     Status => Types.Raw),
                               Processed => False,
                               Free      => False,
                               Ready     => False,
                               Send_Time => Recv_Time + Config.Get_Delay);
         end case;
         Instance.Discard (Server);
      end if;
   end Handle_Incoming_Request;

   procedure Process_Requests;

   procedure Process_Requests
   is
   begin
      for I in Request_Cache'Range loop
         if
            not Request_Cache (I).Free
            and then not Request_Cache (I).Processed
         then
            if Instance_Client.Supported (Client, Request_Cache (I).C_Request.Kind) then
               exit when not Instance_Client.Ready (Client, Request_Cache (I).C_Request);
               case Request_Cache (I).C_Request.Kind is
                  when Types.None | Types.Undefined =>
                     Request_Cache (I) := Null_Cache_Entry;
                  when Types.Sync | Types.Trim | Types.Read | Types.Write =>
                     Request_Cache (I).Processed := True;
                     Instance_Client.Enqueue (Client, Request_Cache (I).C_Request);
               end case;
            else
               Request_Cache (I) := Null_Cache_Entry;
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
         Cai.Log.Client.Info (Log, "Event time: " & Cai.Log.Image (Duration (Get_Next_Ready_Time))
                                   & " " & Cai.Log.Image (Duration (Time.Clock (Timer)))
                                   & " " & Cai.Log.Image (Next_Interrupt));
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
                    B :     Types.Size;
                    S :     Types.Id;
                    L :     Types.Count;
                    D : out Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (B);
      use type Types.Request_Kind;
      use type Types.Id;
      use type Types.Count;
      Found : Boolean := False;
   begin
      for I in Request_Cache'Range loop
         if
            not Request_Cache (I).Free
            and then Request_Cache (I).Processed
            and then not Request_Cache (I).Ready
            and then Request_Cache (I).S_Request.Kind = Types.Write
            and then Request_Cache (I).S_Request.Start = S
            and then Request_Cache (I).S_Request.Length = L
         then
            Instance.Write (Server, Request_Cache (I).S_Request, D);
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         Cai.Log.Client.Warning (Log, "No matching server write request in cache");
         D := (others => 0);
      end if;
   end Write;

   procedure Read (C : Types.Client_Instance;
                   B : Types.Size;
                   S : Types.Id;
                   L : Types.Count;
                   D : Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (B);
      use type Types.Request_Kind;
      use type Types.Id;
      use type Types.Count;
      Found : Boolean := False;
   begin
      for I in Request_Cache'Range loop
         if
            not Request_Cache (I).Free
            and then Request_Cache (I).Processed
            and then not Request_Cache (I).Ready
            and then Request_Cache (I).S_Request.Kind = Types.Read
            and then Request_Cache (I).S_Request.Start = S
            and then Request_Cache (I).S_Request.Length = L
         then
            Instance.Read (Server, Request_Cache (I).S_Request, D);
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         Cai.Log.Client.Warning (Log, "No matching server read request in cache");
      end if;
   end Read;

end Block.Server;
