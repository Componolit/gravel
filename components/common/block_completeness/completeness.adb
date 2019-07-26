
with Componolit.Interfaces.Block.Client;
with Componolit.Interfaces.Timer;
with Componolit.Interfaces.Timer.Client;
with Componolit.Interfaces.Log.Client;
with Componolit.Interfaces.Strings;
with Component;

package body Completeness with
   SPARK_Mode
is

   procedure Read (C : Component.Block.Client_Instance;
                   I : Component.Request_Id;
                   D : Component.Buffer);

   procedure Write (C :     Component.Block.Client_Instance;
                    I :     Component.Request_Id;
                    D : out Component.Buffer);

   procedure Allocate (Id      : out Component.Request_Id;
                       Success : out Boolean);

   function Next_Timeout return Cai.Timer.Time;

   package Block_Client is new Component.Block.Client (Component.Request_Id,
                                                       Component.Event,
                                                       Read,
                                                       Write);
   package Timer_Client is new Cai.Timer.Client (Component.Event);

   type Request_Entry is record
      Request : Block_Client.Request;
      Sent    : Cai.Timer.Time;
   end record;

   type Request_Cache is array (Component.Request_Id) of Request_Entry;

   Cache : Request_Cache := (others => (Request => Block_Client.Null_Request,
                                        Sent    => 0.0));

   Client      : Component.Block.Client_Session := Block_Client.Create;
   Timer       : Cai.Timer.Client_Session       := Timer_Client.Create;
   Timeout_Val : Duration;

   function Next_Timeout return Cai.Timer.Time
   is
      use type Cai.Timer.Time;
      Next : Cai.Timer.Time := Cai.Timer.Time'Last;
   begin
      for C of Cache loop
         if C.Sent + Timeout_Val < Next then
            Next := C.Sent + Timeout_Val;
         end if;
      end loop;
      return Next;
   end Next_Timeout;

   procedure Allocate (Id      : out Component.Request_Id;
                       Success : out Boolean)
   is
      use type Component.Block.Request_Status;
   begin
      Success := False;
      for I in Cache'Range loop
         if Block_Client.Status (Cache (I).Request) = Component.Block.Raw then
            Success := True;
            Id      := I;
            return;
         end if;
      end loop;
   end Allocate;

   procedure Initialize (C : Cai.Types.Capability;
                         T : Duration;
                         L : String)
   is
   begin
      if Block_Client.Initialized (Client) and Timer_Client.Initialized (Timer) then
         return;
      end if;
      if not Block_Client.Initialized (Client) then
         Block_Client.Initialize (Client, C, L);
      end if;
      if not Block_Client.Initialized (Client) then
         return;
      end if;
      if not Timer_Client.Initialized (Timer) then
         Timer_Client.Initialize (Timer, C);
      end if;
      if not Timer_Client.Initialized (Timer) then
         if Block_Client.Initialized (Client) then
            Block_Client.Finalize (Client);
         end if;
         return;
      end if;
      Timeout_Val := T;
   end Initialize;

   procedure Event (Log : in out Cai.Log.Client_Session)
   is
      use type Interfaces.Unsigned_64;
      use type Cai.Timer.Time;
      use type Component.Block.Request_Status;
      Index   : Component.Request_Id;
      Handle  : Block_Client.Request_Handle;
      Current : Cai.Timer.Time;
      Success : Boolean;
   begin
      loop
         loop
            Block_Client.Update_Response_Queue (Client, Handle);
            exit when not Block_Client.Valid (Handle);
            Index := Block_Client.Identifier (Handle);
            if Block_Client.Status (Cache (Index).Request) = Component.Block.Pending then
               Block_Client.Update_Request (Client, Cache (Index).Request, Handle);
               Received := Received + 1;
               if Block_Client.Status (Cache (Index).Request) = Component.Block.Ok then
                  Ok := Ok + 1;
               else
                  Error := Error + 1;
               end if;
               Block_Client.Release (Client, Cache (Index).Request);
            end if;
         end loop;
         Current := Timer_Client.Clock (Timer);
         for I in Cache'Range loop
            if
               Block_Client.Status (Cache (I).Request) = Component.Block.Pending
               and Current - Timeout_Val > Cache (I).Sent
            then
               Timeout := Timeout + 1;
               Block_Client.Release (Client, Cache (I).Request);
            end if;
         end loop;
         loop
            Allocate (Index, Success);
            exit when not Success;
            Block_Client.Allocate_Request (Client,
                                           Cache (Index).Request,
                                           Component.Block.Read,
                                           Component.Block.Id
                                              (Sent mod Interfaces.Unsigned_64 (Block_Client.Block_Count (Client))),
                                           1,
                                           Index);
            exit when Block_Client.Status (Cache (Index).Request) /= Component.Block.Allocated;
            Cache (Index).Sent := Timer_Client.Clock (Timer);
            Block_Client.Enqueue (Client, Cache (Index).Request);
            Sent := Sent + 1;
         end loop;
         Block_Client.Submit (Client);
         Raw     := 0;
         Pending := 0;
         Other   := 0;
         for C of Cache loop
            case Block_Client.Status (C.Request) is
               when Component.Block.Raw =>
                  Raw := Raw + 1;
               when Component.Block.Pending =>
                  Pending := Pending + 1;
               when others =>
                  Other := Other + 1;
            end case;
         end loop;
         Cai.Log.Client.Info (Log, "Stats: "
                                   & Cai.Strings.Image (Completeness.Received)
                                   & "/" & Cai.Strings.Image (Completeness.Sent)
                                   & " (" & Cai.Strings.Image (Completeness.Ok)
                                   & "/" & Cai.Strings.Image (Completeness.Error)
                                   & "/" & Cai.Strings.Image (Completeness.Timeout)
                                   & ")");
         Cai.Log.Client.Info (Log, "Cache: "
                                   & Cai.Strings.Image (Completeness.Raw)
                                   & "/" & Cai.Strings.Image (Completeness.Pending)
                                   & "/" & Cai.Strings.Image (Completeness.Other));
         Current := Timer_Client.Clock (Timer);
         if Current < Next_Timeout then
            Timer_Client.Set_Timeout (Timer, Duration (Next_Timeout) - Duration (Current));
            exit;
         end if;
      end loop;
   end Event;

   procedure Read (C : Component.Block.Client_Instance;
                   I : Component.Request_Id;
                   D : Component.Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (I);
      pragma Unreferenced (D);
   begin
      null;
   end Read;

   procedure Write (C :     Component.Block.Client_Instance;
                    I :     Component.Request_Id;
                    D : out Component.Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (I);
   begin
      D := (others => Component.Byte'First);
   end Write;

end Completeness;
