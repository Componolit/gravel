
with Gneiss.Block.Client;
with Gneiss.Timer;
with Gneiss.Timer.Client;
with Gneiss.Log.Client;
with Basalt.Strings;
with Component;

package body Completeness with
   SPARK_Mode
is

   procedure Read (C : in out Component.Block.Client_Session;
                   I :        Component.Request_Id;
                   D :        Component.Buffer);

   procedure Write (C : in out Component.Block.Client_Session;
                    I :        Component.Request_Id;
                    D :    out Component.Buffer);

   function Next_Timeout return Gneiss.Timer.Time;

   package Block_Client is new Component.Block.Client (Component.Event,
                                                       Read,
                                                       Write);
   package Timer_Client is new Gneiss.Timer.Client (Component.Event);

   type Request_Entry is record
      Request : Block_Client.Request;
      Sent    : Gneiss.Timer.Time := 0.0;
   end record;

   type Request_Cache is array (Component.Request_Id) of Request_Entry;

   Cache : Request_Cache;

   Client      : Component.Block.Client_Session;
   Timer       : Gneiss.Timer.Client_Session;
   Timeout_Val : Duration;

   function Next_Timeout return Gneiss.Timer.Time
   is
      use type Gneiss.Timer.Time;
      Next : Gneiss.Timer.Time := Gneiss.Timer.Time'Last;
   begin
      for C of Cache loop
         if C.Sent + Timeout_Val < Next then
            Next := C.Sent + Timeout_Val;
         end if;
      end loop;
      return Next;
   end Next_Timeout;

   procedure Initialize (C : Gneiss.Types.Capability;
                         T : Duration;
                         L : String)
   is
   begin
      if Component.Block.Initialized (Client) and Gneiss.Timer.Initialized (Timer) then
         return;
      end if;
      if not Component.Block.Initialized (Client) then
         Block_Client.Initialize (Client, C, L, Component.Session_Id (True));
      end if;
      if not Component.Block.Initialized (Client) then
         return;
      end if;
      if not Gneiss.Timer.Initialized (Timer) then
         Timer_Client.Initialize (Timer, C);
      end if;
      if not Gneiss.Timer.Initialized (Timer) then
         if Component.Block.Initialized (Client) then
            Block_Client.Finalize (Client);
         end if;
         return;
      end if;
      Timeout_Val := T;
   end Initialize;

   procedure Event (Log : in out Gneiss.Log.Client_Session)
   is
      use type Interfaces.Unsigned_64;
      use type Gneiss.Timer.Time;
      use type Component.Block.Request_Status;
      use type Component.Block.Request_Kind;
      use type Component.Block.Result;
      Current : Gneiss.Timer.Time;
      Result  : Component.Block.Result;
   begin
      loop
         for I in Cache'Range loop
            Current := Timer_Client.Clock (Timer);
            if Block_Client.Status (Cache (I).Request) = Component.Block.Pending then
               Block_Client.Update_Request (Client, Cache (I).Request);
               case Block_Client.Status (Cache (I).Request) is
                  when Component.Block.Pending =>
                     if Current - Timeout_Val > Cache (I).Sent then
                        Block_Client.Release (Client, Cache (I).Request);
                        Timeout := Timeout + 1;
                     end if;
                  when Component.Block.Ok =>
                     Ok := Ok + 1;
                     Received := Received + 1;
                     Block_Client.Release (Client, Cache (I).Request);
                  when Component.Block.Error =>
                     Error := Error + 1;
                     Received := Received + 1;
                     Block_Client.Release (Client, Cache (I).Request);
                  when others => null;
               end case;
            end if;
            if Block_Client.Status (Cache (I).Request) = Component.Block.Raw then
               Block_Client.Allocate_Request (Client,
                                              Cache (I).Request,
                                              Component.Block.Read,
                                              Component.Block.Id
                                               (Sent mod Interfaces.Unsigned_64
                                                  (Component.Block.Block_Count (Client))),
                                              1,
                                              I,
                                              Result);
               if
                  Result = Component.Block.Success
                  and then Block_Client.Kind (Cache (I).Request) = Component.Block.Write
               then
                  Block_Client.Write (Client, Cache (I).Request);
               end if;
            end if;
            if Block_Client.Status (Cache (I).Request) = Component.Block.Allocated then
               Block_Client.Enqueue (Client, Cache (I).Request);
               if Block_Client.Status (Cache (I).Request) = Component.Block.Pending then
                  Cache (I).Sent := Timer_Client.Clock (Timer);
                  Sent := Sent + 1;
               end if;
            end if;
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
         Gneiss.Log.Client.Info (Log, "Stats: "
                                   & Basalt.Strings.Image (Completeness.Received)
                                   & "/" & Basalt.Strings.Image (Completeness.Sent)
                                   & " (" & Basalt.Strings.Image (Completeness.Ok)
                                   & "/" & Basalt.Strings.Image (Completeness.Error)
                                   & "/" & Basalt.Strings.Image (Completeness.Timeout)
                                   & ")");
         Gneiss.Log.Client.Info (Log, "Cache: "
                                   & Basalt.Strings.Image (Completeness.Raw)
                                   & "/" & Basalt.Strings.Image (Completeness.Pending)
                                   & "/" & Basalt.Strings.Image (Completeness.Other));
         Current := Timer_Client.Clock (Timer);
         if Current < Next_Timeout then
            Timer_Client.Set_Timeout (Timer, (Duration (Next_Timeout) - Duration (Current)) / 2.0);
            exit;
         end if;
      end loop;
   end Event;

   procedure Read (C : in out Component.Block.Client_Session;
                   I :        Component.Request_Id;
                   D :        Component.Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (I);
      pragma Unreferenced (D);
   begin
      null;
   end Read;

   procedure Write (C : in out Component.Block.Client_Session;
                    I :     Component.Request_Id;
                    D :    out Component.Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (I);
   begin
      D := (others => Component.Byte'First);
   end Write;

end Completeness;
