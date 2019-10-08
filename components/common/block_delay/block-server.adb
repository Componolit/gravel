
with Ada.Unchecked_Conversion;
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Config;
with Jitter;

package body Block.Server with
   SPARK_Mode
is
   package Cai renames Componolit.Gneiss;

   use type Types.Request_Status;

   Capability : Cai.Types.Capability;
   Log        : Cai.Log.Client_Session;

   Request_Cache : Cache;

   function Ready (E : Cache_Entry) return Boolean
   is
      (Instance_Client.Status (E.C_Request) in Types.Ok | Types.Error);

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

   function Round_Up (Time  : Componolit.Gneiss.Timer.Time;
                      Slice : Duration) return Componolit.Gneiss.Timer.Time
   is
      use type Componolit.Gneiss.Timer.Time;
      function Convert is new Ada.Unchecked_Conversion (Long_Integer, Duration);
      function Convert is new Ada.Unchecked_Conversion (Duration, Long_Integer);
   begin
      return Time + (Slice - Convert (Convert ( Duration (Time)) mod Convert (Slice)));
   end Round_Up;

   procedure Set_Capability (Cap : Componolit.Gneiss.Types.Capability)
   is
   begin
      Capability := Cap;
   end Set_Capability;

   procedure Event
   is
      use type Cai.Timer.Time;
      Next_Interrupt : Duration;
      Current : Componolit.Gneiss.Timer.Time;
      Result : Types.Result;
      Send_Delay : Duration;
   begin
      if
         Componolit.Gneiss.Log.Initialized (Log)
         and then Componolit.Gneiss.Timer.Initialized (Timer)
         and then Types.Initialized (Client)
      then
         loop
            for I in Request_Cache'Range loop
               if Instance.Status (Request_Cache (I).S_Request) = Types.Pending then
                  if Instance_Client.Status (Request_Cache (I).C_Request) = Types.Pending then
                     Instance_Client.Update_Request (Client, Request_Cache (I).C_Request);
                  end if;
                  if Instance_Client.Status (Request_Cache (I).C_Request) = Types.Ok then
                     Instance_Client.Read (Client, Request_Cache (I).C_Request);
                  end if;
                  if Ready (Request_Cache (I)) then
                     Current := Time.Clock (Timer);
                     if
                        Ready (Request_Cache (I))
                        and then Request_Cache (I).Send_Time > 0.0
                        and then Current > Request_Cache (I).Send_Time
                     then
                        Instance.Acknowledge (Server,
                                              Request_Cache (I).S_Request,
                                              Instance_Client.Status (Request_Cache (I).C_Request));
                     end if;
                  end if;
               end if;
               if Instance.Status (Request_Cache (I).S_Request) = Types.Raw then
                  if Ready (Request_Cache (I)) then
                     Request_Cache (I).Send_Time := 0.0;
                     Instance_Client.Release (Client, Request_Cache (I).C_Request);
                  end if;
                  if Instance_Client.Status (Request_Cache (I).C_Request) = Types.Raw then
                     Instance.Process (Server, Request_Cache (I).S_Request);
                     if Instance.Status (Request_Cache (I).S_Request) = Types.Pending then
                        case Config.Get_Mode is
                           when Config.Default =>
                              Send_Delay := Config.Get_Delay;
                              case Config.Get_Jitter_Distribution is
                                 when Config.Uniform =>
                                    Jitter.Apply (Send_Delay);
                                 when others =>
                                    null;
                              end case;
                              Request_Cache (I).Send_Time := Time.Clock (Timer) + Send_Delay;
                           when Config.Sliced =>
                              Request_Cache (I).Send_Time := Round_Up (Time.Clock (Timer), Config.Get_Slice);
                        end case;
                     end if;
                  end if;
               end if;
               if Instance.Status (Request_Cache (I).S_Request) = Types.Pending then
                  if Instance_Client.Status (Request_Cache (I).C_Request) = Types.Raw then
                     Instance_Client.Allocate_Request (Client,
                                                       Request_Cache (I).C_Request,
                                                       Instance.Kind (Request_Cache (I).S_Request),
                                                       Instance.Start (Request_Cache (I).S_Request),
                                                       Instance.Length (Request_Cache (I).S_Request),
                                                       I, Result);
                  end if;
                  if Instance_Client.Status (Request_Cache (I).C_Request) = Types.Allocated then
                     Instance_Client.Enqueue (Client, Request_Cache (I).C_Request);
                  end if;
               end if;
            end loop;

            case Config.Get_Mode is
               when Config.Default =>
                  Next_Interrupt := Duration (Get_Next_Ready_Time - Time.Clock (Timer));
               when Config.Sliced =>
                  Current := Time.Clock (Timer);
                  Next_Interrupt := Duration (Round_Up (Current, Config.Get_Slice) - Current);
            end case;
            exit when Next_Interrupt > 0.0;
         end loop;
         Time.Set_Timeout (Timer, Next_Interrupt);
         Instance.Unblock_Client (Server);
      end if;
   end Event;

   function Block_Count (S : Types.Server_Session) return Types.Count
   is
      pragma Unreferenced (S);
   begin
      return Types.Block_Count (Client);
   end Block_Count;

   function Block_Size (S : Types.Server_Session) return Types.Size
   is
      pragma Unreferenced (S);
   begin
      return Types.Block_Size (Client);
   end Block_Size;

   function Writable (S : Types.Server_Session) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return Types.Writable (Client);
   end Writable;

   procedure Initialize (S : in out Types.Server_Session;
                         L :        String;
                         B :        Types.Byte_Length)
   is
      pragma Unreferenced (S);
      pragma Unreferenced (B);
      Seed : Duration;
   begin
      if not Cai.Log.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, Capability, L);
      end if;
      if Cai.Log.Initialized (Log) then
         if not Componolit.Gneiss.Timer.Initialized (Timer) then
            Time.Initialize (Timer, Capability);
         end if;
         if Componolit.Gneiss.Timer.Initialized (Timer) then
            if not Types.Initialized (Client) then
               Instance_Client.Initialize (Client, Capability, Config.Get_Client_Id, True);
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
            if Types.Initialized (Client) then
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

   function Initialized (S : Types.Server_Session) return Boolean
   is
      pragma Unreferenced (S);
   begin
      return Types.Initialized (Client);
   end Initialized;

   procedure Finalize (S : in out Types.Server_Session)
   is
      pragma Unreferenced (S);
   begin
      if Cai.Log.Initialized (Log) then
         Cai.Log.Client.Finalize (Log);
      end if;
      if Types.Initialized (Client) then
         Instance_Client.Finalize (Client);
      end if;
   end Finalize;

   procedure Write (C : in out Types.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer)
   is
      pragma Unreferenced (C);
   begin
      Instance.Write (Server, Request_Cache (I).S_Request, D);
   end Write;

   procedure Read (C : in out Types.Client_Session;
                   I :        Request_Id;
                   D :        Buffer)
   is
      pragma Unreferenced (C);
   begin
      Instance.Read (Server, Request_Cache (I).S_Request, D);
   end Read;

end Block.Server;
