
with Gneiss.Log;
with Gneiss.Log.Client;
with Gneiss.Packet;
with Gneiss.Packet.Client;
with Gneiss.Timer;
with Gneiss.Timer.Client;
with Ping.Buffer;
with RFLX_Types;
with RFLX_Builtin_Types;
with ICMP;
with ICMP.Message;
with Basalt.Strings;
with Basalt.Strings_Generic;
with Checksum;

package body Ping.Component with
   SPARK_Mode
is

   Buffer_Size : constant := 1024;

   package Types renames RFLX_Builtin_Types;
   package Echo renames ICMP.Message;

   package Gneiss_Log is new Gneiss.Log;
   package Log_Client is new Gneiss_Log.Client;
   package Gneiss_Timer is new Gneiss.Timer;

   procedure Event;

   procedure Timer_Event;

   package Packet is new Gneiss.Packet (Types.Index, Types.Byte, Types.Bytes);

   package ICMP_Buf is new Buffer (Buffer_Size);

   package Packet_Client is new Packet.Client (Event);
   package Timer_Client is new Gneiss_Timer.Client (Timer_Event);

   function Image is new Basalt.Strings_Generic.Image_Modular (ICMP.Sequence_Number);
   function Image is new Basalt.Strings_Generic.Image_Modular (ICMP.Checksum);

   Client     : Packet.Client_Session;
   Log        : Gneiss_Log.Client_Session;
   Trigger    : Gneiss_Timer.Client_Session;
   Capability : Gneiss.Capability;
   Sent_Time  : Gneiss_Timer.Time := 0.0;
   Seq        : ICMP.Sequence_Number := 0;

   procedure Construct (Cap : Gneiss.Capability)
   is
   begin
      Capability := Cap;
      Log_Client.Initialize (Log, Capability, "log_packet");
      Packet_Client.Initialize (Client, Capability, "log");
      Timer_Client.Initialize (Trigger, Capability, "send_timer");
      if not Gneiss_Log.Initialized (Log) then
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      if not Packet.Initialized (Client) then
         Main.Vacate (Capability, Main.Failure);
         Log_Client.Error (Log, "Failed to initialize packet client.");
         return;
      end if;
      if not Gneiss_Timer.Initialized (Trigger) then
         Main.Vacate (Capability, Main.Failure);
         Log_Client.Error (Log, "Failed to initialize timer.");
         return;
      end if;
      Timer_Event;
   end Construct;

   procedure Event
   is
      use type ICMP.Tag;
      use type ICMP.Code_Zero_Base;
      use type ICMP.Checksum;
      use type ICMP.Identifier;
      use type ICMP.Sequence_Number;
      use type Types.Length;
      use type Types.Bytes_Ptr;
      use type Gneiss_Timer.Time;
      Context         : Echo.Context;
      Received        : Gneiss_Timer.Time;
      Packet_Checksum : ICMP.Checksum;
      Identifier      : ICMP.Identifier;
      Sequence_Number : ICMP.Sequence_Number;
      Length          : Natural;
      Latency         : Duration;
      procedure Process_Data (Buffer : Types.Bytes);
      procedure Process_Data (Buffer : Types.Bytes)
      is
      begin
         Packet_Checksum := Checksum.Echo_Request_Reply_Checksum
            (ICMP.Echo_Reply, 0, Identifier, Sequence_Number, Buffer);
      end Process_Data;
      procedure Generate_Checksum is new Echo.Get_Data (Process_Data);
   begin
      if
         not Gneiss_Timer.Initialized (Trigger)
         or else not Gneiss_Log.Initialized (Log)
         or else not Packet.Initialized (Client)
         or else ICMP_Buf.Ptr = null
         or else ICMP_Buf.Ptr'Length /= Buffer_Size
         or else ICMP_Buf.Ptr'First /= 1
      then
         return;
      end if;
      Received := Timer_Client.Clock (Trigger);
      if
         Received > Sent_Time
         and Then (if Sent_Time < 0.0 then Received < Gneiss_Timer.Time'Last + Sent_Time)
      then
         Latency := Duration (Received - Sent_Time);
      else
         Latency := 0.0;
      end if;
      Timer_Client.Set_Timeout (Trigger, 1.0);
      Packet_Client.Receive (Client, ICMP_Buf.Ptr.all, Length);
      if Length > ICMP_Buf.Ptr'Length then
         Log_Client.Warning (Log, "Receive truncated packet.");
         Length := ICMP_Buf.Ptr'Length;
      end if;
      if Length < 1 then
         return;
      end if;
      Echo.Initialize (Context, ICMP_Buf.Ptr,
                       RFLX_Types.First_Bit_Index (ICMP_Buf.Ptr'First),
                       RFLX_Types.Last_Bit_Index (ICMP_Buf.Ptr'First + Types.Length (Length) - 1));
      Echo.Verify_Message (Context);
      if
         Echo.Structural_Valid_Message (Context)
         and then Echo.Get_Tag (Context) = ICMP.Echo_Reply
         and then Echo.Get_Code_Zero (Context) = 0
      then
         Identifier      := Echo.Get_Identifier (Context);
         Sequence_Number := Echo.Get_Sequence_Number (Context);
         if Echo.Present (Context, Echo.F_Data) then
            Generate_Checksum (Context);
         else
            Packet_Checksum := Checksum.Echo_Request_Reply_Checksum
               (ICMP.Echo_Reply, 0, Identifier, Sequence_Number, (1 .. 0 => 0));
         end if;
         Log_Client.Info (Log, "seq="
                               & Image (Sequence_Number)
                               & " time="
                               & Basalt.Strings.Image (Latency)
                               & " checksum="
                               & Image (Echo.Get_Checksum (Context), 16)
                               & " ("
                               & Image (Packet_Checksum, 16)
                               & ")");
      end if;
      pragma Warnings (Off, "unused assignment to ""Context""");
      Echo.Take_Buffer (Context, ICMP_Buf.Ptr);
      pragma Warnings (On, "unused assignment to ""Context""");
   end Event;

   procedure Timer_Event
   is
      use type ICMP.Sequence_Number;
      use type Types.Length;
      use type Types.Bytes_Ptr;
      use type Types.Bit_Length;
      Context         : Echo.Context;
      Success         : Boolean;
      Data            : constant Types.Bytes (1 .. 8) := (others => 65);
      function Valid_Length (L : Types.Length) return Boolean is
         (L = Data'Length);
      procedure Process_Data (Buffer : out Types.Bytes) with
         Pre => Valid_Length (Buffer'Length);
      procedure Process_Data (Buffer : out Types.Bytes)
      is
      begin
         Buffer := Data;
      end Process_Data;
      procedure Set_Data is new Echo.Set_Bounded_Data (Process_Data, Valid_Length);
   begin
      if
         not Gneiss_Timer.Initialized (Trigger)
         or else not Gneiss_Log.Initialized (Log)
         or else not Packet.Initialized (Client)
         or else ICMP_Buf.Ptr = null
         or else ICMP_Buf.Ptr'Length /= Buffer_Size
         or else ICMP_Buf.Ptr'First /= 1
      then
         return;
      end if;
      Timer_Client.Set_Timeout (Trigger, 1.0);
      Echo.Initialize (Context, ICMP_Buf.Ptr);
      Echo.Set_Tag (Context, ICMP.Echo_Request);
      Echo.Set_Code_Zero (Context, 0);
      Echo.Set_Checksum (Context, Checksum.Echo_Request_Reply_Checksum
                         (ICMP.Echo_Request, 0, 16#0#, Seq, Data));
      Echo.Set_Identifier (Context, 16#0#);
      Echo.Set_Sequence_Number (Context, Seq);
      Set_Data (Context, Data'Length * Types.Byte'Size);
      pragma Warnings (Off, "unused assignment to ""Context""");
      Echo.Take_Buffer (Context, ICMP_Buf.Ptr);
      pragma Warnings (On, "unused assignment to ""Context""");
      Packet_Client.Send (Client, ICMP_Buf.Ptr.all  (ICMP_Buf.Ptr'First .. ICMP_Buf.Ptr'First + 15), Success);
      if not Success then
         Log_Client.Warning (Log, "Failed to send packet.");
         return;
      end if;
      Seq := Seq + 1;
      Sent_Time := Timer_Client.Clock (Trigger);
   end Timer_Event;

   procedure Destruct
   is
   begin
      Log_Client.Finalize (Log);
      Packet_Client.Finalize (Client);
      Timer_Client.Finalize (Trigger);
   end Destruct;

end Ping.Component;
