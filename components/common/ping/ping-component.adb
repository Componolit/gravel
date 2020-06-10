
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

   subtype Desc_Index is Positive range 1 .. 1;

   package Packet is new Gneiss.Packet (Types.Index, Types.Byte, Types.Bytes, Desc_Index);

   package ICMP_Buf is new Buffer (Buffer_Size);

   procedure Update (Session : in out Packet.Client_Session;
                     Idx     :        Desc_Index;
                     Buf     :    out Types.Bytes;
                     Ctx     : in out ICMP_Buf.Bytes);

   procedure Read (Session : in out Packet.Client_Session;
                   Idx     :        Desc_Index;
                   Buf     :        Types.Bytes;
                   Ctx     : in out ICMP_Buf.Bytes);

   package Packet_Client is new Packet.Client (ICMP_Buf.Bytes, Event, Update, Read);
   package Timer_Client is new Gneiss_Timer.Client (Timer_Event);

   function Image is new Basalt.Strings_Generic.Image_Modular (ICMP.Sequence_Number);
   function Image is new Basalt.Strings_Generic.Image_Modular (ICMP.Checksum);

   Client         : Packet.Client_Session;
   Log            : Gneiss_Log.Client_Session;
   Trigger        : Gneiss_Timer.Client_Session;
   Capability     : Gneiss.Capability;
   Desc           : Packet.Descriptor;
   Sent_Time      : Gneiss_Timer.Time := 0.0;
   Seq            : ICMP.Sequence_Number := 0;

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
      Data_Ptr        : ICMP_Buf.Bytes;
      procedure Process_Data (Buffer : Types.Bytes);
      procedure Process_Data (Buffer : Types.Bytes)
      is
      begin
         Packet_Checksum := Checksum.Echo_Request_Reply_Checksum (ICMP.Echo_Reply,
                                                                  0,
                                                                  Identifier,
                                                                  Sequence_Number,
                                                                  Buffer);
      end Process_Data;
      procedure Generate_Checksum is new Echo.Get_Data (Process_Data);
   begin
      if
         not Gneiss_Timer.Initialized (Trigger)
         or else not Gneiss_Log.Initialized (Log)
         or else not Packet.Initialized (Client)
         or else ICMP_Buf.Ptr = null
         or else ICMP_Buf.Ptr'Length /= Buffer_Size
      then
         return;
      end if;
      Received := Timer_Client.Clock (Trigger);
      Timer_Client.Set_Timeout (Trigger, 1.0);
      if Packet_Client.Allocated (Client, Desc) then
         Log_Client.Warning (Log, "Descriptor already allocated");
         return;
      end if;
      Packet_Client.Receive (Client, Desc, 1);
      if not Packet_Client.Allocated (Client, Desc) then
         return;
      end if;
      Data_Ptr := (ICMP_Buf.Ptr, 0);
      Packet_Client.Read (Client, Desc, Data_Ptr);
      Packet_Client.Free (Client, Desc);
      Echo.Initialize (Context, Data_Ptr.Data,
                       RFLX_Types.First_Bit_Index (Data_Ptr.Data'First),
                       RFLX_Types.Last_Bit_Index (Data_Ptr.Data'First + Data_Ptr.Length - 1));
      Echo.Verify_Message (Context);
      if
         Echo.Structural_Valid_Message (Context)
         and then Echo.Get_Tag (Context) = ICMP.Echo_Reply
         and then Echo.Get_Code_Zero (Context) = 0
      then
         Identifier      := Echo.Get_Identifier (Context);
         Sequence_Number := Echo.Get_Sequence_Number (Context);
         Generate_Checksum (Context);
         Log_Client.Info (Log, "seq="
                               & Image (Sequence_Number)
                               & " time="
                               & Basalt.Strings.Image (Duration (Received - Sent_Time))
                               & " checksum="
                               & Image (Echo.Get_Checksum (Context), 16)
                               & " ("
                               & Image (Packet_Checksum, 16)
                               & ")");
      end if;
      Echo.Take_Buffer (Context, ICMP_Buf.Ptr);
   end Event;

   procedure Timer_Event
   is
      use type ICMP.Sequence_Number;
      use type Types.Length;
      use type Types.Bytes_Ptr;
      Context  : Echo.Context;
      Packet_Checksum : ICMP.Checksum;
      Data_Ptr : ICMP_Buf.Bytes;
      procedure Process_Data (Buffer : out Types.Bytes);
      procedure Process_Data (Buffer : out Types.Bytes)
      is
      begin
         Buffer          := (others => 16#65#);
         Packet_Checksum := Checksum.Echo_Request_Reply_Checksum (ICMP.Echo_Request,
                                                                  0,
                                                                  16#0#,
                                                                  Seq,
                                                                  Buffer);
      end Process_Data;
      procedure Set_Data is new Echo.Set_Bounded_Data (Process_Data);
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
      if Packet_Client.Allocated (Client, Desc) then
         Log_Client.Warning (Log, "Descriptor already allocated.");
         return;
      end if;
      Packet_Client.Allocate (Client, Desc, 16, 1);
      if not Packet_Client.Allocated (Client, Desc) then
         Log_Client.Warning (Log, "Failed to allocate descriptor.");
         return;
      end if;
      Echo.Initialize (Context, ICMP_Buf.Ptr);
      Echo.Set_Tag (Context, ICMP.Echo_Request);
      Echo.Set_Code_Zero (Context, 0);
      Echo.Set_Checksum (Context, 16#0#);
      Echo.Set_Identifier (Context, 16#0#);
      Echo.Set_Sequence_Number (Context, Seq);
      Set_Data (Context, 64);
      Echo.Set_Checksum (Context, Packet_Checksum);
      Echo.Take_Buffer (Context, ICMP_Buf.Ptr);
      Data_Ptr := (ICMP_Buf.Ptr, 16);
      Packet_Client.Update (Client, Desc, Data_Ptr);
      ICMP_Buf.Ptr := Data_Ptr.Data;
      Packet_Client.Send (Client, Desc);
      Seq := Seq + 1;
      Sent_Time := Timer_Client.Clock (Trigger);
      if Packet_Client.Allocated (Client, Desc) then
         Log_Client.Warning (Log, "Failed to send request.");
         Packet_Client.Free (Client, Desc);
      end if;
   end Timer_Event;

   procedure Update (Session : in out Packet.Client_Session;
                     Idx     :        Desc_Index;
                     Buf     :    out Types.Bytes;
                     Ctx     : in out ICMP_Buf.Bytes)
   is
      pragma Unreferenced (Session);
      pragma Unreferenced (Idx);
      use type Types.Length;
   begin
      if Buf'Length < 1 then
         return;
      end if;
      if Ctx.Length > Ctx.Data'Length then
         Ctx.Length := Ctx.Data'Length;
      end if;
      if Buf'Length <= Ctx.Length then
         Buf := Ctx.Data.all (Ctx.Data'First .. Ctx.Data'First + Buf'Length - 1);
      else
         Buf := (others => 0);
         Buf (Buf'First .. Buf'First + Ctx.Length - 1) := Ctx.Data.all;
      end if;
   end Update;

   procedure Read (Session : in out Packet.Client_Session;
                   Idx     :        Desc_Index;
                   Buf     :        Types.Bytes;
                   Ctx     : in out ICMP_Buf.Bytes)
   is
      pragma Unreferenced (Session);
      pragma Unreferenced (Idx);
      use type Types.Length;
   begin
      if Buf'Length < 1 then
         return;
      end if;
      if Buf'Length <= Ctx.Data'Length then
         Ctx.Data.all (Ctx.Data'First .. Ctx.Data'First + Buf'Length - 1) := Buf;
         Ctx.Length := Buf'Length;
      else
         Ctx.Data.all := Buf (Buf'First .. Buf'First + Ctx.Data'Length - 1);
         Ctx.Length   := Ctx.Data'Length;
      end if;
   end Read;

   procedure Destruct
   is
   begin
      Log_Client.Finalize (Log);
      Packet_Client.Finalize (Client);
      Timer_Client.Finalize (Trigger);
   end Destruct;

end Ping.Component;
