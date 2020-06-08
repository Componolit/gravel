
with Gneiss.Log;
with Gneiss.Log.Client;
with Gneiss.Packet;
with Gneiss.Packet.Client;
with Gneiss.Timer;
with Gneiss.Timer.Client;
with Buffer;
with RFLX_Builtin_Types;
with ICMP;
with ICMP.Message;
with Basalt.Strings;
with Basalt.Strings_Generic;
with Checksum;

package body Component with
   SPARK_Mode
is

   package Types renames RFLX_Builtin_Types;
   package Echo renames ICMP.Message;

   package Gneiss_Log is new Gneiss.Log;
   package Log_Client is new Gneiss_Log.Client;
   package Gneiss_Timer is new Gneiss.Timer;

   procedure Event;

   procedure Timer_Event;

   subtype Desc_Index is Positive range 1 .. 1;

   package Packet is new Gneiss.Packet (Types.Index, Types.Byte, Types.Bytes, Desc_Index);

   package ICMP_Buf is new Buffer (1024);

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
      use type Gneiss_Timer.Time;
      Context : Echo.Context;
      Received : Gneiss_Timer.Time;
   begin
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
      Packet_Client.Read (Client, Desc, ICMP_Buf.Ptr.all);
      Packet_Client.Free (Client, Desc);
      Echo.Initialize (Context, ICMP_Buf.Ptr);
      Echo.Verify_Message (Context);
      if
         Echo.Structural_Valid_Message (Context)
         and then Echo.Get_Tag (Context) = ICMP.Echo_Reply
         and then Echo.Get_Code_Zero (Context) = 0
      then
         Log_Client.Info (Log, "seq="
                               & Image (Echo.Get_Sequence_Number (Context))
                               & " time="
                               & Basalt.Strings.Image (Duration (Received - Sent_Time))
                               & " checksum="
                               & Image (Echo.Get_Checksum (Context), 16)
                               & "("
                               & Image (Checksum.Echo_Request_Reply_Checksum (ICMP.Echo_Reply,
                                                                              Echo.Get_Code_Zero (Context),
                                                                              Echo.Get_Identifier (Context),
                                                                              Echo.Get_Sequence_Number (Context),
                                                                              (1 .. 0 => 0)),
                                        16)
                               & ")");
      end if;
      Echo.Take_Buffer (Context, ICMP_Buf.Ptr);
   end Event;

   procedure Timer_Event
   is
      use type ICMP.Sequence_Number;
      Context : Echo.Context;
   begin
      Timer_Client.Set_Timeout (Trigger, 1.0);
      if Packet_Client.Allocated (Client, Desc) then
         Log_Client.Warning (Log, "Descriptor already allocated.");
         return;
      end if;
      Packet_Client.Allocate (Client, Desc, 8, 1);
      if not Packet_Client.Allocated (Client, Desc) then
         Log_Client.Warning (Log, "Failed to allocate descriptor.");
         return;
      end if;
      Echo.Initialize (Context, ICMP_Buf.Ptr);
      Echo.Set_Tag (Context, ICMP.Echo_Request);
      Echo.Set_Code_Zero (Context, 0);
      Echo.Set_Checksum (Context, Checksum.Echo_Request_Reply_Checksum (ICMP.Echo_Request,
                                                                        0,
                                                                        16#0#,
                                                                        Seq,
                                                                        (1 .. 0 => 0)));
      Echo.Set_Identifier (Context, 16#0#);
      Echo.Set_Sequence_Number (Context, Seq);
      Echo.Take_Buffer (Context, ICMP_Buf.Ptr);
      Packet_Client.Update (Client, Desc, ICMP_Buf.Ptr.all);
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
      if Buf'Length <= Ctx'Length then
         Buf := Ctx (Ctx'First .. Ctx'First + Buf'Length - 1);
      else
         Buf := (others => 0);
         Buf (Buf'First .. Buf'First + Ctx'Length - 1) := Ctx;
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
      if Buf'Length <= Ctx'Length then
         Ctx (Ctx'First .. Ctx'First + Buf'Length - 1) := Buf;
      else
         Ctx := Buf (Buf'First .. Buf'First + Ctx'Length - 1);
      end if;
   end Read;

   procedure Destruct
   is
   begin
      Log_Client.Finalize (Log);
      Packet_Client.Finalize (Client);
      Timer_Client.Finalize (Trigger);
   end Destruct;

end Component;
