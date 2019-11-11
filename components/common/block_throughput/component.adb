
with Componolit.Gneiss.Block;
with Componolit.Gneiss.Block.Client;
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Rom;
with Componolit.Gneiss.Rom.Client;
with Componolit.Gneiss.Timer;
with Componolit.Gneiss.Timer.Client;
with Componolit.Gneiss.Strings_Generic;
with Config;
with Output;

package body Component with
   SPARK_Mode
is
   type Byte is mod 2**8;
   subtype Unsigned_Long is Long_Integer range 0 .. Long_Integer'Last;
   type Buffer is array (Unsigned_Long range <>) of Byte;
   type Request_Id is mod 2 ** 6;
   type Session_Id is new Integer;

   Internal_Buffer_Size : constant := 4 * 1024 ** 2;

   package Block is new Gns.Block (Byte, Unsigned_Long, Buffer, Session_Id, Request_Id);
   package Conf is new Config (Block);
   package Conf_Client is new Gns.Rom.Client (Character, Positive, String, Conf.Parse);

   function Image is new Output.Byte_Image (Block.Byte_Length);
   function Image is new Gns.Strings_Generic.Image_Ranged (Block.Size);
   function Image is new Gns.Strings_Generic.Image_Ranged (Block.Count);

   procedure Block_Event;
   procedure Timer_Event;

   procedure Read (C : in out Block.Client_Session;
                   I :        Request_Id;
                   D :        Buffer);

   procedure Write (C : in out Block.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer);

   package Block_Client is new Block.Client (Block_Event, Read, Write);
   package Timer_Client is new Gns.Timer.Client (Timer_Event);

   type Request_Cache is array (Request_Id'Range) of Block_Client.Request;

   Test_Buffer   : Buffer (1 .. Internal_Buffer_Size);
   Cache         : Request_Cache;
   Capability    : Gns.Types.Capability;
   Log           : Gns.Log.Client_Session;
   Xml           : Gns.Log.Client_Session;
   Rom           : Gns.Rom.Client_Session;
   Timer         : Gns.Timer.Client_Session;
   Client        : Block.Client_Session;
   Request_Count : Unsigned_Long;
   Current       : Block.Id          := 0;
   Sent          : Unsigned_Long     := 0;
   Received      : Unsigned_Long     := 0;
   Start         : Gns.Timer.Time    := Gns.Timer.Time'First;
   Data_Size     : Block.Byte_Length := 0;

   procedure Construct (Cap : Gns.Types.Capability)
   is
      use type Block.Count;
      use type Block.Size;
      use type Block.Byte_Length;
   begin
      Capability := Cap;
      if not Gns.Log.Initialized (Log) then
         Gns.Log.Client.Initialize (Log, Capability, "Throughput");
      end if;
      if not Gns.Log.Initialized (Log) then
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Gns.Log.Client.Info (Log, "Throughput test");
      if not Gns.Rom.Initialized (Rom) then
         Conf_Client.Initialize (Rom, Capability);
      end if;
      if not Gns.Rom.Initialized (Rom) then
         Gns.Log.Client.Error (Log, "Failed to initialize config rom");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      if not Gns.Timer.Initialized (Timer) then
         Timer_Client.Initialize (Timer, Capability);
      end if;
      if not Gns.Timer.Initialized (Timer) then
         Gns.Log.Client.Error (Log, "Failed to initialize timer");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Conf_Client.Load (Rom);
      if not Conf.Initialized then
         Gns.Log.Client.Error (Log, "Failed to read configuration:");
         Gns.Log.Client.Error (Log, Conf.Failure_Reason);
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      if not Gns.Log.Initialized (Xml) then
         Gns.Log.Client.Initialize (Xml, Capability, "XML");
      end if;
      Gns.Log.Client.Info (Log, "Device: " & Conf.Device);
      Gns.Log.Client.Info (Log, "Request size: " & Image (Conf.Request_Size));
      Gns.Log.Client.Info (Log, "Test size: " & Image (Conf.Data_Size));
      Gns.Log.Client.Info (Log, "Buffer size: " & Image (Conf.Buffer_Size));
      if not Block.Initialized (Client) then
         Block_Client.Initialize (Client, Capability, Conf.Device, 42, Conf.Buffer_Size);
      end if;
      if not Block.Initialized (Client) then
         Gns.Log.Client.Error (Log, "Failed to initialize block device");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      if Conf.Request_Size mod Block.Block_Size (Client) /= 0 then
         Gns.Log.Client.Error (Log, "Invalid request size for block device");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Data_Size := Block.Block_Size (Client) * Block.Block_Count (Client);
      if Data_Size < Conf.Data_Size then
         Gns.Log.Client.Info (Log, "Disk: " & Image (Data_Size));
      else
         Data_Size := Conf.Data_Size;
      end if;
      if
         Block.Byte_Length (Conf.Request_Size) > Internal_Buffer_Size
         or else Block.Byte_Length (Conf.Request_Size) > Conf.Buffer_Size
      then
         Gns.Log.Client.Error (Log, "Request size too large");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Gns.Log.Client.Info (Log, "Block device with " & Image (Block.Block_Count (Client))
                                & " blocks @ " & Image (Block.Block_Size (Client))
                                & " b ("
                                & Image (Block.Byte_Length (Block.Block_Count (Client)
                                                            * Block.Count (Block.Block_Size (Client))))
                                & ")");
      if Gns.Log.Initialized (Xml) then
         Output.Xml_Start (Xml,
                           Long_Integer (Conf.Request_Size),
                           Long_Integer (Data_Size),
                           (case Conf.Operation is
                              when Block.Read      => "read",
                              when Block.Write     => "write",
                              when Block.Sync      => "sync",
                              when Block.Trim      => "trim",
                              when Block.None      => "none",
                              when Block.Undefined => "Undefined"),
                           Long_Integer (Conf.Buffer_Size),
                           Long_Integer (Cache'Length));
      else
         Gns.Log.Client.Warning (Log, "Failed to initialize XML output");
      end if;
      Gns.Log.Client.Info (Log, "Initializing buffer...");
      for I in Test_Buffer'Range loop
         Test_Buffer (I) := Byte (I mod 2 ** 8);
      end loop;
      Request_Count := Unsigned_Long (Data_Size / Block.Byte_Length (Conf.Request_Size));
      Gns.Log.Client.Info (Log, "Starting test");
      Start := Timer_Client.Clock (Timer);
      Timer_Client.Set_Timeout (Timer, 1.0);
      Block_Event;
   end Construct;

   procedure Destruct
   is
      Current : Gns.Timer.Time;
   begin
      if
         Gns.Timer.Initialized (Timer)
         and then Conf.Initialized
         and then Block.Initialized (Client)
      then
         Current := Timer_Client.Clock (Timer);
         if Gns.Log.Initialized (Log) then
            Output.Info (Log, Start, Current, Received, Long_Integer (Conf.Request_Size));
         end if;
         if Gns.Log.Initialized (Xml) then
            Output.Xml_Element (Xml, Start, Current, Received);
            Output.Xml_End (Xml);
         end if;
      end if;
      if Gns.Log.Initialized (Log) then
         Gns.Log.Client.Finalize (Log);
      end if;
      if Gns.Rom.Initialized (Rom) then
         Conf_Client.Finalize (Rom);
      end if;
      if Block.Initialized (Client) then
         for I in Cache'Range loop
            Block_Client.Release (Client, Cache (I));
         end loop;
         Block_Client.Finalize (Client);
      end if;
      if Gns.Timer.Initialized (Timer) then
         Timer_Client.Finalize (Timer);
      end if;
   end Destruct;

   procedure Block_Event
   is
      use type Block.Size;
      use type Block.Request_Kind;
      use type Block.Request_Status;
      use type Block.Count;
      Progress : Boolean := True;
      Result   : Block.Result;
      Count    : Block.Count;
   begin
      Count := Block.Count (Conf.Request_Size / Block.Block_Size (Client));
      while Progress loop
         Progress := False;
         for I in Cache'Range loop
            case Block_Client.Status (Cache (I)) is
               when Block.Raw =>
                  if Sent < Request_Count then
                     Block_Client.Allocate_Request (Client,
                                                    Cache (I),
                                                    Conf.Operation,
                                                    Current,
                                                    Count,
                                                    I,
                                                    Result);
                     case Result is
                        when Block.Success =>
                           Sent     := Sent + 1;
                           Progress := True;
                           Current  := Current + Count;
                           if Block_Client.Kind (Cache (I)) = Block.Write then
                              Block_Client.Write (Client, Cache (I));
                           end if;
                        when Block.Unsupported =>
                           Gns.Log.Client.Error (Log, "Unable to allocate request");
                           Main.Vacate (Capability, Main.Failure);
                           return;
                        when others =>
                           null;
                     end case;
                  end if;
               when Block.Allocated =>
                  Block_Client.Enqueue (Client, Cache (I));
                  Progress := Progress or else Block_Client.Status (Cache (I)) = Block.Pending;
               when Block.Pending =>
                  Block_Client.Update_Request (Client, Cache (I));
                  Progress := Progress or else Block_Client.Status (Cache (I)) /= Block.Pending;
               when Block.Ok =>
                  if Block_Client.Kind (Cache (I)) = Block.Read then
                     Block_Client.Read (Client, Cache (I));
                  end if;
                  Received := Received + 1;
                  Block_Client.Release (Client, Cache (I));
                  Progress := True;
               when Block.Error =>
                  Block_Client.Release (Client, Cache (I));
                  Gns.Log.Client.Error (Log, "Request failed");
                  Main.Vacate (Capability, Main.Failure);
                  return;
            end case;
         end loop;
      end loop;
      if Received >= Request_Count then
         Main.Vacate (Capability, Main.Success);
      end if;
   end Block_Event;

   procedure Timer_Event
   is
      Current : Gns.Timer.Time;
   begin
      Current := Timer_Client.Clock (Timer);
      Output.Info (Log, Start, Current, Received, Long_Integer (Conf.Request_Size));
      if Gns.Log.Initialized (Xml) then
         Output.Xml_Element (Xml, Start, Current, Received);
      end if;
      Timer_Client.Set_Timeout (Timer, 1.0);
   end Timer_Event;

   procedure Read (C : in out Block.Client_Session;
                   I :        Request_Id;
                   D :        Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (I);
   begin
      Test_Buffer (Test_Buffer'First .. Test_Buffer'First + D'Length - 1) := D;
   end Read;

   procedure Write (C : in out Block.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (I);
   begin
      D := Test_Buffer (Test_Buffer'First .. Test_Buffer'First + D'Length - 1);
   end Write;

end Component;
