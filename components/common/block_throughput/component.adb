
with Componolit.Gneiss.Block;
with Componolit.Gneiss.Block.Client;
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Rom;
with Componolit.Gneiss.Rom.Client;
with Config;
with Componolit.Gneiss.Strings_Generic;

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

   function Image is new Gns.Strings_Generic.Image_Ranged (Block.Byte_Length);
   function Image is new Gns.Strings_Generic.Image_Ranged (Block.Size);

   procedure Block_Event;

   procedure Read (C : in out Block.Client_Session;
                   I :        Request_Id;
                   D :        Buffer);

   procedure Write (C : in out Block.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer);

   package Block_Client is new Block.Client (Block_Event, Read, Write);

   Test_Buffer : Buffer (1 .. Internal_Buffer_Size);

   Capability   : Gns.Types.Capability;
   Log          : Gns.Log.Client_Session;
   Rom          : Gns.Rom.Client_Session;
   Client       : Block.Client_Session;
   Request_Size : Block.Size;
   Data_Size    : Block.Byte_Length;

   procedure Construct (Cap : Gns.Types.Capability)
   is
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
      Conf_Client.Load (Rom);
      if not Conf.Initialized then
         Gns.Log.Client.Error (Log, "Failed to read configuration:");
         Gns.Log.Client.Error (Log, Conf.Failure_Reason);
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Gns.Log.Client.Info (Log, "Device: " & Conf.Device);
      Gns.Log.Client.Info (Log, "Request size: " & Image (Conf.Request_Size));
      Gns.Log.Client.Info (Log, "Test size: " & Image (Conf.Data_Size));
      Gns.Log.Client.Info (Log, "Buffer size: " & Image (Conf.Buffer_Size));
      if not Block.Initialized (Client) then
         Block_Client.Initialize (Client, Capability, Conf.Device, 42, Conf.Buffer_Size);
      end if;
      if not Block.Initialized (Client) then
         Gns.Log.Client.Error (Log, "Failed to initialized block device");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      if Conf.Request_Size mod Block.Block_Size (Client) /= 0 then
         Gns.Log.Client.Error (Log, "Invalid request size for block device");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      if
         Block.Byte_Length (Conf.Request_Size) > Internal_Buffer_Size
         or else Block.Byte_Length (Conf.Request_Size) > Conf.Buffer_Size
      then
         Gns.Log.Client.Error (Log, "Request size too large");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Gns.Log.Client.Info (Log, "Initializing buffer...");
      for I in Test_Buffer'Range loop
         Test_Buffer (I) := Byte (I mod 2 ** 8);
      end loop;
      Gns.Log.Client.Info (Log, "Starting test");
      Block_Event;
   end Construct;

   procedure Destruct
   is
   begin
      if Gns.Log.Initialized (Log) then
         Gns.Log.Client.Finalize (Log);
      end if;
      if Gns.Rom.Initialized (Rom) then
         Conf_Client.Finalize (Rom);
      end if;
      if Block.Initialized (Client) then
         Block_Client.Finalize (Client);
      end if;
   end Destruct;

   procedure Block_Event
   is
   begin
      null;
   end Block_Event;

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
