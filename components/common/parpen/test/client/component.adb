with Gneiss.Log.Client;
with Gneiss.Message.Client;
with Gneiss.Memory.Client;

package body Component with
   SPARK_Mode
is

   subtype Message_Buffer is String (1 .. 128);
   Null_Buffer : constant Message_Buffer := (others => ASCII.NUL);

   procedure Event;

   procedure Initialize (Session : in out Gneiss.Log.Client_Session);
   package Log_Client is new Gneiss.Log.Client (Initialize);

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);
   procedure Initialize (Session : in out Message.Client_Session);
   package Message_Client is new Message.Client (Initialize, Event);

   package Memory is new Gneiss.Memory (Character, Positive, String);
   procedure Modify (Session : in out Memory.Client_Session;
                     Data    : in out String);
   procedure Initialize (Session : in out Memory.Client_Session);
   package Memory_Client is new Memory.Client (Initialize, Modify);

   Cap : Gneiss.Capability;
   Log : Gneiss.Log.Client_Session;
   Msg : Message.Client_Session;
   Mem : Memory.Client_Session;

   ---------------
   -- Construct --
   ---------------

   procedure Construct (Capability : Gneiss.Capability)
   is
      Msg_Buffer : Message_Buffer := (others => ASCII.NUL);
      -- FIXME: Generate label
      Label : String := ASCII.ESC & "prpn" & ASCII.NUL;
      use type Gneiss.Session_Status;
   begin
      Cap := Capability;
      Log_Client.Initialize (Log, Cap, "parpen_client");

      Message_Client.Initialize (Msg, Cap, Label);
      if Message.Status (Msg) /= Gneiss.Initialized then
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;

      Log_Client.Info (Log, "Initializing shared memory");
      Memory_Client.Initialize (Mem, Cap, Label, 4096);
      if Memory.Status (Mem) /= Gneiss.Initialized then
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;

      Log_Client.Info (Log, "Sending message");
      Msg_Buffer (1 .. 12) := "Hello World!";
      Message_Client.Write (Msg, Msg_Buffer);
   end Construct;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Session : in out Gneiss.Log.Client_Session)
   is
   begin
      null;
   end Initialize;

   procedure Initialize (Session : in out Message.Client_Session)
   is
   begin
      null;
   end Initialize;

   procedure Initialize (Session : in out Memory.Client_Session)
   is
   begin
      null;
   end Initialize;

   -----------
   -- Event --
   -----------

   procedure Event
   is
      use type Gneiss.Session_Status;
   begin
      if Gneiss.Log.Status (Log) = Gneiss.Initialized then
         Log_Client.Info (Log, "Event...");
      end if;
   end Event;

   ------------
   -- Modify --
   ------------

   procedure Modify (Session : in out Memory.Client_Session;
                     Data    : in out String)
   is
   begin
      null;
   end Modify;

   --------------
   -- Destruct --
   --------------

   procedure Destruct
   is
      use type Gneiss.Session_Status;
   begin
      if Gneiss.Log.Status (Log) = Gneiss.Initialized then
         Log_Client.Info (Log, "Destructing...");
      end if;
      Log_Client.Finalize (Log);
   end Destruct;

end Component;
