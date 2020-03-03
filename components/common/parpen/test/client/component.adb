
with Gneiss.Log.Client;
with Gneiss.Message.Client;

package body Component with
   SPARK_Mode
is

   subtype Message_Buffer is String (1 .. 128);
   Null_Buffer : constant Message_Buffer := (others => ASCII.NUL);

   procedure Event;

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);

   procedure Initialize (Session : in out Message.Client_Session);
   package Message_Client is new Message.Client (Initialize, Event);

   procedure Initialize (Session : in out Gneiss.Log.Client_Session);
   package Log_Client is new Gneiss.Log.Client (Initialize);

   Cap : Gneiss.Capability;
   Log : Gneiss.Log.Client_Session;
   Msg : Message.Client_Session;

   ---------------
   -- Construct --
   ---------------

   procedure Construct (Capability : Gneiss.Capability)
   is
   begin
      Cap := Capability;
      Log_Client.Initialize (Log, Cap, "parpen_client");
   end Construct;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Session : in out Gneiss.Log.Client_Session)
   is
   begin
      case Gneiss.Log.Status (Session) is
         when Gneiss.Initialized =>
            Message_Client.Initialize (Msg, Cap, "log");
            Log_Client.Info (Session, "Initialized.");
         when others =>
            Main.Vacate (Cap, Main.Failure);
      end case;
   end Initialize;

   procedure Initialize (Session : in out Message.Client_Session)
   is
      Msg : Message_Buffer := (others => ASCII.NUL); 
      use type Gneiss.Session_Status;
   begin
      if Message.Status (Session) = Gneiss.Initialized then
         Msg (1 .. 12) := "Hello World!";
         Message_Client.Write (Session, Msg);
      else
         Main.Vacate (Cap, Main.Failure);
      end if;
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
