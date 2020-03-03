
with Gneiss.Log.Client;
with Gneiss.Message.Dispatcher;
with Gneiss.Message.Server;

package body Component with
   SPARK_Mode
is

   subtype Message_Buffer is String (1 .. 128);
   Null_Buffer : constant Message_Buffer := (others => ASCII.NUL);

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);

   procedure Initialize_Log (Session : in out Gneiss.Log.Client_Session);

   package Log_Client is new Gneiss.Log.Client (Initialize_Log);

   Cap        : Gneiss.Capability;
   Log        : Gneiss.Log.Client_Session;
   Dispatcher : Message.Dispatcher_Session;

   --
   -- Message server
   --

   procedure Initialize (Session : in out Message.Server_Session);
   procedure Finalize (Session : in out Message.Server_Session);
   procedure Recv (Session : in out Message.Server_Session;
                   Data    :        Message_Buffer);
   function Ready (Session : Message.Server_Session) return Boolean;

   package Message_Server is new Message.Server (Initialize, Finalize, Recv, Ready);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Session : in out Message.Server_Session)
   is
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Session : in out Message.Server_Session)
   is
   begin
      null;
   end Finalize;

   ----------
   -- Recv --
   ----------

   procedure Recv (Session : in out Message.Server_Session;
                   Data    :        Message_Buffer)
   is
   begin
      null;
   end Recv;

   -----------
   -- Ready --
   -----------

   function Ready (Session : Message.Server_Session) return Boolean
   is
   begin
      return False;
   end Ready;

   --
   -- Message dispatcher
   --

   procedure Dispatch (Session  : in out Message.Dispatcher_Session;
                       Disp_Cap :        Message.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String);

   package Message_Dispatcher is new Message.Dispatcher (Message_Server, Dispatch);

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (Session  : in out Message.Dispatcher_Session;
                       Disp_Cap :        Message.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String)
   is
   begin
      Log_Client.Info (Log, "Dispatching " & Name & " with label " & Label);
   end Dispatch;

   ---------------
   -- Construct --
   ---------------

   procedure Construct (Capability : Gneiss.Capability)
   is
      use type Gneiss.Session_Status;
   begin
      Cap := Capability;
      Log_Client.Initialize (Log, Cap, "parpen");
      Message_Dispatcher.Initialize (Dispatcher, Cap);
      if Message.Initialized (Dispatcher) then
         Message_Dispatcher.Register (Dispatcher);
      else
         if Gneiss.Log.Status (Log) = Gneiss.Initialized then
            Log_Client.Info (Log, "Error initializing message session");
         end if;
         Main.Vacate (Capability, Main.Failure);
      end if;
   end Construct;

   --------------------
   -- Initialize_Log --
   --------------------

   procedure Initialize_Log (Session : in out Gneiss.Log.Client_Session)
   is
   begin
      case Gneiss.Log.Status (Session) is
         when Gneiss.Initialized =>
            Log_Client.Info (Session, "Initialized.");
         when others =>
            Main.Vacate (Cap, Main.Failure);
      end case;
   end Initialize_Log;

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
