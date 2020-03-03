
with Gneiss.Log.Client;
with Gneiss.Message.Dispatcher;
with Gneiss.Message.Server;

package body Component with
   SPARK_Mode
is

   subtype Message_Buffer is String (1 .. 128);
   Null_Buffer : constant Message_Buffer := (others => ASCII.NUL);

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);

   procedure Initialize (Session : in out Gneiss.Log.Client_Session);

   package Log_Client is new Gneiss.Log.Client (Initialize);

   type Server_State is (Uninitialized, SHM_Wait, Initialized, Error);
   type Server_Slot is record
      Ident : String (1 .. 512) := (others => ASCII.NUL);
      State : Server_State := Uninitialized;
   end record;

   type Server_Reg is array (Gneiss.Session_Index range <>) of Message.Server_Session;
   type Server_Meta is array (Gneiss.Session_Index range <>) of Server_Slot;

   Cap        : Gneiss.Capability;
   Log        : Gneiss.Log.Client_Session;
   Dispatcher : Message.Dispatcher_Session;

   Num_Clients  : constant := 10;
   Servers      : Server_Reg (1 .. Num_Clients);
   Servers_Data : Server_Meta (1 .. Num_Clients);

   -- Message server

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
      Idx : constant Gneiss.Session_Index := Message.Index (Session).Value;
   begin
      if Idx in Servers'Range then
         case Servers_Data (Idx).State is
            when Uninitialized =>
               Servers_Data (Idx).State := SHM_Wait;
            when SHM_Wait =>
               Servers_Data (Idx).State := Error;
            when Initialized | Error =>
               null;
         end case;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Session : in out Message.Server_Session)
   is
      Idx : constant Gneiss.Session_Index := Message.Index (Session).Value;
   begin
      if Idx in Servers'Range then
         Servers_Data (Idx).State := Uninitialized;
      end if;
   end Finalize;

   ----------
   -- Recv --
   ----------

   procedure Recv (Session : in out Message.Server_Session;
                   Data    :        Message_Buffer)
   is
      Idx : constant Gneiss.Session_Index := Message.Index (Session).Value;
      use type Gneiss.Session_Status;
   begin
      if Gneiss.Log.Status (Log) = Gneiss.Initialized then
         if Idx in Servers'Range then
            case Servers_Data (Idx).State is
               when Uninitialized | Error =>
                  Log_Client.Error (Log, "Internal error: " & Data);
               when SHM_Wait =>
                  -- FIXME: SHM should be initialized, send error message
                  Log_Client.Error (Log, "SHM not initialized: " & Data);
                  null;
               when Initialized =>
                  -- FIXME: Parse message
                  Log_Client.Info (Log, "Message received: " & Data);
            end case;
         end if;
      end if;
   end Recv;

   -----------
   -- Ready --
   -----------

   function Ready (Session : Message.Server_Session) return Boolean is
      (if
          Message.Index (Session).Valid
          and then Message.Index (Session).Value in Servers_Data'Range
       then Servers_Data (Message.Index (Session).Value).State /= Uninitialized
       else False);

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
      for I in Servers'Range loop
         if not Ready (Servers (I)) then
            Message_Dispatcher.Session_Initialize (Session, Disp_Cap, Servers (I), I);
            if
               Ready (Servers (I))
               and then Message.Initialized (Servers (I))
            then
               Servers_Data (I).Ident (1 .. Name'Length + Label'Length + 1) := Name & ":" & Label;
               Message_Dispatcher.Session_Accept (Session, Disp_Cap, Servers (I));
               exit;
            end if;
         end if;
      end loop;
      for S of Servers loop
         Message_Dispatcher.Session_Cleanup (Session, Disp_Cap, S);
      end loop;
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Session : in out Gneiss.Log.Client_Session)
   is
   begin
      case Gneiss.Log.Status (Session) is
         when Gneiss.Initialized =>
            Log_Client.Info (Session, "Initialized.");
         when others =>
            Main.Vacate (Cap, Main.Failure);
      end case;
   end Initialize;

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
