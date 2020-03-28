
with Gneiss.Log.Client;
with Gneiss.Message.Dispatcher;
with Gneiss.Message.Server;
with Gneiss.Memory.Server;
with Gneiss.Memory.Dispatcher;

with Parpen.Generic_Types;
with Parpen.Protocol.Generic_Label;
with Parpen.Protocol.Generic_Reply;
with Parpen.Protocol.Generic_Request;
with Parpen.Container;

with Parpen.Message;

package body Component with
   SPARK_Mode
is
   Num_Clients  : constant := 10;
   Label_Length : constant := 128;

   subtype Client_ID is Gneiss.Session_Index range 1 .. Num_Clients;
   subtype Message_Buffer is String (1 .. 128);
   Null_Buffer : constant Message_Buffer := (others => ASCII.NUL);

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);

   procedure Initialize (Session : in out Gneiss.Log.Client_Session);
   package Log_Client is new Gneiss.Log.Client (Initialize);

   package Memory is new Gneiss.Memory (Character, Positive, String);

   type String_Ptr is access all String;
   type Bit_Length is range 0 .. Natural'Last * 8;

   package Types is new Parpen.Generic_Types (Index      => Positive,
                                              Byte       => Character,
                                              Bytes      => String,
                                              Bytes_Ptr  => String_Ptr,
                                              Length     => Natural,
                                              Bit_Length => Bit_Length);

   package Label_Package is new Parpen.Protocol.Generic_Label (Types);
   package Reply_Package is new Parpen.Protocol.Generic_Reply (Types);
   package Request_Package is new Parpen.Protocol.Generic_Request (Types);

   package Message_Package is new Parpen.Message (Types               => Types,
                                                  Client_ID           => Client_ID,
                                                  Num_Nodes           => 20,
                                                  Num_Handles         => 100,
                                                  Num_Name_DB_Entries => 5);

   type Server_State is (Uninitialized, SHM_Wait, Initialized, Error);
   type Server_Slot is record
      Len   : Natural := 0;
      Name  : String (1 .. 50) := (others => ASCII.NUL);
      State : Server_State := Uninitialized;
      ID    : Parpen.Protocol.Connection_ID := Parpen.Protocol.Connection_ID'First;
   end record;

   type Server_Session is
   record
      Msg : Message.Server_Session;
      Mem : Memory.Server_Session;
   end record;

   type Server_Reg is array (Client_ID) of Server_Session;
   type Server_Meta is array (Client_ID) of Server_Slot;

   Cap            : Gneiss.Capability;
   Log            : Gneiss.Log.Client_Session;
   Msg_Dispatcher : Message.Dispatcher_Session;
   Mem_Dispatcher : Memory.Dispatcher_Session;

   Servers      : Server_Reg;
   Servers_Data : Server_Meta;

   -- Memory server

   procedure Modify (Session : in out Memory.Server_Session;
                     Data    : in out String);
   procedure Initialize (Session : in out Memory.Server_Session);
   procedure Finalize (Session : in out Memory.Server_Session);
   function Ready (Session : Memory.Server_Session) return Boolean;
   procedure Dispatch (Session  : in out Memory.Dispatcher_Session;
                       Disp_Cap :        Memory.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String);
   package Memory_Server is new Memory.Server (Modify, Initialize, Finalize, Ready);
   package Memory_Dispatcher is new Memory.Dispatcher (Memory_Server, Dispatch);

   -- Message server

   procedure Initialize (Session : in out Message.Server_Session);
   procedure Finalize (Session : in out Message.Server_Session);
   procedure Recv (Session : in out Message.Server_Session;
                   Data    :        Message_Buffer);
   function Ready (Session : Message.Server_Session) return Boolean;
   procedure Dispatch (Session  : in out Message.Dispatcher_Session;
                       Disp_Cap :        Message.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String);

   package Message_Server is new Message.Server (Initialize, Finalize, Recv, Ready);
   package Message_Dispatcher is new Message.Dispatcher (Message_Server, Dispatch);

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

   procedure Initialize (Session : in out Memory.Server_Session)
   is
      Idx : constant Gneiss.Session_Index := Memory.Index (Session).Value;
   begin
      if Idx in Servers'Range then
         case Servers_Data (Idx).State is
            when Uninitialized =>
               Servers_Data (Idx).State := SHM_Wait;
            when SHM_Wait =>
               Servers_Data (Idx).State := Initialized;
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

   procedure Finalize (Session : in out Memory.Server_Session)
   is
      Idx : constant Gneiss.Session_Index := Memory.Index (Session).Value;
   begin
      if Idx in Servers'Range then
         Servers_Data (Idx).State := Uninitialized;
      end if;
   end Finalize;

   --------------------
   -- Handle_Message --
   --------------------

   procedure Handle_Message (Session : in out Message.Server_Session;
                             Data    :        Message_Buffer)
   is
      package Request is new Parpen.Container (Types, Message_Buffer'Length);
      package Reply is new Parpen.Container (Types, Message_Buffer'Length);
      Request_Context : Request_Package.Context := Request_Package.Create;
      Reply_Context   : Reply_Package.Context := Reply_Package.Create;
      Transaction     : Message_Package.Transaction;
      pragma Unreferenced (Transaction);
   begin
      Log_Client.Info (Log, "Message received");
      Request.Ptr.all (1 .. Data'Length) := Data;
      Request_Package.Initialize (Request_Context, Request.Ptr);
      Request_Package.Verify_Message (Request_Context);
      if Request_Package.Structural_Valid_Message (Request_Context) then
         Log_Client.Info (Log, "Received valid Request");
         return;
      end if;

      Log_Client.Error (Log, "Invalid request");
      Reply_Package.Initialize (Reply_Context, Reply.Ptr);
      Reply_Package.Set_Tag (Reply_Context, Parpen.Protocol.REPLY_ERROR);
      Reply_Package.Take_Buffer (Reply_Context, Reply.Ptr);
      Message_Server.Send (Session, Reply.Ptr.all);
   end Handle_Message;

   ----------
   -- Recv --
   ----------

   procedure Recv (Session : in out Message.Server_Session;
                   Data    :        Message_Buffer)
   is
      Idx   : constant Gneiss.Session_Index := Message.Index (Session).Value;
      package Reply is new Parpen.Container (Types, Message_Buffer'Length);
      Context : Reply_Package.Context := Reply_Package.Create;
      use type Gneiss.Session_Status;
   begin
      Reply_Package.Initialize (Context, Reply.Ptr);
      if Gneiss.Log.Status (Log) = Gneiss.Initialized then
         if Idx in Servers'Range then
            case Servers_Data (Idx).State is
               when Uninitialized | Error =>
                  Log_Client.Error (Log, "Internal error: " & Data);
               when SHM_Wait =>
                  Log_Client.Error (Log, "SHM not initialized");
                  Reply_Package.Set_Tag (Context, Parpen.Protocol.REPLY_ERROR);
                  if not Reply_Package.Valid_Message (Context) then
                     Log_Client.Error (Log, "Invalid reply");
                     return;
                  end if;
                  Reply_Package.Take_Buffer (Context, Reply.Ptr);
                  Message_Server.Send (Session, Reply.Ptr.all);
               when Initialized =>
                  Handle_Message (Session, Data);
            end case;
         end if;
      end if;
   end Recv;

   ------------
   -- Modify --
   ------------

   procedure Modify (Session : in out Memory.Server_Session;
                     Data    : in out String)
   is
      pragma Unreferenced (Session, Data);
   begin
      Log_Client.Info (Log, "MEMORY: modify called");
   end Modify;

   -----------
   -- Ready --
   -----------

   function Ready (Session : Message.Server_Session) return Boolean is
      (if
          Message.Index (Session).Valid
          and then Message.Index (Session).Value in Servers_Data'Range
       then Servers_Data (Message.Index (Session).Value).State /= Uninitialized
       else False);

   function Ready (Session : Memory.Server_Session) return Boolean is
      (if
          Memory.Index (Session).Valid
          and then Memory.Index (Session).Value in Servers_Data'Range
       then Servers_Data (Memory.Index (Session).Value).State /= Uninitialized
       else False);

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (Session  : in out Message.Dispatcher_Session;
                       Disp_Cap :        Message.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String)
   is
      Context : Label_Package.Context := Label_Package.Create;
      package L is new Parpen.Container (Types, Label_Length);
   begin
      if L.Ptr.all'Length < Label'Length then
         return;
      end if;
      L.Ptr.all (1 .. Label'Length) := Label;
      Label_Package.Initialize (Context, L.Ptr);
      Label_Package.Verify_Message (Context);

      if Label_Package.Valid (Context, Label_Package.F_Connection_ID) then
         for I in Servers'Range loop
            if not Ready (Servers (I).Msg) then
               Message_Dispatcher.Session_Initialize (Session, Disp_Cap, Servers (I).Msg, I);
               if
                  Ready (Servers (I).Msg)
                  and then Message.Initialized (Servers (I).Msg)
               then
                  Servers_Data (I).Len := Name'Length;
                  Servers_Data (I).Name (1 .. Name'Length) := Name;
                  Servers_Data (I).State := SHM_Wait;
                  Servers_Data (I).ID := Label_Package.Get_Connection_ID (Context);
                  Message_Dispatcher.Session_Accept (Session, Disp_Cap, Servers (I).Msg);
                  exit;
               end if;
            end if;
         end loop;
      else
         Log_Client.Info (Log, "MEMORY: " & Name & " sent invalid label");
      end if;

      for S of Servers loop
         Message_Dispatcher.Session_Cleanup (Session, Disp_Cap, S.Msg);
      end loop;

   end Dispatch;

   procedure Dispatch (Session  : in out Memory.Dispatcher_Session;
                       Disp_Cap :        Memory.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String)
   is
      Done : Boolean := False;
      Context : Label_Package.Context := Label_Package.Create;
      package L is new Parpen.Container (Types, Label_Length);
      use type Parpen.Protocol.Connection_ID_Base;
  begin
      if L.Ptr.all'Length < Label'Length then
         return;
      end if;
      L.Ptr.all (1 .. Label'Length) := Label;
      Label_Package.Initialize (Context, L.Ptr);
      Label_Package.Verify_Message (Context);

      if Label_Package.Valid (Context, Label_Package.F_Connection_ID) then
         if Memory_Dispatcher.Valid_Session_Request (Session, Disp_Cap) then
            for I in Servers'Range loop
               if not Ready (Servers (I).Mem) then
                  Memory_Dispatcher.Session_Initialize (Session, Disp_Cap, Servers (I).Mem, I);
                  if
                     Ready (Servers (I).Mem)
                     and then Servers_Data (I).Name (1 .. Servers_Data (I).Len) = Name
                     and then Servers_Data (I).ID = Label_Package.Get_Connection_ID (Context)
                  then
                     Done := True;
                     Memory_Dispatcher.Session_Accept (Session, Disp_Cap, Servers (I).Mem);
                     exit;
                  end if;
               end if;
            end loop;
         end if;
      else
         Log_Client.Info (Log, "MEMORY: " & Name & " sent invalid label");
      end if;

      if not Done then
         Log_Client.Info (Log, "Error not matching message session found for " & Name & ":" & Label);
      end if;

      for S of Servers loop
         Memory_Dispatcher.Session_Cleanup (Session, Disp_Cap, S.Mem);
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

      Message_Dispatcher.Initialize (Msg_Dispatcher, Cap);
      if Message.Initialized (Msg_Dispatcher) then
         Message_Dispatcher.Register (Msg_Dispatcher);
      else
         if Gneiss.Log.Status (Log) = Gneiss.Initialized then
            Log_Client.Info (Log, "Error initializing message session");
         end if;
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;

      Memory_Dispatcher.Initialize (Mem_Dispatcher, Cap);
      if Memory.Initialized (Mem_Dispatcher) then
         Memory_Dispatcher.Register (Mem_Dispatcher);
      else
         if Gneiss.Log.Status (Log) = Gneiss.Initialized then
            Log_Client.Info (Log, "Error initializing memory session");
         end if;
         Main.Vacate (Capability, Main.Failure);
         return;
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
