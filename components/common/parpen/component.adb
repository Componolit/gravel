
with Gneiss.Log.Client;
with Gneiss.Message.Dispatcher;
with Gneiss.Message.Server;
with Gneiss.Memory.Server;
with Gneiss.Memory.Dispatcher;

with Parpen.Generic_Types;
with Parpen.Protocol.Generic_Label;
with Parpen.Protocol.Generic_Request;
with Parpen.Protocol.Generic_Transaction;
with Parpen.Protocol.Generic_Contains;
with Parpen.Container;

with Parpen.Message;

package body Component with
   SPARK_Mode
is
   Num_Clients  : constant := 10;
   Label_Length : constant := 128;

   subtype Client_ID is Gneiss.Session_Index range 1 .. Num_Clients;

   use type Gneiss.Session_Index;
   subtype External_Client_ID is Client_ID range Client_ID'First + 1 .. Client_ID'Last;

   subtype Message_Buffer is String (1 .. 128);
   Null_Buffer : constant Message_Buffer := (others => ASCII.NUL);

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);
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
   package Request_Package is new Parpen.Protocol.Generic_Request (Types);
   package Transaction_Package is new Parpen.Protocol.Generic_Transaction (Types);
   package Contains_Package is new Parpen.Protocol.Generic_Contains (Types, Request_Package, Transaction_Package);

   procedure Trace (Message : String);

   package Message_Package is new Parpen.Message (Types               => Types,
                                                  Client_ID           => Client_ID,
                                                  Trace               => Trace,
                                                  Num_Nodes           => 20,
                                                  Num_Handles         => 100,
                                                  Num_Name_DB_Entries => 5);

   type Server_State is (Uninitialized, SHM_Wait, Initialized, Error);
   type Server_Slot is record
      Len     : Natural := 0;
      Name    : String (1 .. 50) := (others => ASCII.NUL);
      State   : Server_State := Uninitialized;
      ID      : Parpen.Protocol.Connection_ID := Parpen.Protocol.Connection_ID'First;
   end record;

   package Scratch is new Parpen.Container (Types, 4096);

   type Server_Session is
   record
      Msg : Message.Server_Session;
      Mem : Memory.Server_Session;
   end record;

   type Server_Reg is array (External_Client_ID) of Server_Session;
   type Server_Meta is array (External_Client_ID) of Server_Slot;

   Cap            : Gneiss.Capability;
   Log            : Gneiss.Log.Client_Session;
   Msg_Dispatcher : Message.Dispatcher_Session;
   Mem_Dispatcher : Memory.Dispatcher_Session;

   Servers      : Server_Reg;
   Servers_Data : Server_Meta;

   type Direction is (Dir_Invalid, Dir_In, Dir_Out);
   type Operation (Direction : Component.Direction := Dir_Invalid) is
      record
         case Direction is
            when Dir_In | Dir_Out =>
               Start  : Positive;
               Length : Natural;
            when Dir_Invalid =>
               null;
         end case;
      end record;
   Modify_Operation : Component.Operation := (Direction => Dir_Invalid);

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

   procedure Dump (Data : String)
   is
      function Digit (Value : Natural) return Character;
      function Digit (Value : Natural) return Character
      is
         V : constant Natural := Value mod 16;
      begin
         if V <= 9 then
            return Character'Val (V + Character'Pos ('0'));
         elsif V <= 16 then
            return Character'Val (V + Character'Pos ('A') - 16#A#);
         end if;
         return '*';
      end Digit;

      function Hex (Value : Character) return String;
      function Hex (Value : Character) return String
      is
         Val : constant Natural := Natural (Character'Pos (Value));
      begin
         return Digit (Val / 16) & Digit (Val mod 16);
      end Hex;

      Pos : Natural := 1;
      Buffer : String (1 .. 3 * Data'Length) := (others => ' ');
   begin
      for D of Data loop
         Buffer (Pos .. Pos + 1) := Hex (D);
         Pos := Pos + 3;
      end loop;
      Gneiss.Log.Client.Info (Log, Buffer);
   end Dump;

   -----------------
   -- Send_Status --
   -----------------

   procedure Send_Status (ID   : Client_ID;
                          Code : Parpen.Protocol.Status)
   is
      Request_Context : Request_Package.Context := Request_Package.Create;
      package Reply is new Parpen.Container (Types, Message_Buffer'Length);
   begin
      Request_Package.Initialize (Request_Context, Reply.Ptr);
      Request_Package.Set_Tag (Request_Context, Parpen.Protocol.T_STATUS);
      Request_Package.Set_Code (Request_Context, Code);
      Request_Package.Take_Buffer (Request_Context, Reply.Ptr);
      Message_Server.Send (Servers (ID).Msg, Reply.Ptr.all);
   end Send_Status;

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
      Request_Context     : Request_Package.Context := Request_Package.Create;
      Transaction_Context : Transaction_Package.Context := Transaction_Package.Create;
      Transaction         : Message_Package.Transaction;
      Status              : Message_Package.Status;
      use type Parpen.Protocol.Tag;

      procedure Send (ID         : Client_ID;
                      Handle     : Parpen.Protocol.Handle;
                      Method     : Parpen.Protocol.Method;
                      Cookie     : Parpen.Protocol.Cookie;
                      Data       : String_Ptr;
                      Data_First : Positive;
                      Recv_First : Positive;
                      Length     : Natural)
      is
         Request_Context : Request_Package.Context := Request_Package.Create;
         Transaction_Context: Transaction_Package.Context := Transaction_Package.Create;
         package Reply is new Parpen.Container (Types, Message_Buffer'Length);
      begin
         Trace ("Send: called");
         if Length = 0 then
            Trace ("Send: No send phase");
            Send_Status (ID   => ID,
                         Code => Parpen.Protocol.STATUS_OK);
            return;
         end if;

         Scratch.Ptr (1 .. Length) := Data (Data_First .. Data_First + Length - 1);
         Modify_Operation := (Direction => Dir_Out,
                              Start     => Recv_First,
                              Length    => Length);
         Memory_Server.Modify (Servers (ID).Mem);

         Request_Package.Initialize (Request_Context, Reply.Ptr);
         Request_Package.Set_Tag (Request_Context, Parpen.Protocol.T_TRANSACTION);
         Contains_Package.Switch_To_Data (Request_Context, Transaction_Context);

         Transaction_Package.Set_Handle (Transaction_Context, Handle);
         Transaction_Package.Set_Method (Transaction_Context, Method);
         Transaction_Package.Set_Cookie (Transaction_Context, Cookie);
         Transaction_Package.Set_Send_Offset (Transaction_Context, Parpen.Protocol.Offset (8 * Recv_First));
         Transaction_Package.Set_Send_Length (Transaction_Context, Parpen.Protocol.Length (8 * Length));
         Transaction_Package.Set_Meta_Offset (Transaction_Context, 0);
         Transaction_Package.Set_Meta_Length (Transaction_Context, 0);
         Transaction_Package.Set_Receive_Offset (Transaction_Context, 0);
         Transaction_Package.Set_Receive_Length (Transaction_Context, 0);

         Transaction_Package.Take_Buffer (Transaction_Context, Reply.Ptr);
         Message_Server.Send (Servers (ID).Msg, Reply.Ptr.all);
      end Send;

      procedure Dispatch is new Message_Package.Dispatch (Send);

      use type Message_Package.Status;
   begin
      Trace ("Handle_Message: Message received");
      Request.Ptr.all := (others => ASCII.NUL);
      Request.Ptr.all (1 .. Data'Length) := Data;
      Request_Package.Initialize (Request_Context, Request.Ptr);
      Request_Package.Verify_Message (Request_Context);
      if Request_Package.Structural_Valid_Message (Request_Context) then
         if Request_Package.Get_Tag (Request_Context) = Parpen.Protocol.T_TRANSACTION then
            if Contains_Package.Transaction_In_Request_Data (Request_Context) then
               Contains_Package.Switch_To_Data (Request_Context, Transaction_Context);
               Transaction_Package.Verify_Message (Transaction_Context);
               if Transaction_Package.Valid_Message (Transaction_Context) then
                  Trace ("Handle_Message: Unpacking transaction");
                  Transaction :=
                     (Handle         => Transaction_Package.Get_Handle (Transaction_Context),
                      Method         => Transaction_Package.Get_Method (Transaction_Context),
                      Cookie         => Transaction_Package.Get_Cookie (Transaction_Context),
                      Send_Offset    => Bit_Length (Transaction_Package.Get_Send_Offset (Transaction_Context)),
                      Send_Length    => Bit_Length (Transaction_Package.Get_Send_Length (Transaction_Context)),
                      Offsets_Offset => Bit_Length (Transaction_Package.Get_Meta_Offset (Transaction_Context)),
                      Offsets_Length => Bit_Length (Transaction_Package.Get_Meta_Length (Transaction_Context)),
                      Recv_Offset    => Bit_Length (Transaction_Package.Get_Receive_Offset (Transaction_Context)),
                      Recv_Length    => Bit_Length (Transaction_Package.Get_Receive_Length (Transaction_Context)));

                  Trace ("Handle_Message: Copy data to scratch buffer");
                  Modify_Operation := (Direction => Dir_In,
                                       Start     => Positive (1 + Transaction.Send_Offset / 8),
                                       Length    => Natural (Transaction.Send_Length / 8));
                  Memory_Server.Modify (Servers (Message.Index (Session).Value).Mem);

                  Trace ("Handle_Message: Dispatching transaction");
                  Dispatch (Sender      => Message.Index (Session).Value,
                            Transaction => Transaction,
                            Data        => Scratch.Ptr,
                            Status      => Status);

                  if Status /= Message_Package.Status_Valid then
                     Trace ("ERROR: Handle_Message: Sending error");
                     Send_Status (ID   => Message.Index (Session).Value,
                                  Code => (case Status is
                                           when Message_Package.Status_Invalid_Handle
                                              => Parpen.Protocol.STATUS_INVALID_HANDLE,
                                           when Message_Package.Status_Invalid_Method
                                              => Parpen.Protocol.STATUS_INVALID_METHOD,
                                           when Message_Package.Status_Invalid_Binder
                                              => Parpen.Protocol.STATUS_INVALID_BINDER,
                                           when Message_Package.Status_Offset_Out_Of_Range
                                              => Parpen.Protocol.STATUS_OFFSET_OUT_OF_RANGE,
                                           when Message_Package.Status_Receiver_Not_Ready
                                              => Parpen.Protocol.STATUS_RECEIVER_NOT_READY,
                                           when Message_Package.Status_Receive_Buffer_Too_Small
                                              => Parpen.Protocol.STATUS_RECEIVE_BUFFER_TOO_SMALL,
                                           when Message_Package.Status_Overflow
                                              => Parpen.Protocol.STATUS_OVERFLOW,
                                           when others
                                              => Parpen.Protocol.STATUS_UNKNOWN_ERROR));
                  end if;
                  return;
               end if;
            end if;
         end if;
      end if;

      Trace ("ERROR: Handle_Message: Invalid request");
      Send_Status (ID   => Message.Index (Session).Value,
                   Code => Parpen.Protocol.STATUS_INVALID_REQUEST);
   end Handle_Message;

   ----------
   -- Recv --
   ----------

   procedure Recv (Session : in out Message.Server_Session;
                   Data    :        Message_Buffer)
   is
      Idx   : constant Gneiss.Session_Index := Message.Index (Session).Value;
      package Reply is new Parpen.Container (Types, Message_Buffer'Length);
      Context : Request_Package.Context := Request_Package.Create;
   begin
      Request_Package.Initialize (Context, Reply.Ptr);
      if Gneiss.Log.Initialized (Log) then
         if Idx in Servers'Range then
            case Servers_Data (Idx).State is
               when Uninitialized | Error =>
                  Trace ("Recv: Internal error: " & Data);
               when SHM_Wait =>
                  Trace ("Recv: SHM not initialized");
                  Send_Status (ID   => Message.Index (Session).Value,
                               Code => Parpen.Protocol.STATUS_PROTOCOL_VIOLATION);
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
      pragma Unreferenced (Session);
   begin
      if Data'Length > Scratch.Ptr'Length then
         Trace ("Data overflows scratch buffer");
         Data := (others => ASCII.NUL);
         --  FIXME: How do we signal an error?
         return;
      end if;
      case Modify_Operation.Direction is
         when Dir_In =>
            Trace ("Modify: Copying shared memory to scratch area");
            Scratch.Ptr.all (Scratch.Ptr'First .. Scratch.Ptr'First + Data'Length - 1) := Data;
         when Dir_Out =>
            Trace ("Modify: Copying scratch area to shared memory");
            Data := Scratch.Ptr.all (Scratch.Ptr'First .. Scratch.Ptr'First + Data'Length - 1);
         when Dir_Invalid =>
            Trace ("Modify: Error - called with invalid operation");
      end case;
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
      Status  : Message_Package.Status;
      Context : Label_Package.Context := Label_Package.Create;
      package L is new Parpen.Container (Types, Label_Length);
      use type Message_Package.Status;
   begin
      if L.Ptr.all'Length < Label'Length then
         Trace ("Label too long");
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
                  Trace ("Dispatch: Adding client");
                  Message_Package.Add_Client (ID => I, Status => Status);
                  if Status /= Message_Package.Status_Valid then
                     Trace ("Dispatch: Error registering client");
                     exit;
                  end if;
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
         Trace ("Dispatch: " & Name & " sent invalid label");
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
         Trace ("Dispatch: " & Name & " sent invalid label");
      end if;

      if not Done then
         Trace ("Dispatch: Error no matching message session found for " & Name & ":" & Label);
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
      Status : Message_Package.Status;
      use type Message_Package.Status;
   begin
      Cap := Capability;
      Gneiss.Log.Client.Initialize (Log, Cap, "parpen");

      Message_Package.Initialize (Name_Service_ID => Client_ID'First,
                                  Status          => Status);
      if Status /= Message_Package.Status_Valid then
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Error (Log, "Error initializing messaging");
         end if;
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;

      Message_Dispatcher.Initialize (Msg_Dispatcher, Cap);
      if Message.Initialized (Msg_Dispatcher) then
         Message_Dispatcher.Register (Msg_Dispatcher);
      else
         Trace("Construct: Error initializing message session");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;

      Memory_Dispatcher.Initialize (Mem_Dispatcher, Cap);
      if Memory.Initialized (Mem_Dispatcher) then
         Memory_Dispatcher.Register (Mem_Dispatcher);
      else
         Trace ("Construct: Error initializing memory session");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
   end Construct;

   --------------
   -- Destruct --
   --------------

   procedure Destruct
   is
   begin
      Trace ("Destruct: Destructing");
      Gneiss.Log.Client.Finalize (Log);
   end Destruct;

   -----------
   -- Trace --
   -----------

   procedure Trace (Message : String)
   is
   begin
      if Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Info (Log, Message);
      end if;
   end Trace;

end Component;
