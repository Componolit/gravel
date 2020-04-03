with Gneiss.Log.Client;
with Gneiss.Message.Client;
with Gneiss.Memory.Client;

with Parpen.Generic_Types;
with Parpen.Protocol.Generic_Transaction;

with Parpen.Container;

package body Component with
   SPARK_Mode
is

   subtype Message_Buffer is String (1 .. 128);
   Null_Buffer : constant Message_Buffer := (others => ASCII.NUL);
   type String_Ptr is access all String;
   type Bit_Length is range 0 .. Natural'Last * 8;

   package Types is new Parpen.Generic_Types (Index      => Positive,
                                              Byte       => Character,
                                              Bytes      => String,
                                              Bytes_Ptr  => String_Ptr,
                                              Length     => Natural,
                                              Bit_Length => Bit_Length);

   package Transaction_Package is new Parpen.Protocol.Generic_Transaction (Types);

   procedure Event;

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);
   package Message_Client is new Message.Client (Event);

   package Memory is new Gneiss.Memory (Character, Positive, String);
   procedure Modify (Session : in out Memory.Client_Session;
                     Data    : in out String);
   package Memory_Client is new Memory.Client (Modify);

   package Scratch is new Parpen.Container (Types, 4096);

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

   Cap : Gneiss.Capability;
   Log : Gneiss.Log.Client_Session;
   Msg : Message.Client_Session;
   Mem : Memory.Client_Session;

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

   package FSM is
      procedure Reset;
      procedure Next;
   end FSM;

   package body FSM is

      type State_Type is (Register, NS_Reply, Pong);
      State : State_Type := Register;

      type Reply_Kind is (R_Invalid, R_Status, R_Binder, R_Handle);
      type Reply_Type (Kind : Reply_Kind := R_Invalid) is record
         case Kind is
            when R_Invalid =>
               null;
            when R_Status =>
               Status : Parpen.Protocol.Status;
            when R_Handle =>
               Handle        : Parpen.Protocol.Handle;
               H_Method      : Parpen.Protocol.Method;
               H_Cookie      : Parpen.Protocol.Cookie;
               H_Send_Offset : Parpen.Protocol.Offset;
               H_Send_Length : Parpen.Protocol.Length;
               H_Meta_Offset : Parpen.Protocol.Offset;
               H_Meta_Length : Parpen.Protocol.Length;
               H_Recv_Offset : Parpen.Protocol.Offset;
               H_Recv_Length : Parpen.Protocol.Length;
            when R_Binder =>
               Binder        : Parpen.Protocol.Binder;
               B_Cookie      : Parpen.Protocol.Cookie;
               B_Send_Offset : Parpen.Protocol.Offset;
               B_Send_Length : Parpen.Protocol.Length;
               B_Meta_Offset : Parpen.Protocol.Offset;
               B_Meta_Length : Parpen.Protocol.Length;
               B_Recv_Offset : Parpen.Protocol.Offset;
               B_Recv_Length : Parpen.Protocol.Length;
         end case;
      end record;

      function "&" (Left : String; Right : Natural) return String is
         (Left & (1 => Character'Val (Right)));

      procedure Handle_Register (State : in out State_Type)
      is
         package Request is new Parpen.Container (Types, Message_Buffer'Length);
         Context : Transaction_Package.Context := Transaction_Package.Create;

         Add_Service : constant String := String'(
            ""
            & 16#00# & 16#00# & 16#00# & 16#00#
            & 16#00# & 16#00# & 16#00# & 16#40# -- Offset list with single entry (16#40# -> 64 bit offset within data)
            & 16#00# & 16#00# & 16#00# & 16#04# -- Len
            & "Test"                            -- Name
            & "wb*" & 16#85#                    -- Weak binder
            & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
            & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
            & 16#00# & 16#00# & 16#00# & 16#01# --
            & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
            & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
            & 16#00#                            -- Allow_Isolated: False
            & 16#00# & 16#00# & 16#00# & 16#00# -- Dump_Flags: 0
         );

         use type Parpen.Protocol.Length;
      begin
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Handle_Register");
         end if;

         if not Message.Initialized (Msg)
         then
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;

         Scratch.Ptr.all (1 .. Add_Service'Length) := Add_Service;
         Modify_Operation := (Direction => Dir_Out,
                              Start     => 1,
                              Length    => Add_Service'Length);
         Memory_Client.Modify (Mem);

         Request.Ptr.all := (others => ASCII.NUL);
         Transaction_Package.Initialize (Context, Request.Ptr);
         Transaction_Package.Set_Tag (Context, Parpen.Protocol.T_HANDLE);
         Transaction_Package.Set_Handle (Context, 0);
         Transaction_Package.Set_Method (Context, 3);
         Transaction_Package.Set_Cookie (Context, 16#dead_beef#);
         Transaction_Package.Set_Send_Offset (Context, 64);
         Transaction_Package.Set_Send_Length (Context, Add_Service'Size - 64);
         Transaction_Package.Set_Meta_Offset (Context, 0);
         Transaction_Package.Set_Meta_Length (Context, 64);
         Transaction_Package.Set_Recv_Offset (Context, 0);
         Transaction_Package.Set_Recv_Length (Context, Scratch.Ptr.all'Length);

         if
            Gneiss.Log.Initialized (Log)
            and then not Transaction_Package.Valid_Message (Context)
         then
            Gneiss.Log.Client.Error (Log, "Add transaction invalid");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;

         Transaction_Package.Take_Buffer (Context, Request.Ptr);
         Message_Client.Write (Msg, Request.Ptr.all);
         State := NS_Reply;
      end Handle_Register;

      procedure Parse_Reply (Reply : out Reply_Type)
      is
         package Reply_Buffer is new Parpen.Container (Types, Message_Buffer'Length);
         Context : Transaction_Package.Context := Transaction_Package.Create;
         use type Parpen.Protocol.Status;
      begin
         Gneiss.Log.Client.Info (Log, "Parse_Reply called");
         Message_Client.Read (Msg, Reply_Buffer.Ptr.all);

         Transaction_Package.Initialize (Context, Reply_Buffer.Ptr);
         Transaction_Package.Verify_Message (Context);
         if not Transaction_Package.Valid_Message (Context) then
            if Gneiss.Log.Initialized (Log) then
               Gneiss.Log.Client.Error (Log, "Invalid reply");
            end if;
            Reply := (Kind => R_Invalid);
            return;
         end if;

         case Transaction_Package.Get_Tag (Context) is
            when Parpen.Protocol.T_BINDER =>
               if Gneiss.Log.Initialized (Log) then
                  Gneiss.Log.Client.Info (Log, "Reply binder transaction received");
               end if;
               Reply := (Kind          => R_Binder,
                         Binder        => Transaction_Package.Get_Binder (Context),
                         B_Cookie      => Transaction_Package.Get_Cookie (Context),
                         B_Send_Offset => Transaction_Package.Get_Send_Offset (Context),
                         B_Send_Length => Transaction_Package.Get_Send_Length (Context),
                         B_Meta_Offset => Transaction_Package.Get_Meta_Offset (Context),
                         B_Meta_Length => Transaction_Package.Get_Meta_Length (Context),
                         B_Recv_Offset => Transaction_Package.Get_Recv_Offset (Context),
                         B_Recv_Length => Transaction_Package.Get_Recv_Length (Context));

            when Parpen.Protocol.T_HANDLE =>
               if Gneiss.Log.Initialized (Log) then
                  Gneiss.Log.Client.Info (Log, "Reply handle transaction received");
               end if;
               Reply := (Kind          => R_Handle,
                         Handle        => Transaction_Package.Get_Handle (Context),
                         H_Method      => Transaction_Package.Get_Method (Context),
                         H_Cookie      => Transaction_Package.Get_Cookie (Context),
                         H_Send_Offset => Transaction_Package.Get_Send_Offset (Context),
                         H_Send_Length => Transaction_Package.Get_Send_Length (Context),
                         H_Meta_Offset => Transaction_Package.Get_Meta_Offset (Context),
                         H_Meta_Length => Transaction_Package.Get_Meta_Length (Context),
                         H_Recv_Offset => Transaction_Package.Get_Recv_Offset (Context),
                         H_Recv_Length => Transaction_Package.Get_Recv_Length (Context));
               return;

            when Parpen.Protocol.T_STATUS =>
               Reply := (Kind   => R_Status,
                         Status => Transaction_Package.Get_Code (Context));
               if Reply.Status = Parpen.Protocol.STATUS_OK then
                  return;
               end if;

               if Gneiss.Log.Initialized (Log) then
                  Gneiss.Log.Client.Info
                     (Log, "ERROR: " &
                           (case Reply.Status is
                            when Parpen.Protocol.STATUS_OK                       => "OK (no error)",
                            when Parpen.Protocol.STATUS_UNKNOWN_ERROR            => "Unknown",
                            when Parpen.Protocol.STATUS_PROTOCOL_VIOLATION       => "Protocol violation",
                            when Parpen.Protocol.STATUS_INVALID_REQUEST          => "Invalid request",
                            when Parpen.Protocol.STATUS_INVALID_HANDLE           => "Invalid handle",
                            when Parpen.Protocol.STATUS_INVALID_METHOD           => "Invalid Method",
                            when Parpen.Protocol.STATUS_INVALID_BINDER           => "Invalid Binder",
                            when Parpen.Protocol.STATUS_OFFSET_OUT_OF_RANGE      => "Offset out of range",
                            when Parpen.Protocol.STATUS_RECEIVER_NOT_READY       => "Receiver not ready",
                            when Parpen.Protocol.STATUS_RECEIVE_BUFFER_TOO_SMALL => "Receive buffer too small",
                            when Parpen.Protocol.STATUS_Overflow                 => "Overflow"));
               end if;
               return;
         end case;
      end Parse_Reply;

      procedure Handle_NS_Reply (State : in out State_Type)
      is
         Reply : Reply_Type;
         use type Parpen.Protocol.Status;
      begin
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Handle_NS_Reply");
         end if;

         Parse_Reply (Reply);
         if Reply.Kind /= R_Status or else Reply.Status /= Parpen.Protocol.STATUS_OK then
            Trace ("Expected OK status");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;

         State := Pong;

      end Handle_NS_Reply;

      procedure Handle_Pong (State : in out State_Type)
      is
         package Request is new Parpen.Container (Types, Message_Buffer'Length);
         Context : Transaction_Package.Context := Transaction_Package.Create;
         Reply   : Reply_Type;
         use type Parpen.Protocol.Binder;
      begin
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Handle_Pong");
         end if;

         Parse_Reply (Reply);
         if Reply.Kind /= R_Binder then
            Trace ("Expected binder, got " & (case Reply.Kind is when R_Handle  => "Handle",
                                                                 when R_Binder  => "Binder",
                                                                 when R_Status  => "Status",
                                                                 when R_Invalid => "Invalid"));
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;

         if Reply.Binder /= 16#100000000000001# then
            Trace ("Unexpected binder");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;

         Modify_Operation := (Direction => Dir_In,
                              Start     => 1,
                              Length    => 1);
         Memory_Client.Modify (Mem);

         Scratch.Ptr.all (1) := Character'Val (Character'Pos (Scratch.Ptr.all (1)) + 1);
         Modify_Operation := (Direction => Dir_Out,
                              Start     => 1,
                              Length    => 1);
         Memory_Client.Modify (Mem);

         Request.Ptr.all := (others => ASCII.NUL);
         Transaction_Package.Initialize (Context, Request.Ptr);
         Transaction_Package.Set_Tag (Context, Parpen.Protocol.T_HANDLE);
         --  FIXME: Handle replies specially
         Transaction_Package.Set_Handle (Context, 42);
         Transaction_Package.Set_Method (Context, 1);
         Transaction_Package.Set_Cookie (Context, Reply.B_Cookie);
         Transaction_Package.Set_Send_Offset (Context, 0);
         Transaction_Package.Set_Send_Length (Context, 8);
         Transaction_Package.Set_Meta_Offset (Context, 0);
         Transaction_Package.Set_Meta_Length (Context, 0);
         Transaction_Package.Set_Recv_Offset (Context, 0);
         Transaction_Package.Set_Recv_Length (Context, Scratch.Ptr.all'Length);

         if
            Gneiss.Log.Initialized (Log)
            and then not Transaction_Package.Valid_Message (Context)
         then
            Gneiss.Log.Client.Error (Log, "Query transaction invalid");
            Main.Vacate (Cap, Main.Failure);
            return;
         end if;

         Transaction_Package.Take_Buffer (Context, Request.Ptr);
         Message_Client.Write (Msg, Request.Ptr.all);
      end Handle_Pong;

      procedure Reset is
      begin
         State := Register;
      end Reset;

      procedure Next is
      begin
         case State is
            when Register => Handle_Register (State);
            when NS_Reply => Handle_NS_Reply (State);
            when Pong     => Handle_Pong (State);
         end case;
      end Next;

   end FSM;

   ---------------
   -- Construct --
   ---------------

   procedure Construct (Capability : Gneiss.Capability)
   is
      -- FIXME: Generate label
      Label : constant String := ASCII.ESC & "prpn" & ASCII.NUL;
   begin
      Cap := Capability;
      Gneiss.Log.Client.Initialize (Log, Cap, "pong");

      Message_Client.Initialize (Msg, Cap, Label);
      if not Message.Initialized (Msg) then
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Error initializing message session");
         end if;
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;

      Memory_Client.Initialize (Mem, Cap, Label, 4096);
      if not Memory.Initialized (Mem) then
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Error initializing memory session");
         end if;
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;

      Gneiss.Log.Client.Info (Log, "Initialized");
      FSM.Reset;
      FSM.Next;
   end Construct;

   -----------
   -- Event --
   -----------

   procedure Event
   is
   begin
      FSM.Next;
   end Event;

   ------------
   -- Modify --
   ------------

   procedure Modify (Session : in out Memory.Client_Session;
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


   --------------
   -- Destruct --
   --------------

   procedure Destruct
   is
   begin
      if Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Info (Log, "Destructing...");
      end if;
      Gneiss.Log.Client.Finalize (Log);
   end Destruct;

end Component;
