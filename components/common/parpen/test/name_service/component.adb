with Gneiss.Log.Client;
with Gneiss.Message.Client;
with Gneiss.Memory.Client;

with Parpen.Generic_Types;
with Parpen.Protocol.Generic_Request;
with Parpen.Protocol.Generic_Transaction;
with Parpen.Protocol.Generic_Contains;

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

   package Request_Package is new Parpen.Protocol.Generic_Request (Types);
   package Transaction_Package is new Parpen.Protocol.Generic_Transaction (Types);
   package Contains_Package is new Parpen.Protocol.Generic_Contains (Types, Request_Package, Transaction_Package);

   procedure Event;

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);
   package Message_Client is new Message.Client (Event);

   package Memory is new Gneiss.Memory (Character, Positive, String);
   procedure Modify (Session : in out Memory.Client_Session;
                     Data    : in out String);
   package Memory_Client is new Memory.Client (Modify);

   package Scratch is new Parpen.Container (Types, 4096);
   Scratch_Len : Natural := 0;

   Cap : Gneiss.Capability;
   Log : Gneiss.Log.Client_Session;
   Msg : Message.Client_Session;
   Mem : Memory.Client_Session;

   package FSM is
      procedure Reset;
      procedure Next;
   end FSM;

   package body FSM is

      type State_Type is (Initial, Reply, Data, Final, Fail);
      State : State_Type := Initial;

      function "&" (Left : String; Right : Natural) return String is
         (Left & (1 => Character'Val (Right)));

      procedure Handle_Initial (State : in out State_Type)
      is
         package Request is new Parpen.Container (Types, Message_Buffer'Length);
         Request_Context : Request_Package.Context := Request_Package.Create;
         Transaction_Context: Transaction_Package.Context := Transaction_Package.Create;

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
            Gneiss.Log.Client.Info (Log, "Handle_Initial");
         end if;

         if not Message.Initialized (Msg)
         then
            State := Fail;
            return;
         end if;

         Scratch.Ptr.all (1 .. Add_Service'Length) := Add_Service;
         Scratch_Len := Add_Service'Length;
         Memory_Client.Modify (Mem);

         Request.Ptr.all := (others => ASCII.NUL);
         Request_Package.Initialize (Request_Context, Request.Ptr);
         Request_Package.Set_Tag (Request_Context, Parpen.Protocol.T_TRANSACTION);
         Contains_Package.Switch_To_Data (Request_Context, Transaction_Context);

         Transaction_Package.Set_Handle (Transaction_Context, 0);
         Transaction_Package.Set_Method (Transaction_Context, 3);
         Transaction_Package.Set_Cookie (Transaction_Context, 16#dead_beef#);
         Transaction_Package.Set_Send_Offset (Transaction_Context, 64);
         Transaction_Package.Set_Send_Length (Transaction_Context, Add_Service'Size - 64);
         Transaction_Package.Set_Meta_Offset (Transaction_Context, 0);
         Transaction_Package.Set_Meta_Length (Transaction_Context, 64);
         Transaction_Package.Set_Receive_Offset (Transaction_Context, 0);
         Transaction_Package.Set_Receive_Length (Transaction_Context, 0);

         if
            Gneiss.Log.Initialized (Log)
            and then not Transaction_Package.Valid_Message (Transaction_Context)
         then
            Gneiss.Log.Client.Error (Log, "Add transaction invalid");
            State := Fail;
            return;
         end if;

         Transaction_Package.Take_Buffer (Transaction_Context, Request.Ptr);
         Message_Client.Write (Msg, Request.Ptr.all);
         State := Reply;
      end Handle_Initial;

      procedure Parse_Reply (Valid : out Boolean)
      is
         package Reply is new Parpen.Container (Types, Message_Buffer'Length);
         Reply_Context : Request_Package.Context := Request_Package.Create;
         Status        : Parpen.Protocol.Status;
         use type Parpen.Protocol.Status;
      begin
         Valid := False;
         Message_Client.Read (Msg, Reply.Ptr.all);

         Request_Package.Initialize (Reply_Context, Reply.Ptr);
         Request_Package.Verify_Message (Reply_Context);
         if not Request_Package.Valid_Message (Reply_Context) then
            if Gneiss.Log.Initialized (Log) then
               Gneiss.Log.Client.Error (Log, "Invalid reply");
            end if;
            return;
         end if;

         case Request_Package.Get_Tag (Reply_Context) is
            when Parpen.Protocol.T_TRANSACTION =>
               if Gneiss.Log.Initialized (Log) then
                  Gneiss.Log.Client.Info (Log, "Reply transaction received");
               end if;
               Valid := True;
            when Parpen.Protocol.T_STATUS =>
               Status := Request_Package.Get_Code (Reply_Context);
               if Status = Parpen.Protocol.STATUS_OK then
                  Valid := True;
                  return;
               end if;

               if Gneiss.Log.Initialized (Log) then
                  Gneiss.Log.Client.Info
                     (Log, "ERROR: " &
                           (case Status is
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

      procedure Handle_Reply (State : in out State_Type)
      is
         package Request is new Parpen.Container (Types, Message_Buffer'Length);

         Request_Context     : Request_Package.Context := Request_Package.Create;
         Transaction_Context : Transaction_Package.Context := Transaction_Package.Create;
         Success             : Boolean;

         Get_Service : String := String'(
            ""
            & 16#00# & 16#00# & 16#00# & 16#04# -- Len
            & "Test"                            -- Name
            & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
            & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
            & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
            & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
            & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
            & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
            & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
         );

         use type Parpen.Protocol.Tag;
         use type Parpen.Protocol.Length;
      begin
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Handle_Reply");
         end if;

         Scratch.Ptr.all (1 .. Get_Service'Length) := Get_Service;
         Scratch_Len := Get_Service'Length;
         Memory_Client.Modify (Mem);

         Parse_Reply (Success);
         if not Success then
            State := Fail;
            return;
         end if;

         Request.Ptr.all := (others => ASCII.NUL);
         Request_Package.Initialize (Request_Context, Request.Ptr);
         Request_Package.Set_Tag (Request_Context, Parpen.Protocol.T_TRANSACTION);
         Contains_Package.Switch_To_Data (Request_Context, Transaction_Context);

         Transaction_Package.Set_Handle (Transaction_Context, 0);
         Transaction_Package.Set_Method (Transaction_Context, 1);
         Transaction_Package.Set_Cookie (Transaction_Context, 16#beef_dead_c0de#);
         Transaction_Package.Set_Send_Offset (Transaction_Context, 0);
         Transaction_Package.Set_Send_Length (Transaction_Context, Get_Service'Size - 64);
         Transaction_Package.Set_Meta_Offset (Transaction_Context, 0);
         Transaction_Package.Set_Meta_Length (Transaction_Context, 0);
         Transaction_Package.Set_Receive_Offset (Transaction_Context, 0);
         Transaction_Package.Set_Receive_Length (Transaction_Context, Get_Service'Size - 64);

         if
            Gneiss.Log.Initialized (Log)
            and then not Transaction_Package.Valid_Message (Transaction_Context)
         then
            Gneiss.Log.Client.Error (Log, "Query transaction invalid");
            State := Fail;
            return;
         end if;

         Transaction_Package.Take_Buffer (Transaction_Context, Request.Ptr);
         Message_Client.Write (Msg, Request.Ptr.all);
         State := Data;
      end Handle_Reply;

      procedure Handle_Data (State : in out State_Type)
      is
         Success : Boolean;
      begin
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Handle_Data");
         end if;

         Parse_Reply (Success);
         if not Success then
            State := Fail;
         end if;
      end Handle_Data;

      procedure Handle_Final (State : in out State_Type)
      is
         pragma Unreferenced (State);
      begin
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Handle_Final");
         end if;
         Main.Vacate (Cap, Main.Success);
      end Handle_Final;

      procedure Handle_Fail (State : in out State_Type)
      is
         pragma Unreferenced (State);
      begin
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Info (Log, "Handle_Final");
         end if;
         Main.Vacate (Cap, Main.Failure);
      end Handle_Fail;

      procedure Reset is
      begin
         State := Initial;
      end Reset;

      procedure Next is
      begin
         case State is
            when Initial => Handle_Initial (State);
            when Reply   => Handle_Reply (State);
            when Data    => Handle_Data (State);
            when Final   => Handle_Final (State);
            when Fail    => Handle_Fail (State);
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
      Gneiss.Log.Client.Initialize (Log, Cap, "name_service_test");

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
   begin
      if Gneiss.Log.Initialized (Log) then
         Gneiss.Log.Client.Info (Log, "Modify");
      end if;
      if Data'Length > Scratch.Ptr'Length then
         if Gneiss.Log.Initialized (Log) then
            Gneiss.Log.Client.Error (Log, "Scratch buffer overflow");
         end if;
         return;
      end if;
      Data (Data'First .. Data'First + (Scratch_Len - 1)) := Scratch.Ptr.all (1 .. Scratch_Len);
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
