with Gneiss.Log.Client;
with Gneiss.Message.Client;
with Gneiss.Memory.Client;

with Parpen.Generic_Types;
with Parpen.Protocol.Generic_Reply;

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

   package Reply_Package is new Parpen.Protocol.Generic_Reply (Types);

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
      Label : constant String := ASCII.ESC & "prpn" & ASCII.NUL;
      use type Gneiss.Session_Status;
   begin
      Cap := Capability;
      Log_Client.Initialize (Log, Cap, "parpen_client");

      Message_Client.Initialize (Msg, Cap, Label);
      if Message.Status (Msg) /= Gneiss.Initialized then
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;

      Memory_Client.Initialize (Mem, Cap, Label, 4096);
      if Memory.Status (Mem) /= Gneiss.Initialized then
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;

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
      use type Parpen.Protocol.Reply_Tag;
      package Reply is new Parpen.Container (Types, Message_Buffer'Length);
      Context : Reply_Package.Context := Reply_Package.Create;
   begin
      if
         Gneiss.Log.Status (Log) = Gneiss.Initialized
         and then Message.Status (Msg) = Gneiss.Initialized
      then
         Main.Vacate (Cap, Main.Failure);
         while Message_Client.Available (Msg) loop
            Message_Client.Read (Msg, Reply.Ptr.all);
            Reply_Package.Initialize (Context, Reply.Ptr);
            Reply_Package.Verify_Message (Context);
            if not Reply_Package.Valid_Message (Context) then
               Log_Client.Error (Log, "Invalid reply");
               return;
            end if;
            if Reply_Package.Get_Tag (Context) = Parpen.Protocol.REPLY_ERROR
            then
               Log_Client.Info (Log, "Error detected");
               Main.Vacate (Cap, Main.Success);
               return;
            end if;
         end loop;
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
      Log_Client.Finalize (Log);
   end Destruct;

end Component;
