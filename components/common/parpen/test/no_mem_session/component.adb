with Gneiss.Log.Client;
with Gneiss.Message.Client;

with Parpen.Generic_Types;
with Parpen.Protocol.Generic_Request;

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

   procedure Event;

   package Message is new Gneiss.Message (Message_Buffer, Null_Buffer);
   package Message_Client is new Message.Client (Event);

   Cap : Gneiss.Capability;
   Log : Gneiss.Log.Client_Session;
   Msg : Message.Client_Session;

   ---------------
   -- Construct --
   ---------------

   procedure Construct (Capability : Gneiss.Capability)
   is
      Msg_Buffer : Message_Buffer := (others => ASCII.NUL);
      -- FIXME: Generate label
      Label : constant String := ASCII.ESC & "prpn" & ASCII.NUL;
   begin
      Cap := Capability;
      Gneiss.Log.Client.Initialize (Log, Cap, "parpen_client");

      Message_Client.Initialize (Msg, Cap, Label);
      if not Message.Initialized (Msg) then
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;

      Msg_Buffer (1 .. 12) := "Hello World!";
      Message_Client.Write (Msg, Msg_Buffer);
   end Construct;

   -----------
   -- Event --
   -----------

   procedure Event
   is
      use type Parpen.Protocol.Tag;
      package Reply is new Parpen.Container (Types, Message_Buffer'Length);
      Context : Request_Package.Context := Request_Package.Create;
   begin
      if
         Gneiss.Log.Initialized (Log)
         and then Message.Initialized (Msg)
      then
         Main.Vacate (Cap, Main.Failure);
         while Message_Client.Available (Msg) loop
            Message_Client.Read (Msg, Reply.Ptr.all);
            Request_Package.Initialize (Context, Reply.Ptr);
            Request_Package.Verify_Message (Context);
            if not Request_Package.Valid_Message (Context) then
               Gneiss.Log.Client.Error (Log, "Invalid reply");
               return;
            end if;
            if Request_Package.Get_Tag (Context) = Parpen.Protocol.T_STATUS
            then
               Gneiss.Log.Client.Info (Log, "Error detected");
               Main.Vacate (Cap, Main.Success);
               return;
            end if;
         end loop;
      end if;
   end Event;

   --------------
   -- Destruct --
   --------------

   procedure Destruct
   is
   begin
      Gneiss.Log.Client.Finalize (Log);
   end Destruct;

end Component;
