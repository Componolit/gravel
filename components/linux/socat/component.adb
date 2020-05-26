
with Gneiss.Packet;
with Gneiss.Packet.Dispatcher;
with Gneiss.Packet.Server;
with Gneiss.Rom;
with Gneiss.Rom.Client;
with Gneiss.Log;
with Gneiss.Log.Client;
with SXML;
with SXML.Parser;
with SXML.Query;

package body Component with
   SPARK_Mode
is

   subtype Desc_Index is Positive range 1 .. 10;

   package Log is new Gneiss.Log;
   package Rom is new Gneiss.Rom (Character, Positive, String);
   package Packet is new Gneiss.Packet (Positive, Character, String, Desc_Index);

   type Descriptors is array (Desc_Index'Range) of Packet.Descriptor;

   type Server_Slot is record
      Ready : Boolean := False;
   end record;

   subtype Server_Index is Gneiss.Session_Index range 1 .. 2;
   type Server_Reg is array (Server_Index'Range) of Packet.Server_Session;
   type Server_Slots is array (Server_Index'Range) of Server_Slot;

   type Server_Meta is record
      Slots       : Server_Slots;
      Destination : String (1 .. 1024) := (others => Character'First);
      Last        : Natural := 0;
   end record;

   function Rom_Contract (S : Server_Meta) return Boolean is (True);

   procedure Update (Session : in out Packet.Server_Session;
                     Idx     :        Desc_Index;
                     Buf     :    out String;
                     Ctx     : in out Server_Meta);

   procedure Read (Session : in out Packet.Server_Session;
                   Idx     :        Desc_Index;
                   Buf     :        String;
                   Ctx     : in out Server_Meta);

   procedure Configure (Session : in out Rom.Client_Session;
                        Data    :        String;
                        Ctx     : in out Server_Meta) with
      Pre    => Rom.Initialized (Session),
      Post   => Rom.Initialized (Session),
      Global => null;

   procedure Initialize (Session : in out Packet.Server_Session;
                         Context : in out Server_Meta) with
      Pre    => Packet.Initialized (Session),
      Post   => Packet.Initialized (Session),
      Global => null;

   procedure Finalize (Session : in out Packet.Server_Session;
                       Context : in out Server_Meta) with
      Pre    => Packet.Initialized (Session),
      Post   => Packet.Initialized (Session),
      Global => null;

   procedure Event;

   function Ready (Session : Packet.Server_Session;
                   Context : Server_Meta) return Boolean with
      Global => null;

   procedure Dispatch (Session  : in out Packet.Dispatcher_Session;
                       Disp_Cap :        Packet.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String) with
      Pre    => Packet.Initialized (Session)
                and then Packet.Registered (Session),
      Post   => Packet.Initialized (Session)
                and then Packet.Registered (Session);

   package Packet_Server is new Packet.Server (Server_Meta, Initialize, Finalize, Event, Ready, Update, Read);
   package Packet_Dispatcher is new Packet.Dispatcher (Packet_Server, Dispatch);
   package Rom_Client is new Rom.Client (Server_Meta, Configure);
   package Log_Client is new Log.Client;

   procedure Configure is new Rom_Client.Update (Rom_Contract);

   Dispatcher  : Packet.Dispatcher_Session;
   Capability  : Gneiss.Capability;
   Servers     : Server_Reg;
   Server_Data : Server_Meta;
   Descs       : Descriptors;
   Config      : Rom.Client_Session;
   Logger      : Log.Client_Session;

   procedure Construct (Cap : Gneiss.Capability)
   is
   begin
      Capability := Cap;
      Log_Client.Initialize (Logger, Cap, "socat");
      if not Log.Initialized (Logger) then
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;
      Packet_Dispatcher.Initialize (Dispatcher, Cap);
      if not Packet.Initialized (Dispatcher) then
         Log_Client.Error (Logger, "Failed to initialize Packet dispatcher.");
         Main.Vacate (Cap, Main.Failure);
         return;
      end if;
      Rom_Client.Initialize (Config, Cap, "socat_config");
      if not Rom.Initialized (Config) then
         Log_Client.Error (Logger, "Failed to initialize Rom client.");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Configure (Config, Server_Data);
      if Server_Data.Last < Server_Data.Destination'First then
         Log_Client.Error (Logger, "Failed to parse arguments.");
         Main.Vacate (Capability, Main.Failure);
         return;
      end if;
      Log_Client.Info (Logger, Server_Data.Destination (Server_Data.Destination'First .. Server_Data.Last));
   end Construct;

   procedure Event
   is
   begin
      null;
   end Event;

   procedure Update (Session : in out Packet.Server_Session;
                     Idx     :        Desc_Index;
                     Buf     :    out String;
                     Ctx     : in out Server_Meta)
   is
   begin
      null;
   end Update;

   procedure Read (Session : in out Packet.Server_Session;
                   Idx     :        Desc_Index;
                   Buf     :        String;
                   Ctx     : in out Server_Meta)
   is
   begin
      null;
   end Read;

   procedure Destruct
   is
   begin
      null;
   end Destruct;

   procedure Initialize (Session : in out Packet.Server_Session;
                         Context : in out Server_Meta)
   is
   begin
      if Packet.Index (Session).Value in Context.Slots'Range then
         Context.Slots (Packet.Index (Session).Value).Ready := True;
      end if;
   end Initialize;

   procedure Finalize (Session : in out Packet.Server_Session;
                       Context : in out Server_Meta)
   is
   begin
      if Packet.Index (Session).Value in Context.Slots'Range then
         Context.Slots (Packet.Index (Session).Value).Ready := False;
      end if;
   end Finalize;

   procedure Dispatch (Session  : in out Packet.Dispatcher_Session;
                       Disp_Cap :        Packet.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String)
   is
      pragma Unreferenced (Name);
      pragma Unreferenced (Label);
   begin
      if Packet_Dispatcher.Valid_Session_Request (Session, Disp_Cap) then
         for I in Servers'Range loop
            if not Ready (Servers (I), Server_Data) and then not Packet.Initialized (Servers (I)) then
               Packet_Dispatcher.Session_Initialize (Session, Disp_Cap, Servers (I), Server_Data, I);
               if Ready (Servers (I), Server_Data) and then Packet.Initialized (Servers (I)) then
                  Packet_Dispatcher.Session_Accept (Session, Disp_Cap, Servers (I), Server_Data);
                  exit;
               end if;
            end if;
         end loop;
      end if;
      for S of Servers loop
         Packet_Dispatcher.Session_Cleanup (Session, Disp_Cap, S, Server_Data);
      end loop;
   end Dispatch;

   function Ready (Session : Packet.Server_Session;
                   Context : Server_Meta) return Boolean is
      (if
          Packet.Index (Session).Valid
          and then Packet.Index (Session).Value in Context.Slots'Range
       then Context.Slots (Packet.Index (Session).Value).Ready
       else False);

   procedure Configure (Session : in out Rom.Client_Session;
                        Data    :        String;
                        Ctx     : in out Server_Meta)
   is
      pragma Unreferenced (Session);
      use type SXML.Result_Type;
      use type SXML.Parser.Match_Type;
      Document   : SXML.Document_Type (1 .. 128) := (others => SXML.Null_Node);
      Success    : Boolean;
      State      : SXML.Query.State_Type;
      Result     : SXML.Result_Type;
      Match      : SXML.Parser.Match_Type;
      Ignore_Pos : Natural;
   begin
      if not SXML.Valid_Content (Data'First, Data'Last) then
         return;
      end if;
      SXML.Parser.Parse (Data, Document, Ignore_Pos, Match);
      if Match /= SXML.Parser.Match_OK then
         return;
      end if;
      State := SXML.Query.Init (Document);
      if not SXML.Query.Is_Open (State, Document) then
         return;
      end if;
      State := SXML.Query.Path (State, Document, "/socat");
      if
         SXML.Query.State_Result (State) /= SXML.Result_Ok
         or else not SXML.Query.Is_Open (State, Document)
      then
         return;
      end if;
      SXML.Query.Attribute (State, Document, "arg", Result, Ctx.Destination, Ctx.Last);
      if Result /= SXML.Result_OK then
         Ctx.Last := 0;
      end if;
   end Configure;

end Component;
