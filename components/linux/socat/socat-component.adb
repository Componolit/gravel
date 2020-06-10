
with Basalt.Strings;
with Gneiss.Packet;
with Gneiss.Packet.Dispatcher;
with Gneiss.Packet.Server;
with Gneiss.Packet.Server.Linux;
with Gneiss.Rom;
with Gneiss.Rom.Client;
with Gneiss.Log;
with Gneiss.Log.Client;
with Gneiss_Internal;
with Gneiss_Internal.Syscall;
with SXML;
with SXML.Parser;
with SXML.Query;

package body Socat.Component with
   SPARK_Mode
is

   subtype Desc_Index is Positive range 1 .. 10;

   package Log is new Gneiss.Log;
   package Rom is new Gneiss.Rom (Character, Positive, String);
   package Packet is new Gneiss.Packet (Positive, Character, String, Desc_Index);

   subtype Server_Index is Gneiss.Session_Index range 1 .. 1;

   type Server_Meta is record
      Destination : String (1 .. 1024) := (others => Character'First);
      Last        : Natural := 0;
      Pid         : Integer := -1;
   end record;

   function Rom_Contract (S : Server_Meta) return Boolean is (True);

   procedure Update (Session : in out Packet.Server_Session;
                     Idx     :        Desc_Index;
                     Buf     :    out String;
                     Ctx     : in out Server_Meta) is null;

   procedure Read (Session : in out Packet.Server_Session;
                   Idx     :        Desc_Index;
                   Buf     :        String;
                   Ctx     : in out Server_Meta) is null;

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

   procedure Event is null;

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

   procedure Exec (Dest : String;
                   Src  : String) with
      Import,
      Convention    => C,
      External_Name => "socat_exec";

   procedure Check_Pid (Pid    :     Integer;
                        Status : out Integer) with
      Import,
      Convention    => C,
      External_Name => "socat_check_pid";

   package Packet_Server is new Packet.Server (Server_Meta, Initialize, Finalize, Event, Ready, Update, Read);
   package Linux_Server is new Packet_Server.Linux;
   package Packet_Dispatcher is new Packet.Dispatcher (Packet_Server, Dispatch);
   package Rom_Client is new Rom.Client (Server_Meta, Configure);
   package Log_Client is new Log.Client;

   procedure Configure is new Rom_Client.Update (Rom_Contract);

   Dispatcher  : Packet.Dispatcher_Session;
   Capability  : Gneiss.Capability;
   Server_Data : Server_Meta;
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
      Packet_Dispatcher.Register (Dispatcher);
   end Construct;

   procedure Destruct
   is
   begin
      null;
   end Destruct;

   procedure Initialize (Session : in out Packet.Server_Session;
                         Context : in out Server_Meta)
   is
   begin
      Log_Client.Info (Logger, "FD: " & Basalt.Strings.Image (Linux_Server.Get_Fd (Session)));
      Gneiss_Internal.Syscall.Fork (Context.Pid);
      if Context.Pid = 0 then
         Exec (Context.Destination (Context.Destination'First .. Context.Last) & ASCII.NUL,
               "FD:" & Basalt.Strings.Image (Linux_Server.Get_Fd (Session)) & ASCII.NUL);
         return;
      end if;
      if Context.Pid < 0 then
         return;
      end if;
      Log_Client.Info (Logger, "Child Pid: " & Basalt.Strings.Image (Context.Pid));
   end Initialize;

   procedure Finalize (Session : in out Packet.Server_Session;
                       Context : in out Server_Meta)
   is
   begin
      null;
   end Finalize;

   procedure Dispatch (Session  : in out Packet.Dispatcher_Session;
                       Disp_Cap :        Packet.Dispatcher_Capability;
                       Name     :        String;
                       Label    :        String)
   is
      Server : Packet.Server_Session;
      Status : Integer;
   begin
      Log_Client.Info (Logger, "Request from " & Name & ":" & Label);
      if
         not Packet_Dispatcher.Valid_Session_Request (Session, Disp_Cap)
         or else Ready (Server, Server_Data)
         or else Packet.Initialized (Server)
      then
         Log_Client.Warning (Logger, "[" & Name & ":" & Label & "] Server not free.");
         return;
      end if;
      Packet_Dispatcher.Session_Initialize (Session, Disp_Cap, Server, Server_Data, 1);
      Check_Pid (Server_Data.Pid, Status);
      if
         not Ready (Server, Server_Data)
         or else not Packet.Initialized (Server)
         or else Status /= 0
      then
         Log_Client.Warning (Logger, "[" & Name & ":" & Label & "] Server failed to initialize.");
         Server_Data.Pid := -1;
         return;
      end if;
      Packet_Dispatcher.Session_Accept (Session, Disp_Cap, Server, Server_Data);
      Server_Data.Pid := -1;
   end Dispatch;

   function Ready (Session : Packet.Server_Session;
                   Context : Server_Meta) return Boolean is (Context.Pid > 0);

   procedure Configure (Session : in out Rom.Client_Session;
                        Data    :        String;
                        Ctx     : in out Server_Meta)
   is
      pragma Unreferenced (Session);
      use type SXML.Result_Type;
      use type SXML.Parser.Match_Type;
      Document   : SXML.Document_Type (1 .. 128) := (others => SXML.Null_Node);
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

end Socat.Component;
