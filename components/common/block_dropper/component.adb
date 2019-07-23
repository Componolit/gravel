
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Log.Client;
with Componolit.Interfaces.Rom;
with Componolit.Interfaces.Rom.Client;
with SXML;
with SXML.Parser;
with SXML.Query;
with Block;
with Block.Service;

package body Component with
   SPARK_Mode
is

   procedure Parse (Data : String);
   procedure Fail (Message : String);

   package Config is new Cai.Rom.Client (Character, Positive, String, Parse);

   Cap  : Cai.Types.Capability;
   Log  : Cai.Log.Client_Session        := Cai.Log.Client.Create;
   Conf : Cai.Rom.Client_Session        := Config.Create;
   Doc  : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);

   procedure Fail (Message : String)
   is
   begin
      Cai.Log.Client.Error (Log, Message);
      Main.Vacate (Cap, Main.Failure);
   end Fail;

   procedure Construct (C : Cai.Types.Capability)
   is
   begin
      Cap := C;
      if not Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, C, "Drop");
      end if;
      if not Cai.Log.Client.Initialized (Log) then
         Main.Vacate (C, Main.Failure);
         return;
      end if;
      if not Config.Initialized (Conf) then
         Config.Initialize (Conf, C);
      end if;
      if Config.Initialized (Conf) then
         Config.Load (Conf);
      else
         Fail ("Failed to initialize config rom");
      end if;
   end Construct;

   procedure Parse (Data : String)
   is
      use type SXML.Result_Type;
      use type SXML.Parser.Match_Type;
      Result   : SXML.Parser.Match_Type := SXML.Parser.Match_Invalid;
      Position : Natural;
      State    : SXML.Query.State_Type;
      Success  : Boolean;
   begin
      if not SXML.Valid_Content (Data'First, Data'Last) then
         Fail ("Invalid config");
         return;
      end if;
      SXML.Parser.Parse (Data, Doc, Result, Position);
      if Result /= SXML.Parser.Match_Ok then
         Fail ("Failed to parse XML");
         return;
      end if;
      State := SXML.Query.Init (Doc);
      if
         State.Result /= SXML.Result_Ok
         or else not SXML.Query.Is_Open (Doc, State)
      then
         Fail ("Failed to init document");
         return;
      end if;
      State := SXML.Query.Path (State, Doc, "/test");
      if
         State.Result /= SXML.Result_Ok
         or else not SXML.Query.Is_Open (Doc, State)
      then
         Fail ("Node test not found");
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Doc, "device") then
         Fail ("Missing attribute: device");
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Doc, "part") then
         Fail ("Missing attribute: part");
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Doc, "count") then
         Fail ("Missing attribute: count");
      end if;
      Block.Service.Start (Cap, Success,
                           SXML.Query.Attribute (State, Doc, "device"),
                           0,
                           0,
                           0);
      if not Success then
         Fail ("Failed to start block server");
      end if;
   end Parse;

   procedure Destruct
   is
   begin
      if Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Finalize (Log);
      end if;
   end Destruct;

end Component;
