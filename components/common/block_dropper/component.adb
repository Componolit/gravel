
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Rom;
with Componolit.Gneiss.Rom.Client;
with SXML;
with SXML.Parser;
with SXML.Query;
with Block;
with Block.Service;
with Interfaces;

package body Component with
   SPARK_Mode
is

   procedure Parse (Data : String);
   procedure Fail (Message : String);

   package Config is new Cai.Rom.Client (Character, Positive, String, Parse);

   Cap  : Cai.Types.Capability;
   Log  : Cai.Log.Client_Session;
   Conf : Cai.Rom.Client_Session;
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
      if not Cai.Log.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, C, "Drop");
      end if;
      if not Cai.Log.Initialized (Log) then
         Main.Vacate (C, Main.Failure);
         return;
      end if;
      if not Cai.Rom.Initialized (Conf) then
         Config.Initialize (Conf, C);
      end if;
      if Cai.Rom.Initialized (Conf) then
         Config.Load (Conf);
      else
         Fail ("Failed to initialize config rom");
      end if;
   end Construct;

   function Parse_Int (S : String) return Interfaces.Unsigned_64;

   function Parse_Int (S : String) return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;
      Result : Interfaces.Unsigned_64 := 0;
   begin
      for I in S'Range loop
         if S (I) in '0' .. '9' then
            Result := Result + Character'Pos (S (I)) - 48;
            exit when I = S'Last;
            Result := Result * 10;
         end if;
      end loop;
      return Result;
   end Parse_Int;

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
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Doc, "operation") then
         Fail ("Missing attribute: operation");
         return;
      end if;
      declare
         use type Interfaces.Unsigned_64;
         Part_String : constant String := SXML.Query.Attribute (State, Doc, "part");
         Operation   : constant String := SXML.Query.Attribute (State, Doc, "operation");
         Delimiter   : Positive        := Part_String'First;
         Drop        : Boolean;
      begin
         if Operation = "drop" then
            Drop := True;
         elsif Operation = "modify" then
            Drop := False;
         else
            Fail ("Invalid operation: " & Operation);
            return;
         end if;
         for I in Part_String'Range loop
            if Part_String (I) = '/' then
               Delimiter := I;
               exit;
            end if;
         end loop;
         Block.Service.Start
            (Cap, Success,
             SXML.Query.Attribute (State, Doc, "device"),
             Interfaces.Unsigned_8 (Parse_Int (Part_String (Delimiter + 1 .. Part_String'Last)) mod 256),
             Interfaces.Unsigned_8 (Parse_Int (Part_String (Part_String'First .. Delimiter - 1)) mod 256),
             Parse_Int (SXML.Query.Attribute (State, Doc, "count")),
             Drop);
      end;
      if not Success then
         Fail ("Failed to start block server");
      end if;
   end Parse;

   procedure Destruct
   is
   begin
      if Cai.Log.Initialized (Log) then
         Cai.Log.Client.Finalize (Log);
      end if;
   end Destruct;

end Component;
