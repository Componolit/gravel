
with SXML;
with SXML.Parser;
with SXML.Query;

package body Config with
   SPARK_Mode
is

   procedure Initialize (Cap     :     Cai.Types.Capability;
                         Success : out Boolean)
   is
   begin
      if not Cai.Rom.Initialized (Config_Client) then
         Instance.Initialize (Config_Client, Cap);
      end if;
      Success := Cai.Rom.Initialized (Config_Client);
      if Success then
         Instance.Load (Config_Client);
      end if;
   end Initialize;

   function Get_Delay return Duration is
      (Request_Delay);

   function Get_Client_Id return String
   is
   begin
      if Client_Id (Client_Id'First) = Character'First then
         return "";
      end if;
      for I in Client_Id'Range loop
         if Client_Id (I) = Character'First then
            return Client_Id (Client_Id'First .. I - 1);
         end if;
      end loop;
      return "";
   end Get_Client_Id;

   function Initialized return Boolean is
      (Is_Initialized);

   Document : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);

   procedure Check_Client (C : String)
   is
   begin
      if C'Length <= Client_Id'Length then
         Client_Id (Client_Id'First .. Client_Id'First + C'Length - 1) := C;
      end if;
   end Check_Client;

   function Duration_Value (D : String) return Duration
   is
      Dot        : Positive := (if D'Last < Positive'Last then D'Last + 1 else D'Last);
      Delay_Int  : Duration := 0.0;
      Delay_Frac : Duration := 0.0;
   begin
      for I in D'Range loop
         if D (I) = '.' then
            Dot := I;
            exit;
         end if;
      end loop;
      if Dot > D'First then
         for I in D'First .. Dot - 1 loop
            if D (I) in '0' .. '9' then
               Delay_Int := Delay_Int * 10.0;
               Delay_Int := Delay_Int + Duration (Character'Pos (D(I)) - 48);
            end if;
         end loop;
      end if;
      if D'Last > Dot then
         for I in reverse Dot + 1 .. D'Last loop
            if D(I) in '0' .. '9' then
               Delay_Frac := Delay_Frac + Duration (Character'Pos (D (I)) - 48);
               Delay_Frac := Delay_Frac / 10.0;
            end if;
         end loop;
      end if;
      return Delay_Int + Delay_Frac;
   end Duration_Value;

   function Get_Jitter return Duration
   is
   begin
      return Jitter;
   end Get_Jitter;

   function Get_Jitter_Distribution return Distribution
   is
   begin
      return J_Distribution;
   end Get_Jitter_Distribution;

   function Get_Mode return Mode
   is
   begin
      return Op_Mode;
   end Get_Mode;

   function Get_Slice return Duration
   is
   begin
      return Slice;
   end Get_Slice;

   procedure Parse (Data : String)
   is
      use type SXML.Parser.Match_Type;
      use type SXML.Result_Type;
      Result   : SXML.Parser.Match_Type := SXML.Parser.Match_Invalid;
      Position : Natural;
      State    : SXML.Query.State_Type;
   begin
      if not Is_Initialized then
         if SXML.Valid_Content (Data'First, Data'Last) then
            SXML.Parser.Parse (Data, Document, Result, Position);
         end if;
         if Result = SXML.Parser.Match_OK then
            State := SXML.Query.Init (Document);
            if State.Result = SXML.Result_OK and then SXML.Query.Is_Open (Document, State) then
               State := SXML.Query.Path (State, Document, "/test");
            end if;
            if
               State.Result /= SXML.Result_OK
               or else not SXML.Query.Is_Open (Document, State)
            then
               return;
            end if;
            if
               not SXML.Query.Has_Attribute (State, Document, "mode")
               or else not SXML.Query.Has_Attribute (State, Document, "device")
            then
               return;
            end if;
            Check_Client (SXML.Query.Attribute (State, Document, "device"));
            if SXML.Query.Attribute (State, Document, "mode") = "continuous" then
               Op_Mode := Continuous;
               if
                  not SXML.Query.Has_Attribute (State, Document, "delay")
                  or else not SXML.Query.Has_Attribute (State, Document, "jitter")
                  or else not SXML.Query.Has_Attribute (State, Document, "distribution")
               then
                  return;
               end if;
               Request_Delay := Duration_Value (SXML.Query.Attribute (State, Document, "delay"));
               Jitter        := Duration_Value (SXML.Query.Attribute (State, Document, "jitter"));
               if SXML.Query.Attribute (State, Document, "distribution") = "uniform" then
                  J_Distribution := Uniform;
               else
                  J_Distribution := None;
               end if;
               Is_Initialized := True;
            elsif SXML.Query.Attribute (State, Document, "mode") = "sliced" then
               Op_Mode := Sliced;
               Slice   := Duration_Value (SXML.Query.Attribute (State, Document, "slice"));
               Is_Initialized := True;
            end if;
         end if;
      end if;
   end Parse;

end Config;
