
with SXML;
with SXML.Query;
with SXML.Parser;

package body Config with
   SPARK_Mode
is

   Document : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);
   procedure Parse (D : String)
   is
      use type SXML.Parser.Match_Type;
      use type SXML.Result_Type;
      Result   : SXML.Parser.Match_Type := SXML.Parser.Match_Invalid;
      Position : Natural;
      State    : SXML.Query.State_Type;
   begin
      if not SXML.Valid_Content (D'First, D'Last) then
         Rsn := Invalid_Content;
         return;
      end if;
      SXML.Parser.Parse (D, Document, Result, Position);
      if Result /= SXML.Parser.Match_OK then
         Rsn := Invalid_Xml;
         return;
      end if;
      State := SXML.Query.Init (Document);
      if
         State.Result /= SXML.Result_OK
         or else not SXML.Query.Is_Open (Document, State)
      then
         Rsn := Open_Fail;
         return;
      end if;
      State := SXML.Query.Path (State, Document, "/throttle");
      if State.Result /= SXML.Result_OK then
         Rsn := Invalid_Path;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "device") then
         Rsn := Miss_Attr_Device;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "reqs") then
         Rsn := Miss_Attr_Reqs;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "freq") then
         Rsn := Miss_Attr_Freq;
         return;
      end if;
      Freq := Parse_Natural (SXML.Query.Attribute (State, Document, "freq"));
      Reqs := Parse_Natural (SXML.Query.Attribute (State, Document, "reqs"));
      declare
         Raw_Dev : constant String := SXML.Query.Attribute (State, Document, "device");
      begin
         if Raw_Dev'Length <= Dev'Length then
            Dev (Dev'First .. Dev'First + Raw_Dev'Length - 1) := Raw_Dev;
         else
            Dev := Raw_Dev (Raw_Dev'First .. Raw_Dev'First + Dev'Length - 1);
         end if;
      end;
      Ready := True;
   end Parse;

   function Initialized return Boolean is (Ready);

   function Device return String
   is
      Last : Positive := Dev'First;
   begin
      if Dev (Dev'First) = Character'First then
         return "";
      end if;
      for I in Dev'Range loop
         if Dev (I) = Character'First then
            Last := I - 1;
            exit;
         end if;
      end loop;
      return Dev (Dev'First .. Last);
   end Device;

   function Rate return Natural is (Reqs);

   function Frequency return Natural is (Freq);

   function Reason return String is
      (case Rsn is
         when Success          => "success",
         when Uninitialized    => "uninitialized",
         when Invalid_Content  => "invalid content",
         when Invalid_Xml      => "invalid xml",
         when Open_Fail        => "failed to open document",
         when Invalid_Path     => "invalid path",
         when Miss_Attr_Device => "attribute missing: device",
         when Miss_Attr_Reqs   => "attribute missing: reqs",
         when Miss_Attr_Freq   => "attribute missing: freq");

   function Parse_Natural (S : String) return Natural
   is
      N : Natural := 0;
   begin
      for I in S'Range loop
         if S (I) in '0' .. '9' then
            N := N + (Character'Pos (S (I)) - 48);
            if I /= S'Last then
               N := N * 10;
            end if;
         else
            exit;
         end if;
      end loop;
      return N;
   end Parse_Natural;

end Config;
