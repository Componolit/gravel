
with SXML;
with SXML.Parser;
with SXML.Query;

package body Config with
   SPARK_Mode
is

   Document : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);
   procedure Parse (Data : String)
   is
      use type SXML.Parser.Match_Type;
      use type SXML.Result_Type;
      Result    : SXML.Parser.Match_Type := SXML.Parser.Match_Invalid;
      Position  : Natural;
      State     : SXML.Query.State_Type;
   begin
      if not SXML.Valid_Content (Data'First, Data'Last) then
         F_Reason := Invalid_Content;
         return;
      end if;
      SXML.Parser.Parse (Data, Document, Result, Position);
      if Result /= SXML.Parser.Match_OK then
         F_Reason := Invalid_Xml;
         return;
      end if;
      State := SXML.Query.Init (Document);
      if
         State.Result /= SXML.Result_OK
         or else not SXML.Query.Is_Open (Document, State)
      then
         F_Reason := Open_Fail;
         return;
      end if;
      State := SXML.Query.Path (State, Document, "/test");
      if State.Result /= SXML.Result_OK then
         F_Reason := Invalid_Path;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "device") then
         F_Reason := Miss_Attr_Device;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "request_size") then
         F_Reason := Miss_Attr_Request_Size;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "operation") then
         F_Reason := Miss_Attr_Operation;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "data_size") then
         F_Reason := Miss_Attr_Data_Size;
         return;
      end if;
      if not SXML.Query.Has_Attribute (State, Document, "buffer_size") then
         F_Reason := Miss_Attr_Buffer_Size;
         return;
      end if;
      Ready    := True;
      F_Reason := Success;
      Req_Size := Block.Size (Parse_Number (SXML.Query.Attribute (State, Document, "request_size")));
      Dat_Size := Block.Byte_Length (Parse_Number (SXML.Query.Attribute (State, Document, "data_size")));
      Buf_Size := Block.Byte_Length (Parse_Number (SXML.Query.Attribute (State, Document, "buffer_size")));
      if SXML.Query.Attribute (State, Document, "operation") = "write" then
         Op := Block.Write;
      else
         Op := Block.Read;
      end if;
      declare
         Raw_Dev : constant String := SXML.Query.Attribute (State, Document, "device");
      begin
         if Raw_Dev'Length <= Dev'Length then
            Dev (Dev'First .. Dev'First + Raw_Dev'Length - 1) := Raw_Dev;
         else
            Dev := Raw_Dev (Raw_Dev'First .. Raw_Dev'First + Dev'Length - 1);
         end if;
      end;
   end Parse;

   function Initialized return Boolean is (Ready);

   function Request_Size return Block.Size is (Req_Size);

   function Data_Size return Block.Byte_Length is (Dat_Size);

   function Buffer_Size return Block.Byte_Length is (Buf_Size);

   function Operation return Block.Request_Kind is (Op);

   function Device return String
   is
      Last : Positive := 1;
   begin
      for I in Dev'Range loop
         if Dev (I) = Character'First then
            Last := I - 1;
            exit;
         end if;
      end loop;
      return Dev (Dev'First .. Last);
   end Device;

   function Parse_Number (S : String) return Long_Integer
   is
      V : Long_Integer := 0;
   begin
      for C in S'Range loop
         case S (C) is
            when 'k' =>
               V := V * 1024;
               exit;
            when 'M' =>
               V := V * 1024 ** 2;
               exit;
            when 'G' =>
               V := V * 1024 ** 3;
               exit;
            when '0' .. '9' =>
               V := V + (Character'Pos (S (C)) - 48);
               if C /= S'Last and then S (C + 1) in '0' .. '9' then
                  V := V * 10;
               end if;
               exit when C = S'Last;
            when others =>
               exit;
         end case;
      end loop;
      return V;
   end Parse_Number;

   function Failure_Reason return String
   is
      (case F_Reason is
         when Not_Initialized        => "not initialized",
         when Success                => "success",
         when Invalid_Content        => "invalid content",
         when Invalid_Xml            => "invalid xml",
         when Open_Fail              => "failed to open document",
         when Invalid_Path           => "invalid xml path",
         when Miss_Attr_Device       => "missing attribute: device",
         when Miss_Attr_Request_Size => "missing attribute: request_size",
         when Miss_Attr_Operation    => "missing attribute: operation",
         when Miss_Attr_Data_Size    => "missing attribute: data_size",
         when Miss_Attr_Buffer_Size  => "missing attribute: buffer_size");

end Config;
