package body Parpen.Message is

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Data      : in out Types.Bytes_Ptr;
                        Offsets   :        Types.Bytes_Ptr;
                        Source_ID :        Client_ID;
                        Dest_ID   :        Client_ID;
                        Result    :    out Result_Type)
   is
      pragma Unreferenced (Data, Source_ID, Dest_ID);
   begin
      if Offsets'Length = 0 then
         Result := Result_Valid;
         return;
      end if;
      Result := Result_Invalid;
   end Translate;

   procedure Add_Client (ID : Client_ID)
   is
   begin
      Clients.Inner.Add_Client (ID);
   end Add_Client;

begin
   Clients.Inner.Initialize;
end Parpen.Message;
