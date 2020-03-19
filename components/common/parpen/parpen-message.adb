package body Parpen.Message is

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Data           : in out Types.Bytes_Ptr;
                        Data_Offset    :        Types.Bit_Length;
                        Data_Length    :        Types.Bit_Length;
                        Offsets_Offset :        Types.Bit_Length;
                        Offsets_Length :        Types.Bit_Length;
                        Source_ID      :        Client_ID;
                        Dest_ID        :        Client_ID;
                        Result         :    out Result_Type)
   is
      pragma Unreferenced (Data, Data_Offset, Data_Length, Offsets_Offset, Source_ID, Dest_ID);
      use type Types.Bit_Length;
   begin
      if Offsets_Length = 0 then
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
