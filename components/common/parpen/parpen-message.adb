with Parpen.Protocol.Generic_Offsets;

package body Parpen.Message is

   package Offsets_Package is new Parpen.Protocol.Generic_Offsets (Types);

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

   ----------------
   -- Add_Client --
   ----------------

   procedure Add_Client (ID : Client_ID)
   is
   begin
      Clients.Inner.Add_Client (ID);
   end Add_Client;

   -------------
   -- Offsets --
   -------------

   procedure Offsets (Data           : in out Types.Bytes_Ptr;
                      Offsets_Offset :        Types.Bit_Length;
                      Offsets_Length :        Types.Bit_Length;
                      Result         :    out Result_Type)
   is
      use type Types.Bit_Length;
      Context  : Offsets_Package.Context := Offsets_Package.Create;
      Continue : Boolean;
   begin
      Result := Result_Invalid;
      for I in 0 .. Offsets_Length / 64 - 1 loop
         Offsets_Package.Initialize (Context,
                                     Data,
                                     Types.First_Bit_Index (Data'First) + Offsets_Offset + I * 64,
                                     Types.First_Bit_Index (Data'First) + Offsets_Offset + I * 64 + 63);
         Offsets_Package.Verify_Message (Context);
         if not Offsets_Package.Valid_Message (Context) then
            return;
         end if;
         Apply (Offsets_Package.Get_Data (Context), Continue);
         if not Continue then
            return;
         end if;
         Offsets_Package.Take_Buffer (Context, Data);
      end loop;
      Result := Result_Valid;
   end Offsets;

begin
   Clients.Inner.Initialize;
end Parpen.Message;
