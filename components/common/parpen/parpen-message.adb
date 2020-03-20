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
      use type Types.Bit_Length;
      Internal_Result : Result_Type;

      procedure Handle_Offset (O : Parpen.Protocol.Offset; Continue : out Boolean);
      procedure Handle_Offset (O : Parpen.Protocol.Offset; Continue : out Boolean)
      is
         R : Resolve.Result_Type;
         use type Resolve.Result_Type;
         use type Parpen.Protocol.Offset;
      begin
         if O > Parpen.Protocol.Offset (Data_Length) then
            Continue := False;
            return;
         end if;
         Resolve.Resolve (DB        => Clients.Inner,
                          Buffer    => Data,
                          Offset    => Data_Offset + Types.Bit_Length (O),
                          Length    => Data_Length - Types.Bit_Length (O),
                          Source_ID => Source_ID,
                          Dest_ID   => Dest_ID,
                          Result    => R);
         Continue := R = Resolve.Result_OK;
      end Handle_Offset;
      procedure Iterate_Offsets is new Message.Offsets (Handle_Offset);
   begin
      if Offsets_Length = 0 then
         Result := Result_Valid;
         return;
      end if;
      Iterate_Offsets (Data, Offsets_Offset, Offsets_Length, Internal_Result);
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
      for I in 0 .. Offsets_Length / 64 - 1 loop
         Offsets_Package.Initialize (Context,
                                     Data,
                                     Types.First_Bit_Index (Data'First) + Offsets_Offset + I * 64,
                                     Types.First_Bit_Index (Data'First) + Offsets_Offset + I * 64 + 63);
         Offsets_Package.Verify_Message (Context);

         Continue := False;
         if Offsets_Package.Valid_Message (Context) then
            Apply (Offsets_Package.Get_Data (Context), Continue);
         end if;
         Offsets_Package.Take_Buffer (Context, Data);

         if not Continue then
            Result := Result_Invalid;
            return;
         end if;
      end loop;
      Result := Result_Valid;
   end Offsets;

begin
   Clients.Inner.Initialize;
end Parpen.Message;
