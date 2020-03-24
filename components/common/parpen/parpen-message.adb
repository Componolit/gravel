with Parpen.Binder;
with Parpen.Name_Service;
with Parpen.Protocol.Generic_Offsets;

package body Parpen.Message is

   package Offsets_Package is new Parpen.Protocol.Generic_Offsets (Types);
   package Name_Service is new Parpen.Name_Service (Types       => Types,
                                                    Client_ID   => Client_ID,
                                                    Num_Entries => Num_Name_DB_Entries);

   Name_Service_ID : constant Client_ID := Client_ID'First;

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

      procedure Handle_Offset (O : Parpen.Protocol.Offset; Result : out Result_Type);
      procedure Handle_Offset (O : Parpen.Protocol.Offset; Result : out Result_Type)
      is
         R : Resolve.Result_Type;
         use type Resolve.Result_Type;
         use type Parpen.Protocol.Offset;
      begin
         if O > Parpen.Protocol.Offset (Data_Length) then
            Result := Result_Offset_Out_Of_Range;
            return;
         end if;
         Resolve.Resolve (DB        => Clients.Inner,
                          Buffer    => Data,
                          Offset    => Data_Offset + Types.Bit_Length (O),
                          Length    => Data_Length - Types.Bit_Length (O),
                          Source_ID => Source_ID,
                          Dest_ID   => Dest_ID,
                          Result    => R);
         Result := (case R is
                    when Resolve.Result_OK => Result_Valid,
                    when others            => Result_Invalid);
      end Handle_Offset;
      procedure Iterate_Offsets is new Message.Offsets (Handle_Offset);
   begin
      if Offsets_Length = 0 then
         Result := Result_Valid;
         return;
      end if;
      Iterate_Offsets (Data, Offsets_Offset, Offsets_Length, Result);
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
      Context : Offsets_Package.Context := Offsets_Package.Create;
   begin
      for I in 0 .. Offsets_Length / 64 - 1 loop
         Offsets_Package.Initialize (Context,
                                     Data,
                                     Types.First_Bit_Index (Data'First) + Offsets_Offset + I * 64,
                                     Types.First_Bit_Index (Data'First) + Offsets_Offset + I * 64 + 63);
         Offsets_Package.Verify_Message (Context);

         if Offsets_Package.Valid_Message (Context) then
            Operation (Offsets_Package.Get_Data (Context), Result);
         end if;
         Offsets_Package.Take_Buffer (Context, Data);

         if Result /= Result_Valid then
            return;
         end if;
      end loop;
      Result := Result_Valid;
   end Offsets;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (Sender         :        Client_ID;
                       Handle         :        Parpen.Protocol.Handle;
                       Method         :        Parpen.Protocol.Method;
                       Cookie         :        Parpen.Protocol.Cookie;
                       Oneway         :        Boolean;
                       Accept_FDs     :        Boolean;
                       Data           : in out Types.Bytes_Ptr;
                       Data_Offset    :        Types.Bit_Length;
                       Data_Length    :        Types.Bit_Length;
                       Offsets_Offset :        Types.Bit_Length;
                       Offsets_Length :        Types.Bit_Length;
                       Result         :    out Result_Type)
   is
      pragma Unreferenced (Cookie, Accept_FDs);
      Receiver            : Client_ID;
      Node                : Resolve.Node_Option;
      Name_Service_Result : Name_Service.Result_Type;
      Data_Off            : Types.Bit_Length := Data_Offset;
      Data_Len            : Types.Bit_Length := Data_Length;
      Offsets_Off         : Types.Bit_Length := Offsets_Offset;
      Offsets_Len         : Types.Bit_Length := Offsets_Length;
      use type Parpen.Protocol.Handle;
      use type Name_Service.Result_Type;
   begin
      if Handle = 0 then
         Receiver := Name_Service_ID;
      else
         Node := Clients.Inner.Get_Node (Owner_ID => Sender,
                                         Handle   => Parpen.Binder.Handle'Val (Parpen.Protocol.Handle'Pos (Handle)));
         if not Resolve.Found (Node) then
            Result := Result_Invalid_Handle;
            return;
         end if;
         Receiver := Resolve.Get_Owner (Node);
      end if;

      --  Translate message
      Translate (Data           => Data,
                 Data_Offset    => Data_Offset,
                 Data_Length    => Data_Length,
                 Offsets_Offset => Offsets_Offset,
                 Offsets_Length => Offsets_Length,
                 Source_ID      => Sender,
                 Dest_ID        => Receiver,
                 Result         => Result);
      if Result /= Result_Valid then
         return;
      end if;

      if Handle = 0 then
         Name_Service.Process (Data           => Data,
                               Data_Offset    => Data_Off,
                               Data_Length    => Data_Len,
                               Offsets_Offset => Offsets_Off,
                               Offsets_Length => Offsets_Len,
                               Source_ID      => Sender,
                               Method         => Method,
                               Result         => Name_Service_Result);
         if Name_Service_Result /= Name_Service.Result_Valid then
            Result := Result_Invalid;
            return;
         end if;

         if Oneway then
            Result := Result_Valid;
            return;
         end if;
      end if;

      Result := Result_Invalid;
   end Dispatch;

begin
   Clients.Inner.Initialize;
   Add_Client (Name_Service_ID);
end Parpen.Message;
