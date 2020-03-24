with Parpen.Service_Manager.Generic_Request_Add_Service;
with Parpen.Binder.Generic_IBinder;
with Parpen.Container;

package body Parpen.Name_Service is

   package Req_AS_Package is new Parpen.Service_Manager.Generic_Request_Add_Service (Types);
   package Binder_Buffer is new Parpen.Container (Types, 24);
   package IBinder_Package is new Parpen.Binder.Generic_IBinder (Types);

   procedure Process (Data           : in out Types.Bytes_Ptr;
                      Data_Offset    : in out Types.Bit_Length;
                      Data_Length    : in out Types.Bit_Length;
                      Offsets_Offset :    out Types.Bit_Length;
                      Offsets_Length :    out Types.Bit_Length;
                      Source_ID      :        Client_ID;
                      Method         :        Parpen.Protocol.Method;
                      Result         :    out Result_Type)
   is
      pragma Unreferenced (Offsets_Offset, Offsets_Length, Source_ID);
      use type Parpen.Protocol.Method;
      use type Parpen.Binder.Binder_Kind;
      use type Types.Bit_Length;
      Context      : Req_AS_Package.Context := Req_AS_Package.Create;
      Handle       : Parpen.Binder.Handle;
      Handle_Valid : Boolean := True;

      procedure Parse_Binder (Server : Types.Bytes);
      procedure Parse_Binder (Server : Types.Bytes)
      is
         Binder_Context : IBinder_Package.Context := IBinder_Package.Create;
      begin
         Binder_Buffer.Ptr.all := Server;
         IBinder_Package.Initialize (Binder_Context, Binder_Buffer.Ptr);
         IBinder_Package.Verify_Message (Binder_Context);
         if IBinder_Package.Valid_Message (Binder_Context) then
            if IBinder_Package.Get_Kind (Binder_Context) = Parpen.Binder.BK_WEAK_HANDLE
               or IBinder_Package.Get_Kind (Binder_Context) = Parpen.Binder.BK_STRONG_HANDLE
            then
               Handle := IBinder_Package.Get_Handle (Binder_Context);
               Handle_Valid := True;
            end if;
         end if;
      end Parse_Binder;

      procedure Parse_Binder is new Req_AS_Package.Get_Server (Parse_Binder);
   begin
      pragma Unreferenced (Handle);
      --  Add service
      if Method = 3 then
         Req_AS_Package.Initialize (Ctx    => Context,
                                    Buffer => Data,
                                    First  => Types.First_Bit_Index (Data'First),
                                    Last   => Types.Last_Bit_Index (Data'First) + Data_Length);
         Req_AS_Package.Verify_Message (Context);
         if not Req_AS_Package.Structural_Valid_Message (Context) then
            Result := Result_Invalid;
            return;
         end if;
         Parse_Binder (Context);
         if not Handle_Valid then
            Result := Result_Invalid;
            return;
         end if;
         Result := Result_Invalid;
         return;
      end if;

      Data_Offset := 0;
      Data_Length := 0;
      Result := Result_Invalid;
   end Process;

end Parpen.Name_Service;

