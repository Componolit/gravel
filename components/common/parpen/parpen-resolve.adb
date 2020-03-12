with Parpen.Protocol.Generic_IBinder;

package body Parpen.Resolve is

   package IBinder_Package is new Parpen.Protocol.Generic_IBinder (Types);

   procedure Resolve_Handle (Buffer : in out Types.Bytes_Ptr;
                             Offset :        Types.Bit_Length;
                             Result :    out Result_Type)
   is
      Context : IBinder_Package.Context := IBinder_Package.Create;
      use type Types.Bit_Length;
      use type Parpen.Protocol.Binder_Kind;
   begin
      IBinder_Package.Initialize (Context,
                                  Buffer,
                                  Types.Bit_Length (Buffer'First) + Offset,
                                  Types.Bit_Length (Buffer'Last));
      IBinder_Package.Verify_Message (Context);
      if not IBinder_Package.Valid_Message (Context) then
         Result := Result_Invalid;
         return;
      end if;

      if
         IBinder_Package.Get_Kind (Context) /= Parpen.Protocol.BK_WEAK_HANDLE
         and IBinder_Package.Get_Kind (Context) /= Parpen.Protocol.BK_STRONG_HANDLE
      then
         Result := Result_Needless;
         return;
      end if;

      Result := Result_Invalid;
   end Resolve_Handle;

end Parpen.Resolve;
