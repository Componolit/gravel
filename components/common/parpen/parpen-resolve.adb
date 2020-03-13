with Parpen.Protocol.Generic_IBinder;

package body Parpen.Resolve is

   package IBinder_Package is new Parpen.Protocol.Generic_IBinder (Types);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (DB : in out Database)
   is
   begin
      DB.Nodes.Initialize;
      DB.Clients.Initialize;
   end Initialize;

   -----------------
   -- Insert_Node --
   -----------------

   procedure Insert_Node (DB     : in out Database'Class;
                          Cursor :        Node_Cursor_Option;
                          Owner  :        Client_ID;
                          Value  :        Parpen.Protocol.Binder)
   is
   begin
      DB.Nodes.Insert (K => Cursor.Inner.Cursor,
                       E => (Owner, Value));
   end Insert_Node;

   --------------------
   -- Resolve_Handle --
   --------------------

   procedure Resolve_Handle (DB     :        Database;
                             Buffer : in out Types.Bytes_Ptr;
                             Offset :        Types.Bit_Length;
                             Source :        Client_ID;
                             Dest   :        Client_ID;
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
