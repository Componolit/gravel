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

   ----------------
   -- Add_Client --
   ----------------

   procedure Add_Client (DB : in out Database'Class;
                         ID :        Client_ID)
   is
      C : Client_DB.Option;
      use type Client_DB.Status;
   begin
      C := DB.Clients.Find (ID);
      if C.Result = Client_DB.Status_Not_Found then
         DB.Clients.Insert (K => ID, E => (Handles => Handle_DB.Null_DB));
      end if;
   end Add_Client;

   ----------------
   -- Add_Handle --
   ----------------

   procedure Add_Handle (DB    : in out Database'Class;
                         Owner :        Client_ID;
                         Node  :        Node_Cursor_Option)
   is
      procedure Add_Node (Client : in out Client_Type)
      is
         Result : Handle_DB.Option;
         use type Handle_DB.Status;
      begin
         Result := Client.Handles.Search_Value (Node.Inner.Cursor);
         if Result.Result = Handle_DB.Status_Not_Found then
            Client.Handles.Insert (K => Result.Cursor, E => Node.Inner.Cursor);
         end if;
      end Add_Node;

      procedure Add_Node is new Client_DB.Apply (Operation => Add_Node);
   begin
      Add_Node (DB.Clients, Owner);
   end Add_Handle;

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
      use type Client_DB.Status;

      C : Client_DB.Option;
   begin
      C := DB.Clients.Find (Source);
      if C.Result /= Client_DB.Status_OK then
         Result := Result_Invalid_Source;
         return;
      end if;

      C := DB.Clients.Find (Dest);
      if C.Result /= Client_DB.Status_OK then
         Result := Result_Invalid_Destination;
         return;
      end if;

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
