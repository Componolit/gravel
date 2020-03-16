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

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (DB     : in out Database'Class;
                       Cursor :        Node_Option;
                       Owner  :        Client_ID;
                       Value  :        Parpen.Protocol.Binder)
   is
   begin
      DB.Nodes.Insert (K => Cursor.Inner.Position,
                       E => (Owner, Value));
   end Add_Node;

   ----------------
   -- Add_Client --
   ----------------

   procedure Add_Client (DB : in out Database'Class;
                         ID :        Client_ID)
   is
   begin
      DB.Clients.Insert (K => ID, E => (Handles => Handle_DB.Null_DB));
   end Add_Client;

   ----------------
   -- Add_Handle --
   ----------------

   procedure Add_Handle (DB    : in out Database'Class;
                         Owner :        Client_ID;
                         Node  :        Node_Option)
   is
      procedure Add_Node (Client : in out Client_Type);

      procedure Add_Node (Client : in out Client_Type)
      is
         Result : Handle_DB.Option;
         use type Handle_DB.Status;
      begin
         Result := Client.Handles.Find (Node.Inner.Position);
         if Result.Result = Handle_DB.Status_Not_Found then
            Client.Handles.Insert (K => Result.Position, E => Node.Inner.Position);
         end if;
      end Add_Node;

      procedure Add_Node is new Client_DB.Generic_Apply (Operation => Add_Node);
   begin
      Add_Node (DB.Clients, Owner);
   end Add_Handle;

   --------------------
   -- Resolve_Handle --
   --------------------

   procedure Resolve_Handle (DB        :        Database;
                             Buffer    : in out Types.Bytes_Ptr;
                             Offset    :        Types.Bit_Length;
                             Source_ID :        Client_ID;
                             Dest_ID   :        Client_ID;
                             Result    :    out Result_Type)
   is
      Context : IBinder_Package.Context := IBinder_Package.Create;
      use type Types.Bit_Length;
      use type Parpen.Protocol.Binder_Kind;
      use type Client_DB.Status;
      use type Handle_DB.Status;

      Source_Handle    : Handle_DB.Option;
      Source_Handle_ID : Handle_ID;
      Source           : Client_DB.Option;
      Dest             : Client_DB.Option;
      Node             : Node_DB.Option;
      Handle           : Parpen.Protocol.Handle;
      Cookie           : Parpen.Protocol.Cookie;
      Flags            : Parpen.Protocol.Flat_Binder_Flags;
   begin
      Source := DB.Clients.Get (Source_ID);
      if Source.Result /= Client_DB.Status_OK then
         Result := Result_Invalid_Source;
         return;
      end if;

      Dest := DB.Clients.Get (Dest_ID);
      if Dest.Result /= Client_DB.Status_OK then
         Result := Result_Invalid_Destination;
         return;
      end if;

      IBinder_Package.Initialize (Context,
                                  Buffer,
                                  Types.First_Bit_Index (Buffer'First) + Offset,
                                  Types.Last_Bit_Index (Buffer'Last));
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

      Handle := IBinder_Package.Get_Handle (Context);
      if
         Parpen.Protocol.Handle'Pos (Handle) > Handle_ID'Pos (Handle_ID'Last)
         or Parpen.Protocol.Handle'Pos (Handle) < Handle_ID'Pos (Handle_ID'First)
      then
         Result := Result_Invalid_Handle;
         return;
      end if;

      Source_Handle_ID := Handle_ID'Val (Parpen.Protocol.Handle'Pos (Handle));

      Source_Handle := Source.Data.Handles.Get (Source_Handle_ID);
      if Source_Handle.Result /= Handle_DB.Status_OK then
         Result := Result_Handle_Not_Found;
         return;
      end if;

      Node := DB.Nodes.Get (Source_Handle.Data);
      if Node.Result /= Node_DB.Status_OK then
         Result := Result_Node_Not_Found;
         return;
      end if;

      if Node.Data.Owner = Dest_ID then
         --  Replace handle by binder to N.Data.Value
         Cookie := IBinder_Package.Get_Cookie (Context);
         Flags  := IBinder_Package.Get_Flags (Context);

         if IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_WEAK_HANDLE then
            IBinder_Package.Set_Kind (Context, Parpen.Protocol.BK_WEAK_BINDER);
         elsif IBinder_Package.Get_Kind (Context) /= Parpen.Protocol.BK_STRONG_HANDLE then
            IBinder_Package.Set_Kind (Context, Parpen.Protocol.BK_STRONG_BINDER);
         end if;
         IBinder_Package.Set_Arity (Context, Parpen.Protocol.BA_SINGLE);
         IBinder_Package.Set_Tag (Context, 16#85#);
         IBinder_Package.Set_Flags (Context, Flags);
         IBinder_Package.Set_Binder (Context, Node.Data.Value);
         IBinder_Package.Set_Cookie (Context, Cookie);
         IBinder_Package.Take_Buffer (Context, Buffer);
         Result := Result_OK;
         return;
      else
         --  Replace handle by handle in destination handle list
         null;
      end if;

      Result := Result_Invalid;
   end Resolve_Handle;

end Parpen.Resolve;
