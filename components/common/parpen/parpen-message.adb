with Parpen.Binder;
with Parpen.Name_Service;
with Parpen.Protocol.Generic_Offsets;

package body Parpen.Message is

   package Offsets_Package is new Parpen.Protocol.Generic_Offsets (Types);
   package Name_Service is new Parpen.Name_Service (Types       => Types,
                                                    Num_Entries => Num_Name_DB_Entries);

   type Client_ID_Option (Valid : Boolean := False) is
      record
         case Valid is
            when True =>
               ID : Client_ID;
            when False =>
               null;
         end case;
      end record;

   NS_ID : Client_ID_Option := (Valid => False);

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
      Clients.Inner.Add_Client (ID    => ID,
                                State => (Receiving => False));
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
                       Send_Offset    :        Types.Bit_Length;
                       Send_Length    :        Types.Bit_Length;
                       Recv_Offset    :        Types.Bit_Length;
                       Recv_Length    :        Types.Bit_Length;
                       Offsets_Offset :        Types.Bit_Length;
                       Offsets_Length :        Types.Bit_Length;
                       Result         :    out Result_Type)
   is
      Receiver            : Client_ID;
      Node                : Resolve.Node_Option;
      Name_Service_Result : Name_Service.Result_Type;
      Send_Off            : Types.Bit_Length := Send_Offset;
      Send_Len            : Types.Bit_Length := Send_Length;
      Offsets_Off         : Types.Bit_Length := Offsets_Offset;
      Offsets_Len         : Types.Bit_Length := Offsets_Length;
      Receiver_State      : Client_State;
      Sender_State        : Client_State;
      use type Parpen.Protocol.Handle;
      use type Name_Service.Result_Type;
      use type Types.Bit_Length;
      use type Types.Index;
   begin
      if Handle = 0 then
         Receiver := NS_ID.ID;
      else
         Node := Clients.Inner.Get_Node (Owner_ID => Sender,
                                         Handle   => Parpen.Binder.Handle'Val (Parpen.Protocol.Handle'Pos (Handle)));
         if not Resolve.Found (Node) then
            Result := Result_Invalid_Handle;
            return;
         end if;
         Receiver := Resolve.Get_Owner (Node);
      end if;

      --  Update sender state
      if Recv_Length > 0 then
         Sender_State := (Receiving => True,
                          First     => Types.Index'Val (Types.Index'First + Types.Bit_Length'Pos (Recv_Offset / 8)),
                          Last      => Types.Index'Val (Types.Index'First
                                                        + Types.Bit_Length'Pos (Recv_Offset / 8 + Recv_Length / 8)));
      else
         Sender_State := (Receiving => False);
      end if;
      Set_Client_State (ID    => Sender,
                        State => Sender_State);

      if Send_Length = 0 then
         Result := Result_Valid;
         return;
      end if;

      --  Translate message
      Translate (Data           => Data,
                 Data_Offset    => Send_Offset,
                 Data_Length    => Send_Length,
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
                               Data_Offset    => Send_Off,
                               Data_Length    => Send_Len,
                               Offsets_Offset => Offsets_Off,
                               Offsets_Length => Offsets_Len,
                               Method         => Method,
                               Cookie         => Cookie,
                               Result         => Name_Service_Result);
         Result := (case Name_Service_Result is
                    when Name_Service.Result_Valid          => Result_Valid,
                    when Name_Service.Result_Invalid        => Result_Invalid,
                    when Name_Service.Result_Invalid_Method => Result_Invalid_Method);
         if Result /= Result_Valid then
            return;
         end if;

         if Oneway or Send_Len = 0 then
            Result := Result_Valid;
            return;
         end if;

         if Send_Len > Recv_Length then
            Result := Result_Receive_Buffer_Too_Small;
            return;
         end if;

         --  Translate response
         Translate (Data           => Data,
                    Data_Offset    => Send_Off,
                    Data_Length    => Send_Len,
                    Offsets_Offset => Offsets_Off,
                    Offsets_Length => Offsets_Len,
                    Source_ID      => Receiver,
                    Dest_ID        => Sender,
                    Result         => Result);
         if
            Result /= Result_Valid
            or else Send_Off mod 8 /= 0
            or else Send_Len mod 8 /= 0
         then
            return;
         end if;

         Send (ID          => Sender,
               Handle      => Handle,
               Method      => Method,
               Cookie      => Cookie,
               Oneway      => Oneway,
               Accept_FDs  => Accept_FDs,
               Data        => Data,
               Data_First  => Data'First + Types.Index (Send_Off / 8),
               Data_Last   => Data'First + Types.Index (Send_Off / 8 + Send_Len / 8 - 1),
               Recv_First  => Sender_State.First,
               Recv_Last   => Sender_State.Last);

         Result := Result_Valid;
         return;
      else
         Receiver_State := Get_Client_State (Receiver);
         if not Receiver_State.Receiving then
            Result := Result_Receiver_Not_Ready;
            return;
         end if;

         Send (ID          => Receiver,
               Handle      => Handle,
               Method      => Method,
               Cookie      => Cookie,
               Oneway      => Oneway,
               Accept_FDs  => Accept_FDs,
               Data        => Data,
               Data_First  => Types.Index'Val (Types.Index'Pos (Data'First) + Types.Bit_Index'Pos (Send_Offset) / 8),
               Data_Last   => Types.Index'Val (Types.Index'Pos (Data'First)
                                               + Types.Bit_Index'Pos (Send_Offset / 8 + Send_Length / 8 - 1)),
               Recv_First  => Receiver_State.First,
               Recv_Last   => Receiver_State.Last);

         Set_Client_State (Receiver, (Receiving => False));
         Result := Result_Valid;
         return;
      end if;
   end Dispatch;

   ----------------------
   -- Get_Client_State --
   ----------------------

   function Get_Client_State (ID : Client_ID) return Client_State is (Clients.Inner.Get_Client_State (ID));

   ----------------------
   -- Set_Client_State --
   ----------------------

   procedure Set_Client_State (ID    : Client_ID;
                               State : Client_State)
   is
   begin
      Clients.Inner.Set_Client_State (ID    => ID,
                                      State => State);
   end Set_Client_State;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Name_Service_ID : Client_ID)
   is
   begin
      Name_Service.Initialize;
      Clients.Inner.Initialize;
      NS_ID := (Valid => True,
                ID    => Name_Service_ID);
      Add_Client (ID => Name_Service_ID);
   end Initialize;

   ------------
   -- Ignore --
   ------------

   procedure Ignore (ID         : Client_ID;
                     Handle     : Parpen.Protocol.Handle;
                     Method     : Parpen.Protocol.Method;
                     Cookie     : Parpen.Protocol.Cookie;
                     Oneway     : Boolean;
                     Accept_FDs : Boolean;
                     Data       : Types.Bytes_Ptr;
                     Data_First : Types.Index;
                     Data_Last  : Types.Index;
                     Recv_First : Types.Index;
                     Recv_Last  : Types.Index)
   is
      pragma Unreferenced
         (ID, Handle, Method, Cookie, Oneway, Accept_FDs, Data, Data_First, Data_Last, Recv_First, Recv_Last);
   begin
      null;
   end Ignore;

end Parpen.Message;
