
with Componolit.Interfaces.Log.Client;
with Componolit.Interfaces.Timer.Client;
with Componolit.Interfaces.Strings;

package body Iteration is

   use all type Block.Request_Kind;
   use all type Block.Request_Status;

   procedure Event;

   procedure Event
   is
   begin
      null;
   end Event;

   procedure Allocate_Request (Id      : out Client.Request_Id;
                               Success : out Boolean)
   is
   begin
      Success := False;
      Id := Client.Request_Id'First;
      for I in Cache'Range loop
         if Client.Status (Cache (I)) = Block.Raw then
            Id := I;
            Success := True;
            exit;
         end if;
      end loop;
   end Allocate_Request;

   package Timer_Client is new Cai.Timer.Client (Event);

   Timer : Cai.Timer.Client_Session := Timer_Client.Create;

   procedure Start (Item   :        Client.Request;
                    Offset :        Block.Count;
                    Data   : in out Burst) with
      Pre => Long_Integer (Client.Start (Item) - Offset) in Data'Range;

   procedure Finish (Item   :        Client.Request;
                     Offset :        Block.Count;
                     Data   : in out Burst) with
      Pre => Long_Integer (Client.Start (Item) - Offset) in Data'Range;

   procedure Start (Item   :        Client.Request;
                    Offset :        Block.Count;
                    Data   : in out Burst)
   is
   begin
      Data (Long_Integer (Client.Start (Item) - Offset)).Start := Timer_Client.Clock (Timer);
   end Start;

   procedure Finish (Item   :        Client.Request;
                     Offset :        Block.Count;
                     Data   : in out Burst)
   is
   begin
      Data (Long_Integer (Client.Start (Item) - Offset)).Finish  := Timer_Client.Clock (Timer);
      Data (Long_Integer (Client.Start (Item) - Offset)).Success := Client.Status (Item) = Block.Ok;
   end Finish;

   procedure Initialize (T      : out Test;
                         Offset :     Block.Count;
                         S      :     Boolean;
                         Cap    :     Cai.Types.Capability)
   is
   begin
      T.Sent      := -1;
      T.Received  := -1;
      T.Offset    := Offset;
      T.Finished  := False;
      T.Sync      := S;
      for I in T.Data'Range loop
         T.Data (I) := (Success => False, others => Cai.Timer.Time'First);
      end loop;
      if not Timer_Client.Initialized (Timer) then
         Timer_Client.Initialize (Timer, Cap);
      end if;
   end Initialize;

   procedure Send (C   : in out Block.Client_Session;
                   T   : in out Test;
                   Log : in out Cai.Log.Client_Session) is
      Id    : Client.Request_Id;
      Ready : Boolean;
   begin
      if T.Sent < T.Data'Last then
         if Client.Initialized (C) then
            for I in T.Sent .. T.Data'Last - 1 loop
               Allocate_Request (Id, Ready);
               exit when not Ready;
               Client.Allocate_Request (C, Cache (Id), Operation, Block.Id (I + 1) + T.Offset, 1, Id);
               exit when Client.Status (Cache (Id)) = Block.Raw;
               Start (Cache (Id), T.Offset, T.Data);
               Client.Enqueue (C, Cache (Id));
               T.Sent := T.Sent + 1;
            end loop;
            Client.Submit (C);
         else
            Cai.Log.Client.Error (Log, "Failed to run test, client not initialized");
         end if;
      end if;
      if T.Sent = T.Data'Last and T.Received = T.Data'Last then
         if Operation = Block.Write and T.Sync then
            null;
            --  TODO: Sync after sync has been specified completely
         end if;
         T.Finished := True;
      end if;
   end Send;

   procedure Receive (C   : in out Block.Client_Session;
                      T   : in out Test;
                      Log : in out Cai.Log.Client_Session)
   is
      Id : Client.Request_Id;
      Rc : Client.Request_Handle;
   begin
      if Client.Initialized (C) then
         while T.Received < T.Data'Last loop
            Client.Update_Response_Queue (C, Rc);
            exit when not Client.Valid (Rc);
            Id := Client.Identifier (Rc);
            Client.Update_Request (C, Cache (Id), Rc);
            if Client.Kind (Cache (Id)) = Operation then
               if
                  Client.Kind (Cache (Id)) = Block.Write
                  and then Client.Status (Cache (Id)) = Block.Ok
               then
                  Client.Read (C, Cache (Id));
               end if;
               Finish (Cache (Id), T.Offset, T.Data);
               T.Received := T.Received + 1;
               Client.Release (C, Cache (Id));
            else
               Cai.Log.Client.Warning (Log, "Received unexpected request");
               Client.Release (C, Cache (Id));
            end if;
         end loop;
      else
         Cai.Log.Client.Error (Log, "Failed to run test, client not Initialized");
      end if;
      if T.Sent = T.Data'Last and T.Received = T.Data'Last then
         T.Finished := True;
      end if;
   end Receive;

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session;
                  R       :        Request;
                  B       :        Block.Id);

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session;
                  R       :        Request;
                  B       :        Block.Id)
   is
   begin
      Cai.Log.Client.Info (Xml_Log, "<request id=""" & Cai.Strings.Image (Long_Integer (B))
                                    & """ sent=""" & Cai.Strings.Image (Duration (R.Start))
                                    & """ received=""" & Cai.Strings.Image (Duration (R.Finish))
                                    & """ status=""" & (if R.Success then "OK" else "ERROR")
                                    & """/>");
   end Xml;

   procedure Xml (Xml_Log : in out Cai.Log.Client_Session;
                  B       :        Burst;
                  Offset  :        Block.Count)
   is
   begin
      for I in B'Range loop
         Xml (Xml_Log, B (I), Block.Id (I) + Offset);
      end loop;
   end Xml;

end Iteration;
