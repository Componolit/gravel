
with Cai.Log.Client;
with Cai.Timer.Client;

package body Iteration is

   use all type Block.Request_Kind;
   use all type Block.Request_Status;

   Timer : Cai.Timer.Client_Session := Cai.Timer.Client.Create;

   procedure Start (Item   :        Client.Request;
                    Offset :        Block.Count;
                    Data   : in out Burst) with
      Pre => Long_Integer (Item.Start - Offset) in Data'Range;

   procedure Finish (Item   :        Client.Request;
                     Offset :        Block.Count;
                     Data   : in out Burst) with
      Pre => Long_Integer (Item.Start - Offset) in Data'Range;

   procedure Start (Item   :        Client.Request;
                    Offset :        Block.Count;
                    Data   : in out Burst)
   is
   begin
      Data (Long_Integer (Item.Start - Offset)).Start := Cai.Timer.Client.Clock (Timer);
   end Start;

   procedure Finish (Item   :        Client.Request;
                     Offset :        Block.Count;
                     Data   : in out Burst)
   is
   begin
      Data (Long_Integer (Item.Start - Offset)).Finish  := Cai.Timer.Client.Clock (Timer);
      Data (Long_Integer (Item.Start - Offset)).Success := Item.Status = Block.Ok;
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
      if not Cai.Timer.Client.Initialized (Timer) then
         Cai.Timer.Client.Initialize (Timer, Cap);
      end if;
   end Initialize;

   procedure Send (C   : in out Block.Client_Session;
                   T   : in out Test;
                   Log : in out Cai.Log.Client_Session) is
      Req : Client.Request (Kind => Operation);
   begin
      Req.Priv := Block.Null_Data;
      Req.Length := 1;
      Req.Status := Block.Raw;
      if T.Sent < T.Data'Last then
         if Client.Initialized (C) then
            for I in T.Sent .. T.Data'Last - 1 loop
               Req.Start  := Block.Id (I + 1) + T.Offset;
               exit when not Client.Ready (C, Req);
               case Operation is
                  pragma Warnings (Off, "this code can never be executed and has been deleted");
                  --  Operation is a generic parameter of this package
                  --  In a package instance the compiler deletes the branches that cannot be reached
                  when Block.Write | Block.Read =>
                     Start (Req, T.Offset, T.Data);
                     Client.Enqueue (C, Req);
                  when others =>
                     null;
                  pragma Warnings (On, "this code can never be executed and has been deleted");
               end case;
               T.Sent := T.Sent + 1;
            end loop;
            Client.Submit (C);
         else
            Cai.Log.Client.Error (Log, "Failed to run test, client not initialized");
         end if;
      end if;
      if T.Sent = T.Data'Last and T.Received = T.Data'Last then
         if Operation = Block.Write and T.Sync then
            declare
               S : constant Client.Request := (Kind   => Block.Sync,
                                               Priv   => Block.Null_Data,
                                               Start  => 0,
                                               Length => 0,
                                               Status => Block.Raw);
            begin
               if Client.Supported (C, S.Kind) then
                  while not Client.Ready (C, S) loop
                     null;
                  end loop;
                  Client.Enqueue (C, S);
                  Client.Submit (C);
               end if;
            end;
         end if;
         T.Finished := True;
      end if;
   end Send;

   procedure Receive (C   : in out Block.Client_Session;
                      T   : in out Test;
                      Log : in out Cai.Log.Client_Session) is
   begin
      if Client.Initialized (C) then
         while T.Received < T.Data'Last loop
            declare
               R : Client.Request := Client.Next (C);
            begin
               if R.Kind = Operation then
                  if R.Kind = Block.Read and then R.Status = Block.Ok then
                     Client.Read (C, R);
                  end if;
                  Finish (R, T.Offset, T.Data);
                  T.Received := T.Received + 1;
                  Client.Release (C, R);
               elsif R.Kind = Block.None then
                  exit;
               else
                  Cai.Log.Client.Warning (Log, "Received unexpected request");
               end if;
            end;
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
      Cai.Log.Client.Info (Xml_Log, "<request id=""" & Cai.Log.Image (Long_Integer (B))
                                    & """ sent=""" & Cai.Log.Image (Duration (R.Start))
                                    & """ received=""" & Cai.Log.Image (Duration (R.Finish))
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
