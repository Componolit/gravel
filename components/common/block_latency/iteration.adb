
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Timer.Client;
with Componolit.Gneiss.Strings;

package body Iteration is

   use all type Block.Request_Kind;
   use all type Block.Request_Status;

   procedure Event;

   procedure Event
   is
   begin
      null;
   end Event;

   package Timer_Client is new Cai.Timer.Client (Event);

   Timer : Cai.Timer.Client_Session;

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
      if not Cai.Timer.Initialized (Timer) then
         Timer_Client.Initialize (Timer, Cap);
      end if;
   end Initialize;

   procedure Send (C   : in out Block.Client_Session;
                   T   : in out Test;
                   Log : in out Cai.Log.Client_Session) is
      Result : Block.Result;
   begin
      for I in Cache'Range loop
         exit when T.Sent >= T.Data'Last;
         if Client.Status (Cache (I)) = Block.Raw then
            Client.Allocate_Request (C,
                                     Cache (I),
                                     Operation,
                                     Block.Id (T.Sent + 1) + T.Offset,
                                     1,
                                     I,
                                     Result);
            case Result is
               when Block.Success =>
                  T.Sent := T.Sent + 1;
                  if Client.Kind (Cache (I)) = Block.Write then
                     Client.Write (C, Cache (I));
                  end if;
               when Block.Unsupported =>
                  Cai.Log.Client.Error (Log, "Failed to send request");
               when others =>
                  null;
            end case;
         end if;
         if Client.Status (Cache (I)) = Block.Allocated then
            Client.Enqueue (C, Cache (I));
            if Client.Status (Cache (I)) = Block.Pending then
               Start (Cache (I), T.Offset, T.Data);
            end if;
         end if;
      end loop;
      Client.Submit (C);
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
   begin
      for I in Cache'Range loop
         if Client.Status (Cache (I)) = Block.Pending then
            Client.Update_Request (C, Cache (I));
            case Client.Status (Cache (I)) is
               when Block.Ok =>
                  if Client.Kind (Cache (I)) = Block.Read then
                     Client.Read (C, Cache (I));
                  end if;
                  Finish (Cache (I), T.Offset, T.Data);
                  T.Received := T.Received + 1;
                  Client.Release (C, Cache (I));
               when Block.Error =>
                  Finish (Cache (I), T.Offset, T.Data);
                  Cai.Log.Client.Warning (Log, "Request failed");
                  Client.Release (C, Cache (I));
               when others =>
                  null;
            end case;
         end if;
      end loop;
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
