
with Componolit.Interfaces.Log;
with Componolit.Interfaces.Log.Client;
with Componolit.Interfaces.Block;
with Componolit.Interfaces.Block.Client;
with Componolit.Interfaces.Rom;
with Componolit.Interfaces.Rom.Client;
with Componolit.Interfaces.Timer;
with Componolit.Interfaces.Timer.Client;
with SXML;
with SXML.Parser;
with SXML.Query;
with LSC.Internal.SHA256;
with Correctness;
with Output;

package body Component with
   SPARK_Mode
is

   type Byte is mod 2 ** 8;
   type Unsigned_Long is range 0 .. 2 ** 63 - 1;
   type Buffer is array (Unsigned_Long range <>) of Byte;

   procedure Parse (S : String);

   package Config is new Cai.Rom.Client (Character, Positive, String, Parse);
   package Block is new Cai.Block (Byte, Unsigned_Long, Buffer);
   package Timer_Client is new Cai.Timer.Client (Event);

   procedure Read (C : Block.Client_Instance;
                   B : Block.Size;
                   S : Block.Id;
                   L : Block.Count;
                   D : Buffer);

   procedure Write (C :     Block.Client_Instance;
                    B :     Block.Size;
                    S :     Block.Id;
                    L :     Block.Count;
                    D : out Buffer);

   function Str_To_Long (S : String) return Long_Integer;

   package Block_Client is new Block.Client (Event, Read, Write);
   package Disk_Test is new Correctness (Block, Block_Client, Timer_Client);

   procedure Transfer_State_1 with
      Ghost,
      Import,
      Contract_Cases => (Disk_Test.State_Initialized => Initialized,
                         not Disk_Test.State_Initialized => not Initialized);

   procedure Transfer_State_2 with
      Ghost,
      Import,
      Contract_Cases => (Initialized => Disk_Test.State_Initialized,
                         not Initialized => not Disk_Test.State_Initialized);

   Conf   : Cai.Rom.Client_Session := Config.Create;
   Client : Block.Client_Session             := Block_Client.Create;
   Timer  : Cai.Timer.Client_Session         := Timer_Client.Create;
   Log    : Cai.Log.Client_Session           := Cai.Log.Client.Create;
   Data   : Disk_Test.Test_State;

   Success    : Boolean := True;
   Capability : Cai.Types.Capability;

   procedure Write (C :     Block.Client_Instance;
                    B :     Block.Size;
                    S :     Block.Id;
                    L :     Block.Count;
                    D : out Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (B);
      pragma Unreferenced (L);
   begin
      Disk_Test.Block_Write (Data, S, D);
   end Write;

   procedure Read (C : Block.Client_Instance;
                   B : Block.Size;
                   S : Block.Id;
                   L : Block.Count;
                   D : Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (B);
      pragma Unreferenced (L);
   begin
      Disk_Test.Block_Read (Data, S, D);
   end Read;

   function Str_To_Long (S : String) return Long_Integer
   is
      L : Long_Integer := 0;
   begin
      for C of S loop
         pragma Loop_Invariant (L >= 0);
         exit when
            Long_Integer'Last - 10 < L
            or else Long_Integer'Last / 10 < L + 10;
         if C in '0' .. '9' then
            L := Character'Pos (C) - 48 + L;
            L := L * 10;
         end if;
      end loop;
      return L / 10;
   end Str_To_Long;

   procedure Parse (S : String)
   is
      use type Block.Size;
      use type SXML.Result_Type;
      use type SXML.Parser.Match_Type;
      Count : Long_Integer;
      Size  : Long_Integer;
      Document : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);
      Result : SXML.Parser.Match_Type := SXML.Parser.Match_Invalid;
      Position : Natural;
      State : SXML.Query.State_Type;
      use type SXML.Offset_Type;
   begin
      if SXML.Valid_Content (S'First, S'Last) then
         pragma Warnings (Off, "unused assignment to ""Position""");
         SXML.Parser.Parse (S, Document, Result, Position);
         pragma Warnings (On, "unused assignment to ""Position""");
      end if;
      if Cai.Log.Client.Initialized (Log) then
         if not Block_Client.Initialized (Client) and Result = SXML.Parser.Match_OK then
            State := SXML.Query.Init (Document);
            pragma Assert (Document'Length > 0);
            pragma Assert (State.Result = SXML.Result_OK);
            pragma Assert (SXML.Query.Offset (State) < Document'Length);
            pragma Assert (SXML.Query.Is_Valid (Document, State));
            --  FIXME: Proof of SXML does not work, some postconditions are not recognized when required
            --  as preconditions
            if State.Result = SXML.Result_OK and then SXML.Query.Is_Open (Document, State) then
               State := SXML.Query.Path (State, Document, "/device");
               if
                  State.Result = SXML.Result_OK
                  and then SXML.Query.Is_Open (Document, State)
                  and then SXML.Query.Has_Attribute (State, Document, "location")
               then
                  Block_Client.Initialize (Client, Capability, SXML.Query.Attribute (State, Document, "location"));
                  if
                     Block_Client.Initialized (Client)
                     and then Block_Client.Block_Size (Client) <= 4096
                     and then Block_Client.Block_Size (Client) > 0
                     and then Block_Client.Block_Size (Client) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
                  then
                     Size  := Long_Integer (Block_Client.Block_Size (Client));
                     if SXML.Query.Has_Attribute (State, Document, "size") then
                        Count := Str_To_Long (SXML.Query.Attribute (State, Document, "size")) / Size;
                        Count := (if
                                     Count > Long_Integer (Block_Client.Block_Count (Client))
                                  then
                                     Long_Integer (Block_Client.Block_Count (Client))
                                  else
                                     Count);
                     else
                        Count := Long_Integer (Block_Client.Block_Count (Client));
                     end if;
                     if Count > 1 then
                        Cai.Log.Client.Info (Log, "Testing disk: ");
                        Cai.Log.Client.Info (Log, Cai.Log.Image (Count)
                                                  & " x "
                                                  & Cai.Log.Image (Size)
                                                  & "b ("
                                                  & Output.Byte_Image ((if
                                                                           Long_Integer'Last / Count > Size
                                                                        then
                                                                           Count * Size
                                                                        else
                                                                           Long_Integer'Last))
                                                  & ")");
                        Disk_Test.Initialize (Client, Data, Block.Count (Count));
                        Transfer_State_1;
                        Event;
                     else
                        Cai.Log.Client.Error (Log, "Invalid block count: "
                                                & Cai.Log.Image (Count));
                     end if;
                  else
                     if not Block_Client.Initialized (Client) then
                        Cai.Log.Client.Error (Log, "Failed to initialize block session.");
                     else
                        Cai.Log.Client.Error (Log, "Invalid block size: "
                                              & Cai.Log.Image (Long_Integer (Block_Client.Block_Size (Client))));
                     end if;
                  end if;
               end if;
            end if;
         end if;
      else
         Correctness_Test.Vacate (Capability, Correctness_Test.Failure);
      end if;
   end Parse;

   procedure Construct (Cap : Cai.Types.Capability)
   is
   begin
      Capability := Cap;
      if not Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, Cap, "Correctness");
      end if;
      if not Config.Initialized (Conf) then
         Config.Initialize (Conf, Capability);
      end if;
      if not Timer_Client.Initialized (Timer) then
         Timer_Client.Initialize (Timer, Cap);
      end if;
      if Cai.Log.Client.Initialized (Log) and Config.Initialized (Conf) then
         Cai.Log.Client.Info (Log, "Correctness");
         Config.Load (Conf);
      else
         Correctness_Test.Vacate (Cap, Correctness_Test.Failure);
      end if;
   end Construct;

   procedure Destruct
   is
   begin
      if Block_Client.Initialized (Client) then
         Block_Client.Finalize (Client);
      end if;
      if Cai.Log.Client.Initialized (Log) then
         Cai.Log.Client.Finalize (Log);
      end if;
   end Destruct;

   Output_Bounds : Boolean := True;

   procedure Event
   is
      use type Block.Size;
   begin
      Transfer_State_2;
      if
         Cai.Log.Client.Initialized (Log)
         and Block_Client.Initialized (Client)
         and Timer_Client.Initialized (Timer)
      then
         if
            Success
            and not Disk_Test.Bounds_Check_Finished (Data)
         then
            Disk_Test.Bounds_Check (Client, Data, Success, Log, Timer);
            if Output_Bounds then
               if Success then
                  Cai.Log.Client.Info (Log, "Bounds check succeeded.");
               else
                  Cai.Log.Client.Error (Log, "Bounds check failed.");
               end if;
               Output_Bounds := False;
            end if;
         end if;

         if
            Block_Client.Block_Size (Client) > 0
            and Block_Client.Block_Size (Client) <= 4096
            and Block_Client.Block_Size (Client) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
         then
            if
               Success
               and Disk_Test.Bounds_Check_Finished (Data)
               and not Disk_Test.Write_Finished (Data)
            then
               Disk_Test.Write (Client, Data, Success, Log, Timer);
            end if;

            if
               Success
               and Disk_Test.Bounds_Check_Finished (Data)
               and Disk_Test.Write_Finished (Data)
               and not Disk_Test.Read_Finished (Data)
            then
               Disk_Test.Read (Client, Data, Success, Log, Timer);
            end if;
         else
            Cai.Log.Client.Error (Log, "Unsupported block size: "
                                  & Cai.Log.Image (Long_Integer (Block_Client.Block_Size (Client))));
            Success := False;
         end if;

         if
            Success
            and Disk_Test.Bounds_Check_Finished (Data)
            and Disk_Test.Write_Finished (Data)
            and Disk_Test.Read_Finished (Data)
            and not Disk_Test.Compare_Finished (Data)
         then
            Disk_Test.Compare (Data, Success);
         end if;

         if
            (Disk_Test.Bounds_Check_Finished (Data)
             and Disk_Test.Write_Finished (Data)
             and Disk_Test.Read_Finished (Data)
             and Disk_Test.Compare_Finished (Data))
            or not Success
         then
            pragma Assert (Cai.Log.Client.Initialized (Log));
            Cai.Log.Client.Info (Log, "Correctness test "
                                 & (if Success then "succeeded." else "failed."));
            Correctness_Test.Vacate (Capability,
                                     (if Success then Correctness_Test.Success else Correctness_Test.Failure));
         end if;
      else
         Correctness_Test.Vacate (Capability, Correctness_Test.Failure);
      end if;
   end Event;

end Component;
