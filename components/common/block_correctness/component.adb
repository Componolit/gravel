
with Componolit.Gneiss.Log;
with Componolit.Gneiss.Log.Client;
with Componolit.Gneiss.Block;
with Componolit.Gneiss.Block.Client;
with Componolit.Gneiss.Rom;
with Componolit.Gneiss.Rom.Client;
with Componolit.Gneiss.Timer;
with Componolit.Gneiss.Timer.Client;
with Componolit.Gneiss.Strings;
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

   type Request_Id is mod 2 ** 6;

   type Session_Id is new Boolean;

   procedure Parse (S : String);

   package Config is new Cai.Rom.Client (Character, Positive, String, Parse);
   package Block is new Cai.Block (Byte, Unsigned_Long, Buffer, Session_Id, Request_Id);
   package Timer_Client is new Cai.Timer.Client (Event);

   procedure Read (C : in out Block.Client_Session;
                   I :        Request_Id;
                   D :        Buffer);

   procedure Write (C : in out Block.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer);

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

   Conf   : Cai.Rom.Client_Session;
   Client : Block.Client_Session;
   Timer  : Cai.Timer.Client_Session;
   Log    : Cai.Log.Client_Session;
   Data   : Disk_Test.Test_State;

   Success    : Boolean := True;
   Capability : Cai.Types.Capability;

   procedure Write (C : in out Block.Client_Session;
                    I :        Request_Id;
                    D :    out Buffer)
   is
      pragma Unreferenced (C);
   begin
      Disk_Test.Block_Write (Data, I, D);
   end Write;

   procedure Read (C : in out Block.Client_Session;
                   I :        Request_Id;
                   D :        Buffer)
   is
      pragma Unreferenced (C);
   begin
      Disk_Test.Block_Read (Data, I, D);
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
      if Cai.Log.Initialized (Log) then
         if not Block.Initialized (Client) and Result = SXML.Parser.Match_OK then
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
                  Block_Client.Initialize (Client,
                                           Capability,
                                           SXML.Query.Attribute (State, Document, "location"),
                                           True);
                  if
                     Block.Initialized (Client)
                     and then Block.Block_Size (Client) <= 4096
                     and then Block.Block_Size (Client) > 0
                     and then Block.Block_Size (Client) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
                  then
                     Size  := Long_Integer (Block.Block_Size (Client));
                     if SXML.Query.Has_Attribute (State, Document, "size") then
                        Count := Str_To_Long (SXML.Query.Attribute (State, Document, "size")) / Size;
                        Count := (if
                                     Count > Long_Integer (Block.Block_Count (Client))
                                  then
                                     Long_Integer (Block.Block_Count (Client))
                                  else
                                     Count);
                     else
                        Count := Long_Integer (Block.Block_Count (Client));
                     end if;
                     if Count > 1 then
                        Cai.Log.Client.Info (Log, "Testing disk: ");
                        Cai.Log.Client.Info (Log, Cai.Strings.Image (Count)
                                                  & " x "
                                                  & Cai.Strings.Image (Size)
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
                                                & Cai.Strings.Image (Count));
                     end if;
                  else
                     if not Block.Initialized (Client) then
                        Cai.Log.Client.Error (Log, "Failed to initialize block session.");
                     else
                        Cai.Log.Client.Error (Log, "Invalid block size: "
                                              & Cai.Strings.Image (Long_Integer (Block.Block_Size (Client))));
                     end if;
                  end if;
               end if;
            end if;
         end if;
      else
         Main.Vacate (Capability, Main.Failure);
      end if;
   end Parse;

   procedure Construct (Cap : Cai.Types.Capability)
   is
   begin
      Capability := Cap;
      if not Cai.Log.Initialized (Log) then
         Cai.Log.Client.Initialize (Log, Cap, "Correctness");
      end if;
      if not Cai.Rom.Initialized (Conf) then
         Config.Initialize (Conf, Capability);
      end if;
      if not Cai.Timer.Initialized (Timer) then
         Timer_Client.Initialize (Timer, Cap);
      end if;
      if Cai.Log.Initialized (Log) and Cai.Rom.Initialized (Conf) then
         Cai.Log.Client.Info (Log, "Correctness");
         Config.Load (Conf);
      else
         Main.Vacate (Cap, Main.Failure);
      end if;
   end Construct;

   procedure Destruct
   is
   begin
      if Block.Initialized (Client) then
         Block_Client.Finalize (Client);
      end if;
      if Cai.Log.Initialized (Log) then
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
         Cai.Log.Initialized (Log)
         and Block.Initialized (Client)
         and Cai.Timer.Initialized (Timer)
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
            Block.Block_Size (Client) > 0
            and Block.Block_Size (Client) <= 4096
            and Block.Block_Size (Client) mod (LSC.Internal.SHA256.Block_Size / 8) = 0
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
                                  & Cai.Strings.Image (Long_Integer (Block.Block_Size (Client))));
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
            pragma Assert (Cai.Log.Initialized (Log));
            Cai.Log.Client.Info (Log, "Correctness test "
                                 & (if Success then "succeeded." else "failed."));
            Main.Vacate (Capability, (if Success then Main.Success else Main.Failure));
         end if;
      else
         Main.Vacate (Capability, Main.Failure);
      end if;
   end Event;

end Component;
