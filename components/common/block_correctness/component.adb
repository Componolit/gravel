
with Ada.Unchecked_Conversion;
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Cai.Configuration;
with Cai.Configuration.Client;
with Cai.Timer;
with Cai.Timer.Client;
with SXML;
with SXML.Parser;
with SXML.Query;
with Correctness;
with Output;

package body Component with
   SPARK_Mode
is

   type Byte is mod 2 ** 8;
   type Unsigned_Long is range 0 .. 2 ** 63 - 1;
   type Buffer is array (Unsigned_Long range <>) of Byte;

   procedure Parse (S : String);

   package Config is new Cai.Configuration.Client (Character, Positive, String, Parse);
   package Block is new Cai.Block (Byte, Unsigned_Long, Buffer);

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

   function Str_To_Long (S : String) return Long_Integer with
     Pre => S'Length < 20 and (for all C of S => C in '0' .. '9'),
     Post => Str_To_Long'Result >= 0;

   package Block_Client is new Block.Client (Event, Read, Write);
   package Disk_Test is new Correctness (Block, Block_Client);

   Conf   : Cai.Configuration.Client_Session := Config.Create;
   Client : Block.Client_Session             := Block_Client.Create;
   Log    : Cai.Log.Client_Session           := Cai.Log.Client.Create;
   Timer  : Cai.Timer.Client_Session         := Cai.Timer.Client.Create;
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
         L := Character'Pos (C) - 48 + L;
         L := L * 10;
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
      Result : SXML.Parser.Match_Type;
      Position : Natural;
      State : SXML.Query.State_Type;
   begin
      SXML.Parser.Parse (S, Document, Result, Position);
      if not Block_Client.Initialized (Client) and Result = SXML.Parser.Match_OK then
         State := SXML.Query.Init (Document);
         if State.Result = SXML.Result_OK and then SXML.Query.Is_Open (Document, State) then
            State := SXML.Query.Path (State, Document, "/device");
            if
              State.Result = SXML.Result_OK
              and then SXML.Query.Is_Open (Document, State)
               and then SXML.Query.Has_Attribute (State, Document, "location")
            then
               Block_Client.Initialize (Client, Capability, SXML.Query.Attribute (State, Document, "location"));
               if Block_Client.Initialized (Client) and then Block_Client.Block_Size (Client) <= 4096 then
                  Size  := Long_Integer (Block_Client.Block_Size (Client));
                  if SXML.Query.Has_Attribute (State, Document, "size") and Size > 0 then
                     Count := Str_To_Long (SXML.Query.Attribute (State, Document, "size")) / Size;
                     if Count > Long_Integer (Block_Client.Block_Count (Client)) then
                        Count := Long_Integer (Block_Client.Block_Count (Client));
                     end if;
                  else
                     Count := Long_Integer (Block_Client.Block_Count (Client));
                  end if;
                  Cai.Log.Client.Info (Log, "Running correctness test over "
                                            & Cai.Log.Image (Count)
                                            & " blocks of "
                                            & Cai.Log.Image (Size)
                                            & " byte size ("
                                            & Output.Byte_Image (Count * Size)
                                       & ")...");
                  if Count > 1 then
                     Disk_Test.Initialize (Client, Data, Block.Count (Count));
                     Event;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Parse;

   procedure Construct (Cap : Cai.Types.Capability)
   is
   begin
      Capability := Cap;
      Cai.Log.Client.Initialize (Log, Cap, "Correctness");
      Config.Initialize (Conf, Capability);
      Cai.Timer.Client.Initialize (Timer, Cap);
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

   procedure Event
   is
   begin
      if
        Cai.Log.Client.Initialized (Log)
        and Block_Client.Initialized (Client)
        and Cai.Timer.Client.Initialized (Timer)
      then
         if
           Success
           and not Disk_Test.Bounds_Check_Finished (Data)
         then
            Disk_Test.Bounds_Check (Client, Data, Success, Log, Timer);
         end if;

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
