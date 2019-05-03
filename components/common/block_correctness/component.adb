
with Ada.Unchecked_Conversion;
with Cai.Log;
with Cai.Log.Client;
with Cai.Block;
with Cai.Block.Client;
with Cai.Configuration;
with Cai.Configuration.Client;
with LSC.AES_Generic;
with LSC.AES_Generic.CBC;
with SXML;
with SXML.Parser;
with SXML.Query;
with Permutation;
with Correctness;

package body Component with
   SPARK_Mode
is

   type Byte is mod 2 ** 8;
   type Unsigned_Long is range 0 .. 2 ** 63 - 1;
   type Buffer is array (Unsigned_Long range <>) of Byte;

   procedure Parse (S : String);

   package Config is new Cai.Configuration.Client (Character, Positive, String, Parse);

   Conf : Cai.Configuration.Client_Session := Config.Create;

   package Block is new Cai.Block (Byte, Unsigned_Long, Buffer);

   use all type Block.Id;
   use all type Block.Count;

   package Block_Permutation is new Permutation (Block.Id);

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

   package Block_Client is new Block.Client (Event, Read, Write);

   Client : Block.Client_Session;

   function Next (Current : Block.Id) return Block.Id;

   package Disk_Test is new Correctness (Block, Block_Client, Next);

   Data : Disk_Test.Test_State;

   Log : Cai.Log.Client_Session;

   function Next (Current : Block.Id) return Block.Id
   is
      Next_Block : Block.Id := Current + Block.Count'(1);
   begin
      if Block_Permutation.Has_Element then
         Block_Permutation.Next (Next_Block);
      else
         Cai.Log.Client.Error (Log, "Block permutation exceeded, increasing block number.");
      end if;
      return Next_Block;
   end Next;

   procedure Write (C :     Block.Client_Instance;
                    B :     Block.Size;
                    S :     Block.Id;
                    L :     Block.Count;
                    D : out Buffer)
   is
      pragma Unreferenced (C);
      pragma Unreferenced (B);
      pragma Unreferenced (L);
      function CBC_Key is new LSC.AES_Generic.Enc_Key (Unsigned_Long,
                                                       Byte,
                                                       Buffer);
      procedure CBC is new LSC.AES_Generic.CBC.Encrypt (Unsigned_Long,
                                                        Byte,
                                                        Buffer,
                                                        Unsigned_Long,
                                                        Byte,
                                                        Buffer);
      subtype Id is Buffer (1 .. 8);
      function Convert_Id is new Ada.Unchecked_Conversion (Block.Id, Id);
      Null_Block : constant Buffer (1 .. D'Length) := (others => 0);
      IV : Buffer (1 .. 16) := (others => 0);
      Key : constant Buffer (1 .. 128) := (others => 16#42#);
      --  This is no cryptographically secure encryption and only used to generate pseudo random blocks
   begin
      IV (1 .. 8) := Convert_Id (S);
      CBC (Null_Block, IV, CBC_Key (Key, LSC.AES_Generic.L128), D);
      Disk_Test.Hash_Block (Data.Write_Context, D);
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
      Disk_Test.Cache_Data (Data, S, D);
   end Read;

   Capability : Cai.Types.Capability;

   function Str_To_Long (S : String) return Long_Integer;

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
      use type SXML.Result_Type;
      use type SXML.Parser.Match_Type;
      Count : Long_Integer;
      Size  : Long_Integer;
      Document : SXML.Document_Type (1 .. 100) := (others => SXML.Null_Node);
      Result : SXML.Parser.Match_Type;
      Position : Natural;
      State : SXML.Query.State_Type;
   begin
      Cai.Log.Client.Info (Log, Character'Val (10) & S);
      SXML.Parser.Parse (S, Document, Result, Position);
      if not Block_Client.Initialized (Client) and Result = SXML.Parser.Match_OK then
         State := SXML.Query.Init (Document);
         if State.Result = SXML.Result_OK then
            State := SXML.Query.Path (State, Document, "/device");
            if
               State.Result = SXML.Result_OK
               and then SXML.Query.Has_Attribute (State, Document, "location")
            then
               Block_Client.Initialize (Client, Capability, SXML.Query.Attribute (State, Document, "location"));
               if Block_Client.Initialized (Client) then
                  Size  := Long_Integer (Block_Client.Block_Size (Client));
                  if SXML.Query.Has_Attribute (State, Document, "size") then
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
                                            & Disk_Test.Byte_Image (Count * Size)
                                            & ")...");
                  Disk_Test.Initialize (Client, Data, Log, Capability, Block.Count (Count));
                  Event;
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
      Cai.Log.Client.Info (Log, "Correctness");
      Config.Initialize (Conf, Capability);
      Config.Load (Conf);
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

   Success     : Boolean := True;
   First_Write : Boolean := True;
   First_Read  : Boolean := True;

   procedure Event
   is
   begin
      if
         Success
         and not Disk_Test.Bounds_Check_Finished (Data)
      then
         Disk_Test.Bounds_Check (Client, Data, Success, Log);
      end if;

      if
         Success
         and Disk_Test.Bounds_Check_Finished (Data)
         and not Disk_Test.Write_Finished (Data)
      then
         if First_Write then
            Block_Permutation.Initialize (Block.Id (Block_Client.Block_Count (Client) - 1));
            First_Write := False;
         end if;
         Disk_Test.Write (Client, Data, Success, Log);
      end if;

      if
         Success
         and Disk_Test.Bounds_Check_Finished (Data)
         and Disk_Test.Write_Finished (Data)
         and not Disk_Test.Read_Finished (Data)
      then
         if First_Read then
            Block_Permutation.Initialize (Block.Id (Block_Client.Block_Count (Client) - 1));
            First_Read := False;
         end if;
         Disk_Test.Read (Client, Data, Success, Log);
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
         Cai.Log.Client.Info (Log, "Correctness test "
                                   & (if Success then "succeeded." else "failed."));
         Correctness_Test.Vacate (Capability,
                                  (if Success then Correctness_Test.Success else Correctness_Test.Failure));
      end if;
   end Event;

end Component;
