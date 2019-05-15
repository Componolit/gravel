
package body Ringbuffer is

   use all type Block.Id;

   function Free (R : Cycle) return Boolean
   is
   begin
      return R.Data (R.Write).Block_Id = Block.Id'Last;
   end Free;

   function Has_Block (R : Cycle;
                       B : Block.Id) return Boolean
   is
   begin
      return (for some I of R.Data => I.Block_Id = B);
   end Has_Block;

   function Block_Ready (R : Cycle) return Boolean
   is
   begin
      return R.Data (R.Read).Set;
   end Block_Ready;

   function Block_Peek (R : Cycle) return Block.Id
   is
   begin
      return R.Data (R.Read).Block_Id;
   end Block_Peek;

   procedure Initialize (R : out Cycle;
                         B :     Buffer)
   is
   begin
      R.Read  := Index'First;
      R.Write := Index'First;
      R.Data  := (others => (Block.Id'Last, False, B));
   end Initialize;

   procedure Add (R : in out Cycle;
                  B :        Block.Id)
   is
   begin
      R.Data (R.Write).Block_Id := B;
      R.Data (R.Write).Set      := False;
      R.Write                   := R.Write + 1;
   end Add;

   procedure Set_Data (R   : in out Cycle;
                       B   :        Block.Id;
                       Buf :        Buffer)
   is
   begin
      for I in R.Data'Range loop
         if R.Data (I).Block_Id = B then
            R.Data (I).Set  := True;
            R.Data (I).Data := Buf;
            return;
         end if;
      end loop;
   end Set_Data;

   procedure Get_Block (R   : in out Cycle;
                        Buf :    out Buffer)
   is
   begin
      Buf                      := R.Data (R.Read).Data;
      R.Data (R.Read).Block_Id := Block.Id'Last;
      R.Data (R.Read).Set      := False;
      R.Read                   := R.Read + 1;
   end Get_Block;

end Ringbuffer;
