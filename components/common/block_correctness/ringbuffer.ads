
with Cai.Block;

generic
   with package Block is new Cai.Block (<>);
   type Index is mod <>;
   type Buffer is private;
package Ringbuffer is

   use type Block.Id;

   type Item is record
      Block_Id : Block.Id;
      Set      : Boolean;
      Data     : Buffer;
   end record;

   type Ring is array (Index) of Item;

   type Cycle is record
      Read  : Index;
      Write : Index;
      Data  : Ring;
   end record;

   function Free (R : Cycle) return Boolean;

   function Has_Block (R : Cycle;
                       B : Block.Id) return Boolean with
      Annotate => (GNATprove, Terminating),
      Contract_Cases => ((for some I of R.Data => I.Block_Id = B) => Has_Block'Result,
                         (for all I of R.Data => I.Block_Id /= B) => not Has_Block'Result);

   function Block_Ready (R : Cycle) return Boolean;

   function Block_Peek (R : Cycle) return Block.Id with
      Pre => Block_Ready (R);

   procedure Initialize (R : out Cycle;
                         B :     Buffer);

   procedure Add (R : in out Cycle;
                  B :        Block.Id) with
      Pre => Free (R) and not Has_Block (R, B),
      Post => Has_Block (R, B);

   procedure Set_Data (R   : in out Cycle;
                       B   :        Block.Id;
                       Buf :        Buffer) with
      Pre => Has_Block (R, B);

   procedure Get_Block (R   : in out Cycle;
                        Buf :    out Buffer) with
      Pre => Block_Ready (R);

end Ringbuffer;
