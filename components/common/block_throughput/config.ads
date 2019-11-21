
with Gneiss.Block;

generic
   with package Block is new Gneiss.Block (<>);
package Config with
   SPARK_Mode
is

   procedure Parse (Data : String);

   function Initialized return Boolean;

   function Request_Size return Block.Size with
      Pre => Initialized;

   function Data_Size return Block.Byte_Length with
      Pre => Initialized;

   function Buffer_Size return Block.Byte_Length with
      Pre => Initialized;

   function Operation return Block.Request_Kind with
      Pre => Initialized;

   function Device return String with
      Pre => Initialized;

   function Failure_Reason return String;

private

   type Reason is (Not_Initialized,
                   Success,
                   Invalid_Content,
                   Invalid_Xml,
                   Open_Fail,
                   Invalid_Path,
                   Miss_Attr_Device,
                   Miss_Attr_Request_Size,
                   Miss_Attr_Operation,
                   Miss_Attr_Data_Size,
                   Miss_Attr_Buffer_Size);

   function Parse_Number (S : String) return Long_Integer;

   Ready    : Boolean            := False;
   Req_Size : Block.Size         := Block.Size'First;
   Dat_Size : Block.Byte_Length  := Block.Byte_Length'First;
   Buf_Size : Block.Byte_Length  := Block.Byte_Length'First;
   Op       : Block.Request_Kind := Block.None;
   Dev      : String (1 .. 256)  := (others => Character'First);
   F_Reason : Reason             := Not_Initialized;

end Config;
