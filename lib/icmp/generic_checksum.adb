
with System.Unsigned_Types;

package body Generic_Checksum with
   SPARK_Mode
is

   package SU renames System.Unsigned_Types;

   function Shl (C : Net.ICMP.Checksum) return Net.ICMP.Checksum is
      (Net.ICMP.Checksum (SU.Shift_Left (SU.Short_Unsigned (C), 8)));

   function To_Checksum (B : Types.Byte) return Net.ICMP.Checksum is
      (Net.ICMP.Checksum'Val (Types.Byte'Pos (B)));

   function Echo_Request_Reply_Checksum (Tag             : Net.ICMP.Tag;
                                         Code            : Net.ICMP.Code_Zero;
                                         Identifier      : Net.ICMP.Identifier;
                                         Sequence_Number : Net.ICMP.Sequence_Number;
                                         Data            : Types.Bytes) return Net.ICMP.Checksum
   is
      use type Net.ICMP.Checksum;
      use type Types.Index;
      Checksum : Net.ICMP.Checksum := Shl (Net.ICMP.Checksum (Net.ICMP.To_Base (Tag)))
                                      + Net.ICMP.Checksum (Net.ICMP.To_Base (Code));
      Index    : Types.Index := Data'First;
   begin
      Checksum := Add (Checksum, Add (Net.ICMP.Checksum (Identifier), Net.ICMP.Checksum (Sequence_Number)));
      while Index < Data'Last loop
         Checksum := Add (Checksum, Shl (To_Checksum (Data (Index))) + To_Checksum (Data (Index + 1)));
         Index    := Index + 2;
      end loop;
      if Index = Data'Last then
         Checksum := Add (Checksum, Shl (To_Checksum (Data (Index))));
      end if;
      return not Checksum;
   end Echo_Request_Reply_Checksum;

   function Add (C1 : Net.ICMP.Checksum;
                 C2 : Net.ICMP.Checksum) return Net.ICMP.Checksum
   is
      use type SU.Unsigned;
      Ch32 : SU.Unsigned;
   begin
      Ch32 := SU.Unsigned (C1) + SU.Unsigned (C2);
      if Ch32 > SU.Unsigned (Net.ICMP.Checksum'Last) then
         Ch32 := Ch32 + 1;
      end if;
      return Net.ICMP.Checksum (Ch32 and SU.Unsigned (Net.ICMP.Checksum'Last));
   end Add;

end Generic_Checksum;
