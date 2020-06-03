with Net.RFLX_Generic_Types;
with Net.ICMP;

generic
   with package Types is new Net.RFLX_Generic_Types (<>);
package Generic_Checksum with
   SPARK_Mode
is

   function Echo_Request_Reply_Checksum (Tag             : Net.ICMP.Tag;
                                         Code            : Net.ICMP.Code_Zero;
                                         Identifier      : Net.ICMP.Identifier;
                                         Sequence_Number : Net.ICMP.Sequence_Number;
                                         Data            : Types.Bytes) return Net.ICMP.Checksum;

private

   function Add (C1 : Net.ICMP.Checksum;
                 C2 : Net.ICMP.Checksum) return Net.ICMP.Checksum;

end Generic_Checksum;
