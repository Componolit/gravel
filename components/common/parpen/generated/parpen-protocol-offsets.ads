pragma SPARK_Mode;
with Parpen.Protocol.Generic_Offsets;
with Parpen.Types;
with Parpen.Protocol.Offset_Array;

package Parpen.Protocol.Offsets is new Parpen.Protocol.Generic_Offsets (Parpen.Types, Offset_Array);
