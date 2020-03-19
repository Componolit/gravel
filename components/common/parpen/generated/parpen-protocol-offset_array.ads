pragma SPARK_Mode;
with Parpen.Scalar_Sequence;
with Parpen.Protocol;
with Parpen.Types;

package Parpen.Protocol.Offset_Array is new Parpen.Scalar_Sequence (Parpen.Types, Offset, Offset, Valid, Convert, Convert);
