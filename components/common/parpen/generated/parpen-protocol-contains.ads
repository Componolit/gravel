pragma SPARK_Mode;
with Parpen.Protocol.Generic_Contains;
with Parpen.Types;
with Parpen.Protocol.Request;
with Parpen.Protocol.Transaction;

package Parpen.Protocol.Contains is new Parpen.Protocol.Generic_Contains (Parpen.Types, Parpen.Protocol.Request, Parpen.Protocol.Transaction);
