package body Parpen.Container with SPARK_Mode => Off
is
   Data : aliased Types.Bytes (1 .. Length);
begin
   Ptr := Data'Unrestricted_Access;
end Parpen.Container;
