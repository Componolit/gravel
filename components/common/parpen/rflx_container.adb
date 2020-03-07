package body RFLX_Container with SPARK_Mode => Off
is
   Data : aliased Bytes (1 .. Length);
begin
   Ptr := Data'Unrestricted_Access;
end RFLX_Container;
