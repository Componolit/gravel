package Parpen.Service_Manager with
  SPARK_Mode
is

   type Len is mod 2**32;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Service_Manager_Len return Service_Manager.Len is
     (Service_Manager.Len'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Service_Manager.Len) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Service_Manager.Len) return Service_Manager.Len is
     (Val)
    with
     Pre =>
       Valid (Val);

   type MBZ_7_Base is range 0 .. 2**7 - 1 with
     Size =>
       7;

   subtype MBZ_7 is MBZ_7_Base range 0 .. 0;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Service_Manager_MBZ_7 return Service_Manager.MBZ_7 is
     (Service_Manager.MBZ_7'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Service_Manager.MBZ_7_Base) return Boolean is
     (Val <= 0);

   function Convert (Val : Service_Manager.MBZ_7_Base) return Service_Manager.MBZ_7 is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Integer_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Integer is Integer_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Service_Manager_Integer return Service_Manager.Integer is
     (Service_Manager.Integer'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Service_Manager.Integer_Base) return Boolean is
     (True);

   function Convert (Val : Service_Manager.Integer_Base) return Service_Manager.Integer is
     (Val)
    with
     Pre =>
       Valid (Val);

end Parpen.Service_Manager;
