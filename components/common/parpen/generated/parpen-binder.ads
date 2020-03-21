package Parpen.Binder with
  SPARK_Mode
is

   type Binder_Kind_Base is mod 2**16;

   type Binder_Kind is (BK_FD, BK_POINTER, BK_STRONG_BINDER, BK_STRONG_HANDLE, BK_WEAK_BINDER, BK_WEAK_HANDLE) with
     Size =>
       16;
   for Binder_Kind use (BK_FD => 16#6664#, BK_POINTER => 16#7074#, BK_STRONG_BINDER => 16#7362#, BK_STRONG_HANDLE => 16#7368#, BK_WEAK_BINDER => 16#7762#, BK_WEAK_HANDLE => 16#7768#);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Binder_Kind return Binder.Binder_Kind is
     (Binder.Binder_Kind'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.Binder_Kind_Base) return Boolean is
     ((case Val is
         when 16#7362# | 16#7762# | 16#7368# | 16#7768# | 16#7074# | 16#6664# =>
            True,
         when others =>
            False));

   function Convert (Enum : Binder.Binder_Kind) return Binder.Binder_Kind_Base is
     ((case Enum is
         when BK_STRONG_BINDER =>
            16#7362#,
         when BK_WEAK_BINDER =>
            16#7762#,
         when BK_STRONG_HANDLE =>
            16#7368#,
         when BK_WEAK_HANDLE =>
            16#7768#,
         when BK_POINTER =>
            16#7074#,
         when BK_FD =>
            16#6664#));

   function Convert (Val : Binder.Binder_Kind_Base) return Binder.Binder_Kind is
     ((case Val is
         when 16#7362# =>
            BK_STRONG_BINDER,
         when 16#7762# =>
            BK_WEAK_BINDER,
         when 16#7368# =>
            BK_STRONG_HANDLE,
         when 16#7768# =>
            BK_WEAK_HANDLE,
         when 16#7074# =>
            BK_POINTER,
         when 16#6664# =>
            BK_FD,
         when others =>
            Unreachable_Binder_Binder_Kind))
    with
     Pre =>
       Valid (Val);

   type Binder_Arity_Base is mod 2**8;

   type Binder_Arity is (BA_SINGLE, BA_ARRAY) with
     Size =>
       8;
   for Binder_Arity use (BA_SINGLE => 16#2A#, BA_ARRAY => 16#61#);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Binder_Arity return Binder.Binder_Arity is
     (Binder.Binder_Arity'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.Binder_Arity_Base) return Boolean is
     ((case Val is
         when 16#2A# | 16#61# =>
            True,
         when others =>
            False));

   function Convert (Enum : Binder.Binder_Arity) return Binder.Binder_Arity_Base is
     ((case Enum is
         when BA_SINGLE =>
            16#2A#,
         when BA_ARRAY =>
            16#61#));

   function Convert (Val : Binder.Binder_Arity_Base) return Binder.Binder_Arity is
     ((case Val is
         when 16#2A# =>
            BA_SINGLE,
         when 16#61# =>
            BA_ARRAY,
         when others =>
            Unreachable_Binder_Binder_Arity))
    with
     Pre =>
       Valid (Val);

   type Binder_Tag_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Binder_Tag is Binder_Tag_Base range 16#85# .. 16#85#;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Binder_Tag return Binder.Binder_Tag is
     (Binder.Binder_Tag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.Binder_Tag_Base) return Boolean is
     (Val >= 16#85#
      and Val <= 16#85#);

   function Convert (Val : Binder.Binder_Tag_Base) return Binder.Binder_Tag is
     (Val)
    with
     Pre =>
       Valid (Val);

   type MBZ32_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype MBZ32 is MBZ32_Base range 0 .. 0;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_MBZ32 return Binder.MBZ32 is
     (Binder.MBZ32'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.MBZ32_Base) return Boolean is
     (Val <= 0);

   function Convert (Val : Binder.MBZ32_Base) return Binder.MBZ32 is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Flat_Binder_Flags_Base is mod 2**32;

   type Flat_Binder_Flags is (FBF_NONE, FBF_ACCEPT_FDS, FBF_INHERIT_RT) with
     Size =>
       32;
   for Flat_Binder_Flags use (FBF_NONE => 16#0#, FBF_ACCEPT_FDS => 16#100#, FBF_INHERIT_RT => 16#800#);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Flat_Binder_Flags return Binder.Flat_Binder_Flags is
     (Binder.Flat_Binder_Flags'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.Flat_Binder_Flags_Base) return Boolean is
     ((case Val is
         when 16#0# | 16#100# | 16#800# =>
            True,
         when others =>
            False));

   function Convert (Enum : Binder.Flat_Binder_Flags) return Binder.Flat_Binder_Flags_Base is
     ((case Enum is
         when FBF_NONE =>
            16#0#,
         when FBF_ACCEPT_FDS =>
            16#100#,
         when FBF_INHERIT_RT =>
            16#800#));

   function Convert (Val : Binder.Flat_Binder_Flags_Base) return Binder.Flat_Binder_Flags is
     ((case Val is
         when 16#0# =>
            FBF_NONE,
         when 16#100# =>
            FBF_ACCEPT_FDS,
         when 16#800# =>
            FBF_INHERIT_RT,
         when others =>
            Unreachable_Binder_Flat_Binder_Flags))
    with
     Pre =>
       Valid (Val);

   type Handle_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Handle is Handle_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Handle return Binder.Handle is
     (Binder.Handle'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.Handle_Base) return Boolean is
     (True);

   function Convert (Val : Binder.Handle_Base) return Binder.Handle is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Count is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Count return Binder.Count is
     (Binder.Count'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Binder.Count) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Binder.Count) return Binder.Count is
     (Val)
    with
     Pre =>
       Valid (Val);

   type MBZ31_Base is range 0 .. 2**31 - 1 with
     Size =>
       31;

   subtype MBZ31 is MBZ31_Base range 0 .. 0;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_MBZ31 return Binder.MBZ31 is
     (Binder.MBZ31'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.MBZ31_Base) return Boolean is
     (Val <= 0);

   function Convert (Val : Binder.MBZ31_Base) return Binder.MBZ31 is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Value is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Value return Binder.Value is
     (Binder.Value'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Binder.Value) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Binder.Value) return Binder.Value is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Index is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Index return Binder.Index is
     (Binder.Index'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Binder.Index) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Binder.Index) return Binder.Index is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Offset is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Offset return Binder.Offset is
     (Binder.Offset'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Binder.Offset) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Binder.Offset) return Binder.Offset is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Length_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Length is Length_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Length return Binder.Length is
     (Binder.Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Binder.Length_Base) return Boolean is
     (True);

   function Convert (Val : Binder.Length_Base) return Binder.Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Cookie is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Binder_Cookie return Binder.Cookie is
     (Binder.Cookie'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Binder.Cookie) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Binder.Cookie) return Binder.Cookie is
     (Val)
    with
     Pre =>
       Valid (Val);

end Parpen.Binder;
