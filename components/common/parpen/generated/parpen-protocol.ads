package Parpen.Protocol with
  SPARK_Mode
is

   type Esc_Char_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Esc_Char is Esc_Char_Base range 27 .. 27;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Esc_Char return Protocol.Esc_Char is
     (Protocol.Esc_Char'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Esc_Char_Base) return Boolean is
     (Val >= 27
      and Val <= 27);

   function Convert (Val : Protocol.Esc_Char_Base) return Protocol.Esc_Char is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Connection_ID_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Connection_ID is Connection_ID_Base range 1 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Connection_ID return Protocol.Connection_ID is
     (Protocol.Connection_ID'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Connection_ID_Base) return Boolean is
     (Val >= 1);

   function Convert (Val : Protocol.Connection_ID_Base) return Protocol.Connection_ID is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Handle_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Handle is Handle_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Handle return Protocol.Handle is
     (Protocol.Handle'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Handle_Base) return Boolean is
     (True);

   function Convert (Val : Protocol.Handle_Base) return Protocol.Handle is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Method_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Method is Method_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Method return Protocol.Method is
     (Protocol.Method'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Method_Base) return Boolean is
     (True);

   function Convert (Val : Protocol.Method_Base) return Protocol.Method is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Offset_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Offset is Offset_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Offset return Protocol.Offset is
     (Protocol.Offset'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Offset_Base) return Boolean is
     (True);

   function Convert (Val : Protocol.Offset_Base) return Protocol.Offset is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Length_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Length is Length_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Length return Protocol.Length is
     (Protocol.Length'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Length_Base) return Boolean is
     (True);

   function Convert (Val : Protocol.Length_Base) return Protocol.Length is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Request_Tag_Base is mod 2**8;

   type Request_Tag is (REQUEST_TRANSACTION) with
     Size =>
       8;
   for Request_Tag use (REQUEST_TRANSACTION => 0);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Request_Tag return Protocol.Request_Tag is
     (Protocol.Request_Tag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Request_Tag_Base) return Boolean is
     ((case Val is
         when 0 =>
            True,
         when others =>
            False));

   function Convert (Enum : Protocol.Request_Tag) return Protocol.Request_Tag_Base is
     ((case Enum is
         when REQUEST_TRANSACTION =>
            0));

   function Convert (Val : Protocol.Request_Tag_Base) return Protocol.Request_Tag is
     ((case Val is
         when 0 =>
            REQUEST_TRANSACTION,
         when others =>
            Unreachable_Protocol_Request_Tag))
    with
     Pre =>
       Valid (Val);

   type Reply_Tag_Base is mod 2**8;

   type Reply_Tag is (REPLY_ERROR) with
     Size =>
       8;
   for Reply_Tag use (REPLY_ERROR => 0);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Reply_Tag return Protocol.Reply_Tag is
     (Protocol.Reply_Tag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Reply_Tag_Base) return Boolean is
     ((case Val is
         when 0 =>
            True,
         when others =>
            False));

   function Convert (Enum : Protocol.Reply_Tag) return Protocol.Reply_Tag_Base is
     ((case Enum is
         when REPLY_ERROR =>
            0));

   function Convert (Val : Protocol.Reply_Tag_Base) return Protocol.Reply_Tag is
     ((case Val is
         when 0 =>
            REPLY_ERROR,
         when others =>
            Unreachable_Protocol_Reply_Tag))
    with
     Pre =>
       Valid (Val);

   type Binder_Kind_Base is mod 2**16;

   type Binder_Kind is (BK_FD, BK_POINTER, BK_STRONG_BINDER, BK_STRONG_HANDLE, BK_WEAK_BINDER, BK_WEAK_HANDLE) with
     Size =>
       16;
   for Binder_Kind use (BK_FD => 16#6664#, BK_POINTER => 16#7074#, BK_STRONG_BINDER => 16#7362#, BK_STRONG_HANDLE => 16#7368#, BK_WEAK_BINDER => 16#7762#, BK_WEAK_HANDLE => 16#7768#);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Binder_Kind return Protocol.Binder_Kind is
     (Protocol.Binder_Kind'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Binder_Kind_Base) return Boolean is
     ((case Val is
         when 16#7362# | 16#7762# | 16#7368# | 16#7768# | 16#7074# | 16#6664# =>
            True,
         when others =>
            False));

   function Convert (Enum : Protocol.Binder_Kind) return Protocol.Binder_Kind_Base is
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

   function Convert (Val : Protocol.Binder_Kind_Base) return Protocol.Binder_Kind is
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
            Unreachable_Protocol_Binder_Kind))
    with
     Pre =>
       Valid (Val);

   type Binder_Arity_Base is mod 2**8;

   type Binder_Arity is (BA_SINGLE, BA_ARRAY) with
     Size =>
       8;
   for Binder_Arity use (BA_SINGLE => 16#2A#, BA_ARRAY => 16#61#);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Binder_Arity return Protocol.Binder_Arity is
     (Protocol.Binder_Arity'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Binder_Arity_Base) return Boolean is
     ((case Val is
         when 16#2A# | 16#61# =>
            True,
         when others =>
            False));

   function Convert (Enum : Protocol.Binder_Arity) return Protocol.Binder_Arity_Base is
     ((case Enum is
         when BA_SINGLE =>
            16#2A#,
         when BA_ARRAY =>
            16#61#));

   function Convert (Val : Protocol.Binder_Arity_Base) return Protocol.Binder_Arity is
     ((case Val is
         when 16#2A# =>
            BA_SINGLE,
         when 16#61# =>
            BA_ARRAY,
         when others =>
            Unreachable_Protocol_Binder_Arity))
    with
     Pre =>
       Valid (Val);

   type Binder_Tag_Base is range 0 .. 2**8 - 1 with
     Size =>
       8;

   subtype Binder_Tag is Binder_Tag_Base range 16#85# .. 16#85#;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Binder_Tag return Protocol.Binder_Tag is
     (Protocol.Binder_Tag'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Binder_Tag_Base) return Boolean is
     (Val >= 16#85#
      and Val <= 16#85#);

   function Convert (Val : Protocol.Binder_Tag_Base) return Protocol.Binder_Tag is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Pad32 is mod 2**32;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Pad32 return Protocol.Pad32 is
     (Protocol.Pad32'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Protocol.Pad32) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Protocol.Pad32) return Protocol.Pad32 is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Flat_Binder_Flags_Base is mod 2**32;

   type Flat_Binder_Flags is (FBF_ACCEPT_FDS, INHERIT_RT) with
     Size =>
       32;
   for Flat_Binder_Flags use (FBF_ACCEPT_FDS => 16#100#, INHERIT_RT => 16#800#);

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Flat_Binder_Flags return Protocol.Flat_Binder_Flags is
     (Protocol.Flat_Binder_Flags'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Flat_Binder_Flags_Base) return Boolean is
     ((case Val is
         when 16#100# | 16#800# =>
            True,
         when others =>
            False));

   function Convert (Enum : Protocol.Flat_Binder_Flags) return Protocol.Flat_Binder_Flags_Base is
     ((case Enum is
         when FBF_ACCEPT_FDS =>
            16#100#,
         when INHERIT_RT =>
            16#800#));

   function Convert (Val : Protocol.Flat_Binder_Flags_Base) return Protocol.Flat_Binder_Flags is
     ((case Val is
         when 16#100# =>
            FBF_ACCEPT_FDS,
         when 16#800# =>
            INHERIT_RT,
         when others =>
            Unreachable_Protocol_Flat_Binder_Flags))
    with
     Pre =>
       Valid (Val);

   type Count is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Count return Protocol.Count is
     (Protocol.Count'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Protocol.Count) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Protocol.Count) return Protocol.Count is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Pad31 is mod 2**31;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Pad31 return Protocol.Pad31 is
     (Protocol.Pad31'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Protocol.Pad31) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Protocol.Pad31) return Protocol.Pad31 is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Binder is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Binder return Protocol.Binder is
     (Protocol.Binder'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Protocol.Binder) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Protocol.Binder) return Protocol.Binder is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Index is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Index return Protocol.Index is
     (Protocol.Index'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Protocol.Index) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Protocol.Index) return Protocol.Index is
     (Val)
    with
     Pre =>
       Valid (Val);

   type Cookie is mod 2**64;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Cookie return Protocol.Cookie is
     (Protocol.Cookie'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   pragma Warnings (Off, "unused variable ""Val""");

   function Valid (Val : Protocol.Cookie) return Boolean is
     (True);

   pragma Warnings (On, "unused variable ""Val""");

   function Convert (Val : Protocol.Cookie) return Protocol.Cookie is
     (Val)
    with
     Pre =>
       Valid (Val);

end Parpen.Protocol;
