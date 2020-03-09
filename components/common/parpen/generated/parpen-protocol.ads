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

   type Descriptor_Base is range 0 .. 2**32 - 1 with
     Size =>
       32;

   subtype Descriptor is Descriptor_Base range 0 .. 2**32 - 1;

   pragma Warnings (Off, "precondition is statically false");

   function Unreachable_Protocol_Descriptor return Protocol.Descriptor is
     (Protocol.Descriptor'First)
    with
     Pre =>
       False;

   pragma Warnings (On, "precondition is statically false");

   function Valid (Val : Protocol.Descriptor_Base) return Boolean is
     (True);

   function Convert (Val : Protocol.Descriptor_Base) return Protocol.Descriptor is
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

end Parpen.Protocol;
