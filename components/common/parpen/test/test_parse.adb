with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Generic_Types;
with Parpen.Protocol.Generic_IBinder;

package body Test_Parse is

   type String_Ptr is access all String;
   type Bit_Length is range 0 .. Natural'Last * 8;

   package Types is new Parpen.Generic_Types (Index      => Positive,
                                              Byte       => Character,
                                              Bytes      => String,
                                              Bytes_Ptr  => String_Ptr,
                                              Length     => Natural,
                                              Bit_Length => Bit_Length);
   package IBinder_Package is new Parpen.Protocol.Generic_IBinder (Types);

   function "&" (Left : String; Right : Natural) return String is
      (Left & (1 => Character'Val (Right)));

   procedure Test_Parse_Binder (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Input : String_Ptr :=
      new String'(
         "sb*" & 16#85#                      -- Strong binder
         & 16#00# & 16#00# & 16#01# & 16#00# -- flat_binder_flags with accept_fds set
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000000)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      B       : Parpen.Protocol.Binder;
      C       : Parpen.Protocol.Cookie;
      Context : IBinder_Package.Context := IBinder_Package.Create;
      use type Parpen.Protocol.Binder_Kind;
      use type Parpen.Protocol.Binder;
      use type Parpen.Protocol.Cookie;
      use type Parpen.Protocol.Flat_Binder_Flags;
   begin
      IBinder_Package.Initialize (Context, Input);
      IBinder_Package.Verify_Message (Context);
      Assert (IBinder_Package.Valid_Message (Context), "Message invalid");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Kind), "Kind invalid");
      Assert (IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_STRONG_BINDER, "Strong binder expected");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Flags), "Flags invalid");
      Assert (IBinder_Package.Get_Flags (Context) = Parpen.Protocol.FBF_ACCEPT_FDS, "FBF_ACCEPT_FDS not set");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Binder), "Binder invalid");
      B := IBinder_Package.Get_Binder (Context);
      Assert (B = 16#100000000000000#, "Invalid binder value (" & B'Img & ")");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Cookie), "Cookie invalid");
      C := IBinder_Package.Get_Cookie (Context);
      Assert (C = 16#123456789abcdef0#, "Invalid cookie value (" & C'Img & ")");
   end Test_Parse_Binder;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parsing");
   end Name;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parse_Binder'Access, "Parse binder");
   end Register_Tests;

end Test_Parse;
