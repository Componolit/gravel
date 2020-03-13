with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Generic_Types;
with Parpen.Protocol.Generic_IBinder;
with Parpen.Resolve;

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

   type Client_ID is new Natural range 1 .. 10;
   type Node_ID is new Natural;

   package Resolve is new Parpen.Resolve (Client_ID      => Client_ID,
                                          Null_Client_ID => Client_ID'Last,
                                          Node_ID        => Node_ID,
                                          Null_Node_ID   => 0,
                                          Types          => Types);

   function "&" (Left : String; Right : Natural) return String is
      (Left & (1 => Character'Val (Right)));

   procedure Test_Parse_Strong_Binder (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Input : String_Ptr :=
      new String'(
         "sb*" & 16#85#                      -- Strong binder
         & 16#00# & 16#00# & 16#01# & 16#00# -- flat_binder_flags with accept_fds set
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
         & 16#00# & 16#00# & 16#00# & 16#01# --
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
      Assert (B = 16#100000000000001#, "Invalid binder value (" & B'Img & ")");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Cookie), "Cookie invalid");
      C := IBinder_Package.Get_Cookie (Context);
      Assert (C = 16#123456789abcdef0#, "Invalid cookie value (" & C'Img & ")");
   end Test_Parse_Strong_Binder;

   procedure Test_Parse_Weak_Handle (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Input : String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#12# & 16#34# & 16#00# & 16#00# -- handle (value: 12340000)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      H       : Parpen.Protocol.Handle;
      C       : Parpen.Protocol.Cookie;
      Context : IBinder_Package.Context := IBinder_Package.Create;
      use type Parpen.Protocol.Binder_Kind;
      use type Parpen.Protocol.Handle;
      use type Parpen.Protocol.Cookie;
      use type Parpen.Protocol.Flat_Binder_Flags;
   begin
      IBinder_Package.Initialize (Context, Input);
      IBinder_Package.Verify_Message (Context);
      Assert (IBinder_Package.Valid_Message (Context), "Message invalid");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Kind), "Kind invalid");
      Assert (IBinder_Package.Get_Kind (Context) = Parpen.Protocol.BK_WEAK_HANDLE, "Weak handle expected");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Flags), "Flags invalid");
      Assert (IBinder_Package.Get_Flags (Context) = Parpen.Protocol.FBF_NONE, "FBF_NONE expected");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Handle), "Handle invalid");
      H := IBinder_Package.Get_Handle (Context);
      Assert (H = 16#12340000#, "Invalid binder value (" & H'Img & ")");

      Assert (IBinder_Package.Valid (Context, IBinder_Package.F_Cookie), "Cookie invalid");
      C := IBinder_Package.Get_Cookie (Context);
      Assert (C = 16#123456789abcdef0#, "Invalid cookie value (" & C'Img & ")");
   end Test_Parse_Weak_Handle;

   procedure Test_Resolve_Handle (T : in out Aunit.Test_Cases.Test_Case'Class)
   is
      Input : String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#12# & 16#34# & 16#00# & 16#00# -- handle (value: 12340000)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      Expected : String_Ptr :=
      new String'(
         "wb*" & 16#85#                      -- Weak binder
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
         & 16#00# & 16#00# & 16#00# & 16#01# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      use type Resolve.Result_Type;
      Result   : Resolve.Result_Type;
      Database : Resolve.Database (Num_Nodes => 200);
      Node     : Resolve.Node_Cursor_Option;
   begin
      Database.Initialize;
      --  FIXME: Add node with value 100000000000001 to NodeDB DB
      Node := Database.Find_Node (Owner => 1, Value => 16#100000000000001#);
      if not Node.Valid then
         Database.Insert_Node (Cursor => Node, Owner => 1, Value => 16#100000000000001#);
      end if;

      --  FIXME: Add source handle to DB (client 1)
      --  FIXME: Add dest handle to DB (client 2)
      --  FIXME: Resolve from client 1 to client 2
      Database.Resolve_Handle (Buffer => Input,
                               Offset => 0,
                               Source => 1,
                               Dest   => 2,
                               Result => Result);
      Assert (Result = Resolve.Result_OK, "Resolving handle unsuccessful: " & Result'Img);
      Assert (Input = Expected, "Binder not resolved correctly");
   end Test_Resolve_Handle;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parsing");
   end Name;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parse_Strong_Binder'Access, "Parse strong binder");
      Register_Routine (T, Test_Parse_Weak_Handle'Access, "Parse weak handle");
      Register_Routine (T, Test_Resolve_Handle'Access, "Resolve handle");
   end Register_Tests;

end Test_Parse;
