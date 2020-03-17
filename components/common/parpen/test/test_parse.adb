with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Generic_Types;
with Parpen.Protocol.Generic_IBinder;
with Parpen.Resolve;

package body Test_Parse is

   type String_Ptr is access all String;
   type Bit_Length is range 0 .. Natural'Last * 8;

   pragma Warnings (Off, "value not in range of type *");

   package Types is new Parpen.Generic_Types (Index      => Positive,
                                              Byte       => Character,
                                              Bytes      => String,
                                              Bytes_Ptr  => String_Ptr,
                                              Length     => Natural,
                                              Bit_Length => Bit_Length);
   package IBinder_Package is new Parpen.Protocol.Generic_IBinder (Types);

   type Client_ID is new Natural range 11 .. 21;
   Client_1 : constant Client_ID := Client_ID'First + 1;
   Client_2 : constant Client_ID := Client_ID'Last - 1;
   Client_3 : constant Client_ID := Client_ID'Last - 2;

   type Node_ID is new Natural range 7 .. 51;
   type Handle_ID is new Natural range 18 .. 47;

   package Resolve is new Parpen.Resolve (Client_ID      => Client_ID,
                                          Null_Client_ID => Client_ID'Last,
                                          Node_ID        => Node_ID,
                                          Null_Node_ID   => Node_ID'Last,
                                          Handle_ID      => Handle_ID,
                                          Types          => Types);

   function "&" (Left : String; Right : Natural) return String is
      (Left & (1 => Character'Val (Right)));

   procedure Test_Parse_Strong_Binder (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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

   procedure Test_Parse_Weak_Handle (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
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

   procedure Test_Parse_Weak_Handle_With_Offset (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input : String_Ptr :=
      new String'(
         "ABC"                               -- Unrelated data (offset: 24 bit)
         & "wh*" & 16#85#                    -- Weak handle
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
      IBinder_Package.Initialize (Context,
                                  Input,
                                  Types.First_Bit_Index (Input'First + 3),
                                  Types.Last_Bit_Index (Input'Last));
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
   end Test_Parse_Weak_Handle_With_Offset;

   procedure Test_Resolve_Invalid_Source (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input    : String_Ptr := new String'("Dummy");
      Result   : Resolve.Result_Type;
      Database : Resolve.Database;
      use type Resolve.Result_Type;
   begin
      Database.Initialize;
      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_1,
                        Dest_ID   => Client_2,
                        Result    => Result);
      Assert (Result = Resolve.Result_Invalid_Source, "Invalid source not detected");
   end Test_Resolve_Invalid_Source;


   procedure Test_Resolve_Invalid_Dest (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input    : String_Ptr := new String'("Dummy");
      Result   : Resolve.Result_Type;
      Database : Resolve.Database;
      use type Resolve.Result_Type;
   begin
      Database.Initialize;
      Database.Add_Client (ID => Client_1);
      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_1,
                        Dest_ID   => Client_2,
                        Result    => Result);
      Assert (Result = Resolve.Result_Invalid_Destination, "Invalid destination not detected");
   end Test_Resolve_Invalid_Dest;


   procedure Test_Resolve_Invalid_Node (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input : String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#12# & 16#12# -- handle (value: 16#1212#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      use type Resolve.Result_Type;
      Result   : Resolve.Result_Type;
      Database : Resolve.Database;
   begin
      Database.Initialize;
      Database.Add_Client (ID => Client_1);
      Database.Add_Client (ID => Client_2);
      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_2,
                        Dest_ID   => Client_1,
                        Result    => Result);
      Assert (Result = Resolve.Result_Invalid_Handle, "Invalid node not detected: " & Result'Img);
   end Test_Resolve_Invalid_Node;


   procedure Test_Resolve_Missing_Handle (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input : String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#12# -- handle (value: 16#12#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      use type Resolve.Result_Type;
      Result   : Resolve.Result_Type;
      Database : Resolve.Database;
   begin
      Database.Initialize;
      Database.Add_Client (ID => Client_1);
      Database.Add_Client (ID => Client_2);
      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_2,
                        Dest_ID   => Client_1,
                        Result    => Result);
      Assert (Result = Resolve.Result_Handle_Not_Found, "Missing node not detected: " & Result'Img);
   end Test_Resolve_Missing_Handle;


   procedure Test_Resolve_Missing_Node (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input : String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#12# -- handle (value: 16#12#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      use type Resolve.Result_Type;
      Result   : Resolve.Result_Type;
      Database : Resolve.Database;
      D2       : Resolve.Database;
      Node     : Resolve.Node_Option;
   begin
      D2.Initialize;
      Node := D2.Get_Node (Owner => Client_1, Value => 16#1#);
      D2.Add_Node (Cursor => Node, Owner => Client_1, Value => 16#1#);

      Database.Initialize;
      Database.Add_Client (ID => Client_1);
      Database.Add_Client (ID => Client_2);

      --  The Handle_ID type starts at 18 (16#12#), hence the first entry matches the node ID encoded above
      Database.Add_Handle (ID => Client_2, Node => Node);

      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_2,
                        Dest_ID   => Client_1,
                        Result    => Result);
      Assert (Result = Resolve.Result_Node_Not_Found, "Missing node not detected");
   end Test_Resolve_Missing_Node;


   procedure Test_Resolve_Handle_To_Binder (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input : String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#12# -- handle (value: 16#12#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      Expected : constant String_Ptr :=
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
      Database : Resolve.Database;
      Node     : Resolve.Node_Option;
   begin
      Assert (Input.all /= Expected.all, "Binder do not differ");
      Database.Initialize;
      Node := Database.Get_Node (Owner => Client_1, Value => 16#100000000000001#);
      Assert (not Node.Found, "Node already present");
      Database.Add_Node (Cursor => Node, Owner => Client_1, Value => 16#100000000000001#);

      Database.Add_Client (ID => Client_1);
      Database.Add_Client (ID => Client_2);

      --  The Handle_ID type starts at 18 (16#12#), hence the first entry matches the node ID encoded above
      Database.Add_Handle (ID => Client_2, Node => Node);

      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_2,
                        Dest_ID   => Client_1,
                        Result    => Result);
      Assert (Result = Resolve.Result_OK, "Resolving handle unsuccessful: " & Result'Img);
      Assert (Input.all = Expected.all, "Binder not resolved correctly");
   end Test_Resolve_Handle_To_Binder;


   procedure Test_Resolve_Binder_To_Handle (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Input : String_Ptr :=
      new String'(
         "wb*" & 16#85#                      -- Weak binder
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
         & 16#00# & 16#00# & 16#00# & 16#01# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      Expected : constant String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#12# -- handle (value: 16#12#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      use type Resolve.Result_Type;
      Result   : Resolve.Result_Type;
      Database : Resolve.Database;
   begin
      Assert (Input.all /= Expected.all, "Binder do not differ");
      Database.Initialize;

      Database.Add_Client (ID => Client_1);
      Database.Add_Client (ID => Client_2);

      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_1,
                        Dest_ID   => Client_2,
                        Result    => Result);
      Assert (Result = Resolve.Result_OK, "Resolving handle unsuccessful: " & Result'Img);
      Assert (Input.all = Expected.all, "Binder not resolved correctly");
   end Test_Resolve_Binder_To_Handle;

   procedure Test_Resolve_Handle_To_Handle (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input : String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#12# -- handle (value: 16#12#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      Expected : constant String_Ptr :=
      new String'(
         "wh*" & 16#85#                      -- Weak binder
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#13# -- handle (value: 16#13#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
      );

      use type Resolve.Result_Type;
      Result     : Resolve.Result_Type;
      Database   : Resolve.Database;
      Node       : Resolve.Node_Option;
      Other_Node : Resolve.Node_Option;
   begin
      Assert (Input.all /= Expected.all, "Binder do not differ");
      Database.Initialize;

      Database.Add_Client (ID => Client_1);
      Database.Add_Client (ID => Client_2);
      Database.Add_Client (ID => Client_3);

      Node := Database.Get_Node (Owner => Client_3, Value => 16#100000000000001#);
      Assert (not Node.Found, "Node already present");
      Database.Add_Node (Cursor => Node, Owner => Client_3, Value => 16#100000000000001#);
      Database.Add_Handle (ID => Client_1, Node => Node);

      Other_Node := Database.Get_Node (Owner => Client_2, Value => 16#100000000000123#);
      Database.Add_Node (Cursor => Other_Node, Owner => Client_2, Value => 16#100000000000123#);
      Database.Add_Handle (ID => Client_2, Node => Other_Node);

      Database.Resolve (Buffer    => Input,
                        Offset    => 0,
                        Source_ID => Client_1,
                        Dest_ID   => Client_2,
                        Result    => Result);
      Assert (Result = Resolve.Result_OK, "Resolving handle unsuccessful: " & Result'Img);
      Assert (Input.all = Expected.all, "Binder not resolved correctly");
   end Test_Resolve_Handle_To_Handle;

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
      Register_Routine (T, Test_Parse_Weak_Handle_With_Offset'Access, "Parse weak handle with offset");
      Register_Routine (T, Test_Resolve_Invalid_Source'Access, "Resolve invalid source");
      Register_Routine (T, Test_Resolve_Invalid_Dest'Access, "Resolve invalid destination");
      Register_Routine (T, Test_Resolve_Invalid_Node'Access, "Resolve invalid node");
      Register_Routine (T, Test_Resolve_Missing_Node'Access, "Resolve missing node");
      Register_Routine (T, Test_Resolve_Handle_To_Binder'Access, "Resolve handle to binder");
      Register_Routine (T, Test_Resolve_Binder_To_Handle'Access, "Resolve binder to handle");
      Register_Routine (T, Test_Resolve_Handle_To_Handle'Access, "Resolve handle to handle");
   end Register_Tests;

end Test_Parse;
