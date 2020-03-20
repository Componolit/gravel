with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Generic_Types;
with Parpen.Message;
with Parpen.Protocol;

package body Test_Offsets is

   type String_Ptr is access all String;
   type Bit_Length is range 0 .. Natural'Last * 8;

   pragma Warnings (Off, "value not in range of type *");

   package Types is new Parpen.Generic_Types (Index      => Positive,
                                              Byte       => Character,
                                              Bytes      => String,
                                              Bytes_Ptr  => String_Ptr,
                                              Length     => Natural,
                                              Bit_Length => Bit_Length);

   type Client_ID is new Natural range 11 .. 21;
   Client_1 : constant Client_ID := Client_ID'First + 1;
   Client_2 : constant Client_ID := Client_ID'Last - 1;

   package Message is new Parpen.Message (Client_ID   => Client_ID,
                                          Types       => Types,
                                          Num_Nodes   => 100,
                                          Num_Handles => 20);

   function "&" (Left : String; Right : Natural) return String is
      (Left & (1 => Character'Val (Right)));

   procedure Test_Parse_Offset_List (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      Input : String_Ptr := new String'(
         ""
         & 16#01# & 16#00# & 16#00# & 16#00# & 16#00# & 16#00# & 16#00# & 16#01#
         & 16#00# & 16#70# & 16#00# & 16#00# & 16#00# & 16#00# & 16#50# & 16#00#
         & 16#aa# & 16#aa# & 16#aa# & 16#aa# & 16#aa# & 16#aa# & 16#aa# & 16#aa#
         & 16#01# & 16#02# & 16#03# & 16#04# & 16#05# & 16#06# & 16#07# & 16#08#
      );

      type Offsets_Array is array (Positive range <>) of Parpen.Protocol.Offset;
      Expected : constant Offsets_Array (1 .. 4) := (16#0100000000000001#, 16#0070000000005000#,
                                                     16#aaaaaaaaaaaaaaaa#, 16#0102030405060708#);
      Decoded : Offsets_Array (1 .. 20);
      Last    : Natural := 0;
      Result  : Message.Result_Type;
      use type Message.Result_Type;

      procedure Handle_Offset (O : Parpen.Protocol.Offset; Continue : out Boolean);
      procedure Handle_Offset (O : Parpen.Protocol.Offset; Continue : out Boolean) is
      begin
         Last := Last + 1;
         Decoded (Last) := O;
         Continue := True;
      end Handle_Offset;
      procedure Iterate is new Message.Offsets (Handle_Offset);
   begin
      Iterate (Input, 0, 256, Result);
      Assert (Result = Message.Result_Valid, "Decoding failed");
      Assert (Last = 4, "Invalid number of offsets");
      Assert (Decoded (Decoded'First .. Last) = Expected, "Parsed offsets mismatch");
   end Test_Parse_Offset_List;

   procedure Test_Empty_Offset_List (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Input : String_Ptr :=
      new String'(
         "Test"
         & 16#a0# & 16#0b# & 16#35# & 16#af# & 16#f1# & 16#12#
         & 16#ff# & 16#00# & 16#67# & 16#2f# & 16#e4# & 16#ee#
      );
      Expected : constant String := Input.all;
      Result   : Message.Result_Type;

      use type Message.Result_Type;
   begin
      Message.Translate (Data           => Input,
                         Data_Offset    => 0,
                         Data_Length    => Input'Length,
                         Offsets_Offset => 0,
                         Offsets_Length => 0,
                         Source_ID      => Client_1,
                         Dest_ID        => Client_2,
                         Result         => Result);
      Assert (Result = Message.Result_Valid, "Translating message failed: " & Result'Img);
      Assert (Input.all = Expected, "Message not resolved correctly");
   end Test_Empty_Offset_List;

   procedure Test_Single_Offset (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Input : String_Ptr :=
      new String'(
         ""
         & 16#00# & 16#00# & 16#00# & 16#00#
         & 16#00# & 16#00# & 16#00# & 16#30# -- Offset list with single entry (16#30# -> 48 bit offset within data)
         & 16#a0# & 16#0b# & 16#35# & 16#af# & 16#f1# & 16#12#
         & "wb*" & 16#85#                    -- Weak binder
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
         & 16#00# & 16#00# & 16#00# & 16#01# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
         & 16#ff# & 16#00# & 16#67# & 16#2f# & 16#e4# & 16#ee#
      );

      Expected : constant String_Ptr :=
      new String'(
         ""
         & 16#00# & 16#00# & 16#00# & 16#00#
         & 16#00# & 16#00# & 16#00# & 16#30# -- Offset list with single entry (16#30# -> 48 bit offset within data)
         & 16#a0# & 16#0b# & 16#35# & 16#af# & 16#f1# & 16#12# -- Unrelated data
         & "wh*" & 16#85#                    -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#01# -- handle (value: 16#1#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
         & 16#ff# & 16#00# & 16#67# & 16#2f# & 16#e4# & 16#ee#
      );

      Result : Message.Result_Type;
      use type Message.Result_Type;
   begin
      Message.Add_Client (ID => Client_1);
      Message.Add_Client (ID => Client_2);

      Message.Translate (Data           => Input,
                         Data_Offset    => 64,
                         Data_Length    => Input.all'Size - 64,
                         Offsets_Offset => 0,
                         Offsets_Length => 64,
                         Source_ID      => Client_1,
                         Dest_ID        => Client_2,
                         Result         => Result);
      Assert (Result = Message.Result_Valid, "Resolving binder unsuccessful: " & Result'Img);
      Assert (Input.all = Expected.all, "Binder not resolved correctly");
   end Test_Single_Offset;

   procedure Test_Multiple_Offsets (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Input : String_Ptr :=
      new String'(
         ""
         & 16#00# & 16#00# & 16#00# & 16#00# -- Offset list with 3 entries (#1: 16#40# -> 64 bit offset within data)
         & 16#00# & 16#00# & 16#00# & 16#40#
         & 16#00# & 16#00# & 16#00# & 16#00# -- (#2: 16#120# -> 288 bit offset within data)
         & 16#00# & 16#00# & 16#01# & 16#20#
         & 16#00# & 16#00# & 16#00# & 16#00# -- (#3: 16#200# -> 512 bit offset within data)
         & 16#00# & 16#00# & 16#02# & 16#00#

         & 16#a0# & 16#0b# & 16#35# & 16#af# -- Unrelated data
         & 16#f1# & 16#12# & 16#f1# & 16#12# -- Unrelated data

         & "wb*" & 16#85#                    -- Weak binder @ 256 bit
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
         & 16#00# & 16#00# & 16#00# & 16#01# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)

         & 16#ff# & 16#00# & 16#67# & 16#2f# -- Unrelated data

         & "wb*" & 16#85#                    -- Weak binder @ 480 bit
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#de# & 16#ad# & 16#be# & 16#ef# -- binder (value: 16#deadbeefdeadc0de#)
         & 16#de# & 16#ad# & 16#c0# & 16#de# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F1# -- cookie (part 2)

         & 16#ff# & 16#00# & 16#67# & 16#2f# -- Unrelated data

         & "wb*" & 16#85#                    -- Weak binder @ 704 bit
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#11# & 16#11# & 16#11# & 16#11# -- binder (value: 16#1111111111111111#)
         & 16#11# & 16#11# & 16#11# & 16#11# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F1# -- cookie (part 2)
      );

      Expected : constant String_Ptr :=
      new String'(
         ""
         & 16#00# & 16#00# & 16#00# & 16#00# -- Offset list with 3 entries (#1: 16#40# -> 64 bit offset within data)
         & 16#00# & 16#00# & 16#00# & 16#40#
         & 16#00# & 16#00# & 16#00# & 16#00# -- (#2: 16#120# -> 288 bit offset within data)
         & 16#00# & 16#00# & 16#01# & 16#20#
         & 16#00# & 16#00# & 16#00# & 16#00# -- (#3: 16#200# -> 512 bit offset within data)
         & 16#00# & 16#00# & 16#02# & 16#00#

         & 16#a0# & 16#0b# & 16#35# & 16#af# -- Unrelated data
         & 16#f1# & 16#12# & 16#f1# & 16#12# -- Unrelated data

         & "wh*" & 16#85#                    -- Weak binder @ 0 bit
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#01# -- handle (value: 16#1#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)

         & 16#ff# & 16#00# & 16#67# & 16#2f#

         & "wh*" & 16#85#                    -- Weak binder @ 224 bit
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#02# -- handle (value: 16#2#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F1# -- cookie (part 2)

         & 16#ff# & 16#00# & 16#67# & 16#2f#

         & "wh*" & 16#85#                    -- Weak binder @ 448bit
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#03# -- handle (value: 16#3#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F1# -- cookie (part 2)
      );

      Result : Message.Result_Type;
      use type Message.Result_Type;
   begin
      Message.Add_Client (ID => Client_1);
      Message.Add_Client (ID => Client_2);

      Message.Translate (Data           => Input,
                         Data_Offset    => 3 * 64,
                         Data_Length    => Input.all'Size - 3 * 64,
                         Offsets_Offset => 0,
                         Offsets_Length => 3 * 64,
                         Source_ID      => Client_1,
                         Dest_ID        => Client_2,
                         Result         => Result);
      Assert (Result = Message.Result_Valid, "Resolving binder unsuccessful: " & Result'Img);
      Assert (Input.all = Expected.all, "Binder not resolved correctly");
   end Test_Multiple_Offsets;


   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parsing");
   end Name;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Parse_Offset_List'Access, "Parse_Offset_List");
      Register_Routine (T, Test_Empty_Offset_List'Access, "Empty offset list");
      Register_Routine (T, Test_Single_Offset'Access, "Single offset");
      Register_Routine (T, Test_Multiple_Offsets'Access, "Multiple offsets");
   end Register_Tests;

end Test_Offsets;
