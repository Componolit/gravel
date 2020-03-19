with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Generic_Types;
with Parpen.Message;

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
         "" & 16#a0# & 16#0b# & 16#35# & 16#af# & 16#f1# & 16#12#
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
         & 16#00# & 16#00# & 16#00# & 16#30#
         & 16#a0# & 16#0b# & 16#35# & 16#af# & 16#f1# & 16#12#
         & "wh*" & 16#85#                    -- Weak handle
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#00# & 16#00# & 16#00# & 16#12# -- handle (value: 16#12#)
         & 16#00# & 16#00# & 16#00# & 16#00# -- padding
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
         & 16#ff# & 16#00# & 16#67# & 16#2f# & 16#e4# & 16#ee#
      );

      Result : Message.Result_Type;
      use type Message.Result_Type;
   begin
      Assert (Input.all /= Expected.all, "Binder do not differ");
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

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parsing");
   end Name;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty_Offset_List'Access, "Empty offset list");
      Register_Routine (T, Test_Single_Offset'Access, "Single offset");
   end Register_Tests;

end Test_Offsets;
