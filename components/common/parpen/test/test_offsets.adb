with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Generic_Types;
with Parpen.Message;

package body Test_Offsets is

   type String_Ptr is access all String;
   type Bit_Length is range 0 .. Natural'Last * 8;

   package Types is new Parpen.Generic_Types (Index      => Positive,
                                              Byte       => Character,
                                              Bytes      => String,
                                              Bytes_Ptr  => String_Ptr,
                                              Length     => Natural,
                                              Bit_Length => Bit_Length);

   type Client_ID is new Natural range 11 .. 21;

   package Message is new Parpen.Message (Client_ID => Client_ID,
                                          Types     => Types);

   function "&" (Left : String; Right : Natural) return String is
      (Left & (1 => Character'Val (Right)));

   procedure Test_Empty_Offset_List (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Offsets : constant String_Ptr := new String'("");
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
      Message.Translate (Offsets => Offsets,
                         Data    => Input,
                         Result  => Result);
      Assert (Result = Message.Result_OK, "Translating message failed: " & Result'Img);
      Assert (Input.all = Expected, "Message not resolved correctly");
   end Test_Empty_Offset_List;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parsing");
   end Name;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty_Offset_List'Access, "Empty offset list");
   end Register_Tests;

end Test_Offsets;
