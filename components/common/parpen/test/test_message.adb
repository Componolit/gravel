with AUnit.Assertions; use AUnit.Assertions;
with Parpen.Generic_Types;
with Parpen.Message;
with Parpen.Protocol;

package body Test_Message is

   type String_Ptr is access all String;
   type Bit_Length is range 0 .. Natural'Last * 8;

   pragma Warnings (Off, "value not in range of type *");
   pragma Warnings (Off, "mod with zero divisor");

   package Types is new Parpen.Generic_Types (Index      => Positive,
                                              Byte       => Character,
                                              Bytes      => String,
                                              Bytes_Ptr  => String_Ptr,
                                              Length     => Natural,
                                              Bit_Length => Bit_Length);

   type Client_ID is new Natural range 11 .. 21;
   Client_1 : constant Client_ID := Client_ID'First + 1;
   Client_2 : constant Client_ID := Client_ID'Last - 1;

   package Message is new Parpen.Message (Client_ID           => Client_ID,
                                          Types               => Types,
                                          Num_Nodes           => 100,
                                          Num_Handles         => 20,
                                          Num_Name_DB_Entries => 200);

   function "&" (Left : String; Right : Natural) return String is
      (Left & (1 => Character'Val (Right)));

   procedure Test_Register_Service (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Add_Service : String_Ptr := new String'(
         ""
         & 16#00# & 16#00# & 16#00# & 16#00#
         & 16#00# & 16#00# & 16#00# & 16#40# -- Offset list with single entry (16#40# -> 64 bit offset within data)
         & 16#00# & 16#00# & 16#00# & 16#04# -- Len
         & "Test"                            -- Name
         & "wb*" & 16#85#                    -- Weak binder
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
         & 16#00# & 16#00# & 16#00# & 16#01# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
         & 16#00#                            -- Allow_Isolated: False
         & 16#00# & 16#00# & 16#00# & 16#00# -- Dump_Flags: 0
      );

      Result : Message.Result_Type;
      use type Message.Result_Type;

      procedure Send_Message (ID         : Client_ID;
                              Handle     : Parpen.Protocol.Handle;
                              Method     : Parpen.Protocol.Method;
                              Cookie     : Parpen.Protocol.Cookie;
                              Oneway     : Boolean;
                              Accept_FDs : Boolean;
                              Data       : String;
                              Length     : Natural);

      procedure Send_Message (ID         : Client_ID;
                              Handle     : Parpen.Protocol.Handle;
                              Method     : Parpen.Protocol.Method;
                              Cookie     : Parpen.Protocol.Cookie;
                              Oneway     : Boolean;
                              Accept_FDs : Boolean;
                              Data       : String;
                              Length     : Natural)
      is
         pragma Unreferenced (ID, Handle, Method, Cookie, Oneway, Accept_FDs, Data, Length);
      begin
         Assert (False, "Send called in one-way transaction");
      end Send_Message;

      procedure Dispatch is new Message.Dispatch (Send_Message);
   begin
      Message.Add_Client (ID => Client_1);
      Dispatch (Sender         => Client_1,
                Handle         => 0,
                Method         => 3,
                Cookie         => 16#dead_beef#,
                Oneway         => True,
                Accept_FDs     => False,
                Data           => Add_Service,
                Data_Offset    => 64,
                Data_Length    => Add_Service.all'Size - 64,
                Offsets_Offset => 0,
                Offsets_Length => 64,
                Result         => Result);
      Assert (Result = Message.Result_Valid, "Translating message failed: " & Result'Img);
   end Test_Register_Service;

   procedure Test_Query_Service (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Add_Service : String_Ptr := new String'(
         ""
         & 16#00# & 16#00# & 16#00# & 16#00#
         & 16#00# & 16#00# & 16#00# & 16#40# -- Offset list with single entry (16#40# -> 64 bit offset within data)
         & 16#00# & 16#00# & 16#00# & 16#04# -- Len
         & "Test"                            -- Name
         & "wb*" & 16#85#                    -- Weak binder
         & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
         & 16#01# & 16#00# & 16#00# & 16#00# -- binder (value: 100000000000001)
         & 16#00# & 16#00# & 16#00# & 16#01# --
         & 16#12# & 16#34# & 16#56# & 16#78# -- cookie (part 1)
         & 16#9A# & 16#BC# & 16#DE# & 16#F0# -- cookie (part 2)
         & 16#00#                            -- Allow_Isolated: False
         & 16#00# & 16#00# & 16#00# & 16#00# -- Dump_Flags: 0
      );

      Result : Message.Result_Type;
      use type Message.Result_Type;

      procedure No_Reply (ID         : Client_ID;
                            Handle     : Parpen.Protocol.Handle;
                            Method     : Parpen.Protocol.Method;
                            Cookie     : Parpen.Protocol.Cookie;
                            Oneway     : Boolean;
                            Accept_FDs : Boolean;
                            Data       : String;
                            Length     : Natural);

      procedure No_Reply (ID         : Client_ID;
                            Handle     : Parpen.Protocol.Handle;
                            Method     : Parpen.Protocol.Method;
                            Cookie     : Parpen.Protocol.Cookie;
                            Oneway     : Boolean;
                            Accept_FDs : Boolean;
                            Data       : String;
                            Length     : Natural)
      is
         pragma Unreferenced (ID, Handle, Method, Cookie, Oneway, Accept_FDs, Data, Length);
      begin
         Assert (False, "Send called in one-way transaction");
      end No_Reply;

      Get_Service : String_Ptr := new String'(
         ""
         & 16#00# & 16#00# & 16#00# & 16#04# -- Len
         & "Test"                            -- Name
         & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
         & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
         & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
         & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
         & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
         & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
         & 16#00# & 16#00# & 16#00# & 16#00# -- Buffer for reply
      );
      Reply_Checked : Boolean := False;

      procedure Check_Reply (ID         : Client_ID;
                             Handle     : Parpen.Protocol.Handle;
                             Method     : Parpen.Protocol.Method;
                             Cookie     : Parpen.Protocol.Cookie;
                             Oneway     : Boolean;
                             Accept_FDs : Boolean;
                             Data       : String;
                             Last       : Natural);

      procedure Check_Reply (ID         : Client_ID;
                             Handle     : Parpen.Protocol.Handle;
                             Method     : Parpen.Protocol.Method;
                             Cookie     : Parpen.Protocol.Cookie;
                             Oneway     : Boolean;
                             Accept_FDs : Boolean;
                             Data       : String;
                             Last       : Natural)
      is
         Expected : constant String_Ptr := new String'(
            ""
            & 16#00# & 16#00# & 16#00# & 16#00#
            & 16#00# & 16#00# & 16#00# & 16#00# -- Offset list with single entry (0 bit offset within data)
            & "wh*" & 16#85#                    -- Weak handle
            & 16#00# & 16#00# & 16#00# & 16#00# -- flat_binder_flags with accept_fds unset
            & 16#00# & 16#00# & 16#00# & 16#01# -- handle (value: 16#1#)
            & 16#00# & 16#00# & 16#00# & 16#00# -- padding
            & 16#00# & 16#00# & 16#BE# & 16#EF# -- cookie (part 1)
            & 16#DE# & 16#AD# & 16#C0# & 16#DE# -- cookie (part 2)
         );
         use type Parpen.Protocol.Handle;
         use type Parpen.Protocol.Method;
         use type Parpen.Protocol.Cookie;
      begin
         Assert (ID = Client_2, "Invalid client");
         Assert (Handle = 0, "Invalid handle");
         Assert (Method = 1, "Invalid method");
         Assert (Cookie = 16#beef_dead_c0de#, "Invalid cookie");
         Assert (Oneway = False, "Invalid oneway flag");
         Assert (Accept_FDs = False, "Invalid accept_fds flag");
         Assert (Data (Data'First .. Last) = Expected.all, "Invalid reply");
         Reply_Checked := True;
      end Check_Reply;

      procedure Dispatch_Add is new Message.Dispatch (No_Reply);
      procedure Dispatch_Get is new Message.Dispatch (Check_Reply);
   begin
      Message.Initialize;
      Message.Add_Client (ID => Client_1);
      Message.Add_Client (ID => Client_2);

      Dispatch_Add (Sender         => Client_1,
                    Handle         => 0,
                    Method         => 3,
                    Cookie         => 16#dead_beef#,
                    Oneway         => True,
                    Accept_FDs     => False,
                    Data           => Add_Service,
                    Data_Offset    => 64,
                    Data_Length    => Add_Service.all'Size - 64,
                    Offsets_Offset => 0,
                    Offsets_Length => 64,
                    Result         => Result);
      Assert (Result = Message.Result_Valid, "Registering service failed: " & Result'Img);

      Dispatch_Get (Sender         => Client_2,
                    Handle         => 0,
                    Method         => 1,
                    Cookie         => 16#beef_dead_c0de#,
                    Oneway         => False,
                    Accept_FDs     => False,
                    Data           => Get_Service,
                    Data_Offset    => 0,
                    Data_Length    => Get_Service.all'Size,
                    Offsets_Offset => 0,
                    Offsets_Length => 0,
                    Result         => Result);
      Assert (Result = Message.Result_Valid, "Quering service failed: " & Result'Img);
      Assert (Reply_Checked, "Reply not checked");
   end Test_Query_Service;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Message");
   end Name;

   procedure Register_Tests (T : in out Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Register_Service'Access, "Register service");
      Register_Routine (T, Test_Query_Service'Access, "Query service");
   end Register_Tests;

end Test_Message;
