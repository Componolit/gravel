with System;

package Parpen.Name_Service is

   type Database (Size : Natural) is tagged limited private;

   type Status is (Status_OK, Status_Not_Found, Status_In_Use, Status_Out_Of_Memory);

   subtype Element is Natural;

   type Result (Valid : Boolean := False) is
   record
      case Valid is
         when True =>
            Elem : Element;
         when False =>
            Stat : Status;
      end case;
   end record;

   procedure Init (DB : out Database);

   procedure Add (DB     : in out Database;
                  Elem   :        Element;  -- FIXME: Use genric type
                  Name   :        String;  --  FIXME: Use generic type 
                  Result :    out Status);

   function Exists (DB    :     Database;
                    Name  :     String) return Boolean;
   
   procedure Get (DB    :     Database;
                  Name  :     String;
                  Res   : out Result);
   
private

   type Byte is mod 2**8;
   subtype Hash_Index is Natural range 1 .. 20;
   type Hash_Type is array (Hash_Index) of Byte;

   type Internal_Element is
   record
      Valid : Boolean;
      Hash  : Hash_Type;
      Elem  : Element;
   end record;
   Null_Internal_Element : constant Internal_Element := (False, (others => 0), 0);

   type Elements is array (Natural range <>) of Internal_Element;

   type Database (Size : Natural) is tagged limited
   record
      E : Elements (1 .. Size);
   end record;

end Parpen.Name_Service;
