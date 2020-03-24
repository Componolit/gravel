with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Utils is

   procedure Hexdump (Label : String;
                      Data  : Types.Bytes);

end Utils;
