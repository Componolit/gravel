
package body Block.Server with
   SPARK_Mode
is

   Capability : Componolit.Interfaces.Types.Capability;

   procedure Set_Capability (Cap : Componolit.Interfaces.Types.Capability)
   is
   begin
      Capability := Cap;
   end Set_Capability;

   procedure Event
   is
   begin
      null;
   end Event;

   function Block_Count (S : Types.Server_Instance) return Types.Count
   is
   begin
      return 1024;
   end Block_Count;

   function Block_Size (S : Types.Server_Instance) return Types.Size
   is
   begin
      return 512;
   end Block_Size;

   function Writable (S : Types.Server_Instance) return Boolean
   is
   begin
      return False;
   end Writable;

   function Maximum_Transfer_Size (S : Types.Server_Instance) return Types.Byte_Length
   is
   begin
      return 512;
   end Maximum_Transfer_Size;

   procedure Initialize (S : Types.Server_Instance;
                         L : String;
                         B : Types.Byte_Length)
   is
   begin
      null;
   end Initialize;

   procedure Finalize (S : Types.Server_Instance)
   is
   begin
      null;
   end Finalize;

end Block.Server;
