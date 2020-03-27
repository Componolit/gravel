with Parpen.Service_Manager.Generic_Request_Add_Service;
with Parpen.Service_Manager.Generic_Request_Get_Service;
with Parpen.Binder.Generic_IBinder;
with Parpen.Container;
with Parpen.NameDB;

package body Parpen.Name_Service is

   package DB is new Parpen.NameDB (Element       => Parpen.Binder.Handle,
                                    Query_Index   => Types.Index,
                                    Query_Element => Types.Byte,
                                    Query_String  => Types.Bytes);

   Name_DB : DB.Database (Num_Entries);

   package Req_AS_Package is new Parpen.Service_Manager.Generic_Request_Add_Service (Types);
   package Req_GS_Package is new Parpen.Service_Manager.Generic_Request_Get_Service (Types);
   package Binder_Buffer is new Parpen.Container (Types, 24);
   package IBinder_Package is new Parpen.Binder.Generic_IBinder (Types);

   procedure Process (Data           : in out Types.Bytes_Ptr;
                      Data_Offset    : in out Types.Bit_Length;
                      Data_Length    : in out Types.Bit_Length;
                      Offsets_Offset :    out Types.Bit_Length;
                      Offsets_Length :    out Types.Bit_Length;
                      Method         :        Parpen.Protocol.Method;
                      Cookie         :        Parpen.Protocol.Cookie;
                      Status         :    out Parpen.Name_Service.Status)
   is
      use type Parpen.Protocol.Method;
      use type Parpen.Binder.Binder_Kind;
      use type Types.Bit_Length;
      use type Types.Index;

      AS_Context : Req_AS_Package.Context := Req_AS_Package.Create;
      GS_Context : Req_GS_Package.Context := Req_GS_Package.Create;
      B_Context  : IBinder_Package.Context := IBinder_Package.Create;
      Handle     : Parpen.Binder.Handle;

      procedure Parse_Binder (Server : Types.Bytes);
      procedure Parse_Binder (Server : Types.Bytes)
      is
         Binder_Context : IBinder_Package.Context := IBinder_Package.Create;
      begin
         Binder_Buffer.Ptr.all := (others => Types.Byte'Val (0));
         Binder_Buffer.Ptr.all := Server;
         IBinder_Package.Initialize (Binder_Context, Binder_Buffer.Ptr);
         IBinder_Package.Verify_Message (Binder_Context);
         if IBinder_Package.Valid_Message (Binder_Context) then
            if IBinder_Package.Get_Kind (Binder_Context) = Parpen.Binder.BK_WEAK_HANDLE
               or IBinder_Package.Get_Kind (Binder_Context) = Parpen.Binder.BK_STRONG_HANDLE
            then
               Handle := IBinder_Package.Get_Handle (Binder_Context);
               Status := Status_Valid;
            end if;
         end if;
         IBinder_Package.Take_Buffer (Binder_Context, Binder_Buffer.Ptr);
      end Parse_Binder;

      procedure Parse_Binder is new Req_AS_Package.Get_Server (Parse_Binder);

      procedure Insert_Name (Name : Types.Bytes);
      procedure Insert_Name (Name : Types.Bytes)
      is
         DB_Status : DB.Status;
         use type DB.Status;
      begin
         Name_DB.Add (Element => Handle,
                      Query   => Name,
                      Status  => DB_Status);
         Status := (if DB_Status = DB.Status_OK
                    then Status_Valid
                    else Status_Invalid);
      end Insert_Name;

      procedure Insert_Name is new Req_AS_Package.Get_Name (Insert_Name);

      procedure Query_Name (Name : Types.Bytes);
      procedure Query_Name (Name : Types.Bytes)
      is
         DB_Result : DB.Result;
      begin
         Name_DB.Get (Query  => Name,
                      Result => DB_Result);
         if DB_Result.Valid then
            Handle := DB_Result.Element;
            Status := Status_Valid;
         else
            Status := Status_Invalid;
         end if;
      end Query_Name;

      procedure Query_Name is new Req_GS_Package.Get_Name (Query_Name);
   begin
      Status         := Status_Invalid;
      Offsets_Offset := 0;
      Offsets_Length := 0;

      --  Get service
      if Method = 1 then
         Req_GS_Package.Initialize (Ctx    => GS_Context,
                                    Buffer => Data,
                                    First  => Types.First_Bit_Index (Data'First) + Data_Offset,
                                    Last   => Types.Last_Bit_Index (Data'First) + Data_Offset + Data_Length);
         Req_GS_Package.Verify_Message (GS_Context);
         if not Req_GS_Package.Structural_Valid_Message (GS_Context) then
            Req_GS_Package.Take_Buffer (GS_Context, Data);
            return;
         end if;
         Query_Name (GS_Context);
         Req_GS_Package.Take_Buffer (GS_Context, Data);

         if Status = Status_Invalid then
            return;
         end if;

         --  Set offsets to 0
         Data.all (Data'First .. Data'First + 7) := (others => Types.Byte'Val (0));
         Offsets_Offset := 0;
         Offsets_Length := 64;

         IBinder_Package.Initialize (Ctx    => B_Context,
                                     Buffer => Data,
                                     First  => Types.First_Bit_Index (Data'First) + 64,
                                     Last   => Types.Last_Bit_Index (Data'Last));

         IBinder_Package.Set_Kind (B_Context, Parpen.Binder.BK_WEAK_HANDLE);
         IBinder_Package.Set_Arity (B_Context, Parpen.Binder.BA_SINGLE);
         IBinder_Package.Set_Tag (B_Context, 16#85#);
         IBinder_Package.Set_Flags (B_Context, Parpen.Binder.FBF_NONE);
         IBinder_Package.Set_Handle (B_Context, Handle);
         IBinder_Package.Set_Unused_Padding (B_Context, 0);
         IBinder_Package.Set_Cookie (B_Context, Parpen.Binder.Cookie'Val (Parpen.Protocol.Cookie'Pos (Cookie)));
         IBinder_Package.Take_Buffer (B_Context, Data);

         Data_Offset := 64;
         Data_Length := 24 * 8;
         Status      := Status_Valid;
         return;

      --  Add service
      elsif Method = 3 then
         Req_AS_Package.Initialize (Ctx    => AS_Context,
                                    Buffer => Data,
                                    First  => Types.First_Bit_Index (Data'First) + Data_Offset,
                                    Last   => Types.Last_Bit_Index (Data'First) + Data_Offset + Data_Length);
         Req_AS_Package.Verify_Message (AS_Context);
         if not Req_AS_Package.Structural_Valid_Message (AS_Context) then
            Req_AS_Package.Take_Buffer (AS_Context, Data);
            return;
         end if;
         Parse_Binder (AS_Context);
         if Status /= Status_Valid then
            Req_AS_Package.Take_Buffer (AS_Context, Data);
            return;
         end if;

         Insert_Name (AS_Context);
         Req_AS_Package.Take_Buffer (AS_Context, Data);
         Status := Status_Valid;

      else
         Status := Status_Invalid_Method;
      end if;

      Data_Length := 0;
      Data_Offset := 0;
   end Process;

   procedure Initialize
   is
   begin
      DB.Init (Name_DB);
   end Initialize;

begin
   Initialize;
end Parpen.Name_Service;
