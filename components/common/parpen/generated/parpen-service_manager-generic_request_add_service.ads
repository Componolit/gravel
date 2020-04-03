with Parpen.Builtin_Types;
with Parpen.Builtin_Types.Conversions;
use Parpen.Builtin_Types.Conversions;
with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Service_Manager.Generic_Request_Add_Service with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Request_Add_Service);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Len, F_Name, F_Server, F_Padding, F_Allow_Isolated, F_Dump_Flags, F_Final);

   subtype Field is Virtual_Field range F_Len .. F_Dump_Flags;

   type Field_Cursor is private with
     Default_Initial_Condition =>
       False;

   type Field_Cursors is array (Virtual_Field) of Field_Cursor;

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is private with
     Default_Initial_Condition =>
       False;

   type Field_Dependent_Value (Fld : Virtual_Field := F_Initial) is
      record
         case Fld is
            when F_Initial | F_Name | F_Server | F_Final =>
               null;
            when F_Len =>
               Len_Value : Parpen.Service_Manager.Len;
            when F_Padding =>
               Padding_Value : Parpen.Service_Manager.MBZ_7_Base;
            when F_Allow_Isolated =>
               Allow_Isolated_Value : Parpen.Builtin_Types.Boolean_Base;
            when F_Dump_Flags =>
               Dump_Flags_Value : Parpen.Service_Manager.Integer_Base;
         end case;
      end record;

   function Create return Context;

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Buffer'Last <= Types.Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Buffer = null
          and Ctx.Buffer_First = Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = Types.Bytes_Last (Buffer)'Old
          and Ctx.First = Types.First_Bit_Index (Ctx.Buffer_First)
          and Initialized (Ctx);

   procedure Initialize (Ctx : out Context; Buffer : in out Types.Bytes_Ptr; First, Last : Types.Bit_Index) with
     Pre =>
       not Ctx'Constrained
          and then Buffer /= null
          and then Buffer'Length > 0
          and then Types.Byte_Index (First) >= Buffer'First
          and then Types.Byte_Index (Last) <= Buffer'Last
          and then First <= Last
          and then Last <= Types.Bit_Index'Last / 2,
     Post =>
       Valid_Context (Ctx)
          and Buffer = null
          and Has_Buffer (Ctx)
          and Ctx.Buffer_First = Types.Bytes_First (Buffer)'Old
          and Ctx.Buffer_Last = Types.Bytes_Last (Buffer)'Old
          and Ctx.First = First
          and Ctx.Last = Last
          and Initialized (Ctx);

   function Initialized (Ctx : Context) return Boolean with
     Ghost;

   procedure Take_Buffer (Ctx : in out Context; Buffer : out Types.Bytes_Ptr) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx),
     Post =>
       Valid_Context (Ctx)
          and not Has_Buffer (Ctx)
          and Buffer /= null
          and Ctx.Buffer_First = Buffer'First
          and Ctx.Buffer_Last = Buffer'Last
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old
          and Cursors (Ctx) = Cursors (Ctx)'Old;

   function Has_Buffer (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Message_Last (Ctx : Context) return Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Structural_Valid_Message (Ctx);

   function Path_Condition (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Predecessor (Ctx, Fld);

   function Field_Condition (Ctx : Context; Val : Field_Dependent_Value) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Val.Fld in Field'Range
          and Valid_Predecessor (Ctx, Val.Fld);

   function Field_Length (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_First (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   function Field_Last (Ctx : Context; Fld : Field) return Types.Bit_Index with
     Pre =>
       Valid_Next (Ctx, Fld);

   function Predecessor (Ctx : Context; Fld : Virtual_Field) return Virtual_Field with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Predecessor (Ctx : Context; Fld : Virtual_Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Next (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Available_Space (Ctx : Context; Fld : Field) return Types.Bit_Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid_Next (Ctx, Fld);

   procedure Verify (Ctx : in out Context; Fld : Field) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   procedure Verify_Message (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx) = Has_Buffer (Ctx)'Old
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Ctx.Last = Ctx.Last'Old;

   function Present (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx),
     Post =>
       (if Valid'Result then
           Structural_Valid (Ctx, Fld)
             and Present (Ctx, Fld));

   function Incomplete (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Invalid (Ctx : Context; Fld : Field) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Structural_Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Valid_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Incomplete_Message (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx);

   function Get_Len (Ctx : Context) return Parpen.Service_Manager.Len with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Len);

   function Get_Padding (Ctx : Context) return Parpen.Service_Manager.MBZ_7 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Padding);

   function Get_Allow_Isolated (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Allow_Isolated);

   function Get_Dump_Flags (Ctx : Context) return Parpen.Service_Manager.Integer with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Dump_Flags);

   generic
      with procedure Process_Name (Name : Types.Bytes);
   procedure Get_Name (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Name);

   generic
      with procedure Process_Server (Server : Types.Bytes);
   procedure Get_Server (Ctx : Context) with
     Pre =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Present (Ctx, F_Server);

   procedure Set_Len (Ctx : in out Context; Val : Parpen.Service_Manager.Len) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Len)
          and then Field_Last (Ctx, F_Len) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Len, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Len) >= Field_Length (Ctx, F_Len),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Len)
          and Get_Len (Ctx) = Val
          and Invalid (Ctx, F_Name)
          and Invalid (Ctx, F_Server)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Name) = F_Len
            and Valid_Next (Ctx, F_Name))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Len) = Predecessor (Ctx, F_Len)'Old
          and Valid_Next (Ctx, F_Len) = Valid_Next (Ctx, F_Len)'Old;

   procedure Set_Padding (Ctx : in out Context; Val : Parpen.Service_Manager.MBZ_7) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Padding)
          and then Field_Last (Ctx, F_Padding) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Padding, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Padding) >= Field_Length (Ctx, F_Padding),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Padding)
          and Get_Padding (Ctx) = Val
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Allow_Isolated) = F_Padding
            and Valid_Next (Ctx, F_Allow_Isolated))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Padding) = Predecessor (Ctx, F_Padding)'Old
          and Valid_Next (Ctx, F_Padding) = Valid_Next (Ctx, F_Padding)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server) = Cursor (Ctx, F_Server)'Old;

   procedure Set_Allow_Isolated (Ctx : in out Context; Val : Boolean) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Allow_Isolated)
          and then Field_Last (Ctx, F_Allow_Isolated) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Allow_Isolated, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Allow_Isolated) >= Field_Length (Ctx, F_Allow_Isolated),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Allow_Isolated)
          and Get_Allow_Isolated (Ctx) = Val
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Dump_Flags) = F_Allow_Isolated
            and Valid_Next (Ctx, F_Dump_Flags))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Allow_Isolated) = Predecessor (Ctx, F_Allow_Isolated)'Old
          and Valid_Next (Ctx, F_Allow_Isolated) = Valid_Next (Ctx, F_Allow_Isolated)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Padding (Ctx) = Get_Padding (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server) = Cursor (Ctx, F_Server)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old;

   procedure Set_Dump_Flags (Ctx : in out Context; Val : Parpen.Service_Manager.Integer) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Dump_Flags)
          and then Field_Last (Ctx, F_Dump_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Dump_Flags, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Dump_Flags) >= Field_Length (Ctx, F_Dump_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Dump_Flags)
          and Get_Dump_Flags (Ctx) = Val
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Dump_Flags) = Predecessor (Ctx, F_Dump_Flags)'Old
          and Valid_Next (Ctx, F_Dump_Flags) = Valid_Next (Ctx, F_Dump_Flags)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Padding (Ctx) = Get_Padding (Ctx)'Old
          and Get_Allow_Isolated (Ctx) = Get_Allow_Isolated (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server) = Cursor (Ctx, F_Server)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Allow_Isolated) = Cursor (Ctx, F_Allow_Isolated)'Old;

   generic
      with procedure Process_Name (Name : out Types.Bytes);
   procedure Set_Name (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Name)
          and then Field_Last (Ctx, F_Name) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Name))
          and then Available_Space (Ctx, F_Name) >= Field_Length (Ctx, F_Name),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Invalid (Ctx, F_Server)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server) = F_Name
            and Valid_Next (Ctx, F_Server))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Name) = Predecessor (Ctx, F_Name)'Old
          and Valid_Next (Ctx, F_Name) = Valid_Next (Ctx, F_Name)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Structural_Valid (Ctx, F_Name);

   generic
      with procedure Process_Server (Server : out Types.Bytes);
   procedure Set_Server (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server)
          and then Field_Last (Ctx, F_Server) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Server))
          and then Available_Space (Ctx, F_Server) >= Field_Length (Ctx, F_Server),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Padding) = F_Server
            and Valid_Next (Ctx, F_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server) = Predecessor (Ctx, F_Server)'Old
          and Valid_Next (Ctx, F_Server) = Valid_Next (Ctx, F_Server)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Structural_Valid (Ctx, F_Server);

   procedure Initialize_Name (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Name)
          and then Field_Last (Ctx, F_Name) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Name))
          and then Available_Space (Ctx, F_Name) >= Field_Length (Ctx, F_Name),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Invalid (Ctx, F_Server)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server) = F_Name
            and Valid_Next (Ctx, F_Server))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Name) = Predecessor (Ctx, F_Name)'Old
          and Valid_Next (Ctx, F_Name) = Valid_Next (Ctx, F_Name)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Structural_Valid (Ctx, F_Name);

   procedure Initialize_Server (Ctx : in out Context) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server)
          and then Field_Last (Ctx, F_Server) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (Fld => F_Server))
          and then Available_Space (Ctx, F_Server) >= Field_Length (Ctx, F_Server),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Padding) = F_Server
            and Valid_Next (Ctx, F_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server) = Predecessor (Ctx, F_Server)'Old
          and Valid_Next (Ctx, F_Server) = Valid_Next (Ctx, F_Server)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Structural_Valid (Ctx, F_Server);

   function Valid_Context (Ctx : Context) return Boolean with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

   function Cursors (Ctx : Context) return Field_Cursors with
     Annotate =>
       (GNATprove, Inline_For_Proof),
     Ghost;

private

   type Cursor_State is (S_Valid, S_Structural_Valid, S_Invalid, S_Incomplete);

   function Valid_Value (Val : Field_Dependent_Value) return Boolean is
     ((case Val.Fld is
         when F_Len =>
            Valid (Val.Len_Value),
         when F_Name | F_Server =>
            True,
         when F_Padding =>
            Valid (Val.Padding_Value),
         when F_Allow_Isolated =>
            Valid (Val.Allow_Isolated_Value),
         when F_Dump_Flags =>
            Valid (Val.Dump_Flags_Value),
         when F_Initial | F_Final =>
            False));

   type Field_Cursor (State : Cursor_State := S_Invalid) is
      record
         Predecessor : Virtual_Field := F_Final;
         case State is
            when S_Valid | S_Structural_Valid =>
               First : Types.Bit_Index := Types.Bit_Index'First;
               Last : Types.Bit_Length := Types.Bit_Length'First;
               Value : Field_Dependent_Value := (Fld => F_Final);
            when S_Invalid | S_Incomplete =>
               null;
         end case;
      end record with
     Dynamic_Predicate =>
       (if State = S_Valid
             or State = S_Structural_Valid then
           Valid_Value (Field_Cursor.Value));

   function Structural_Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid
      or Cursor.State = S_Structural_Valid);

   function Valid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Valid);

   function Invalid (Cursor : Field_Cursor) return Boolean is
     (Cursor.State = S_Invalid
      or Cursor.State = S_Incomplete);

   pragma Warnings (Off, """Buffer"" is not modified, could be of access constant type");

   function Valid_Context (Buffer_First, Buffer_Last : Types.Index; First, Last : Types.Bit_Index; Buffer : Types.Bytes_Ptr; Cursors : Field_Cursors) return Boolean is
     ((if Buffer /= null then
         Buffer'First = Buffer_First
           and Buffer'Last = Buffer_Last)
      and then Types.Byte_Index (First) >= Buffer_First
      and then Types.Byte_Index (Last) <= Buffer_Last
      and then First <= Last
      and then Last <= Types.Bit_Index'Last / 2
      and then (for all F in Field'First .. Field'Last =>
        (if Structural_Valid (Cursors (F)) then
         Cursors (F).First >= First
           and Cursors (F).Last <= Last
           and Cursors (F).First <= (Cursors (F).Last + 1)
           and Cursors (F).Value.Fld = F))
      and then ((if Structural_Valid (Cursors (F_Name)) then
           (Valid (Cursors (F_Len))
               and then Cursors (F_Name).Predecessor = F_Len))
        and then (if Structural_Valid (Cursors (F_Server)) then
           (Structural_Valid (Cursors (F_Name))
               and then Cursors (F_Server).Predecessor = F_Name))
        and then (if Structural_Valid (Cursors (F_Padding)) then
           (Structural_Valid (Cursors (F_Server))
               and then Cursors (F_Padding).Predecessor = F_Server))
        and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
           (Valid (Cursors (F_Padding))
               and then Cursors (F_Allow_Isolated).Predecessor = F_Padding))
        and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
           (Valid (Cursors (F_Allow_Isolated))
               and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated)))
      and then ((if Invalid (Cursors (F_Len)) then
           Invalid (Cursors (F_Name)))
        and then (if Invalid (Cursors (F_Name)) then
           Invalid (Cursors (F_Server)))
        and then (if Invalid (Cursors (F_Server)) then
           Invalid (Cursors (F_Padding)))
        and then (if Invalid (Cursors (F_Padding)) then
           Invalid (Cursors (F_Allow_Isolated)))
        and then (if Invalid (Cursors (F_Allow_Isolated)) then
           Invalid (Cursors (F_Dump_Flags))))
      and then (if Structural_Valid (Cursors (F_Len)) then
         (Cursors (F_Len).Last - Cursors (F_Len).First + 1) = Parpen.Service_Manager.Len'Size
           and then Cursors (F_Len).Predecessor = F_Initial
           and then Cursors (F_Len).First = First
           and then (if Structural_Valid (Cursors (F_Name)) then
              (Cursors (F_Name).Last - Cursors (F_Name).First + 1) = Types.Bit_Length (Cursors (F_Len).Value.Len_Value) * 8
                and then Cursors (F_Name).Predecessor = F_Len
                and then Cursors (F_Name).First = (Cursors (F_Len).Last + 1)
                and then (if Structural_Valid (Cursors (F_Server)) then
                   (Cursors (F_Server).Last - Cursors (F_Server).First + 1) = 192
                     and then Cursors (F_Server).Predecessor = F_Name
                     and then Cursors (F_Server).First = (Cursors (F_Name).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Padding)) then
                        (Cursors (F_Padding).Last - Cursors (F_Padding).First + 1) = Parpen.Service_Manager.MBZ_7_Base'Size
                          and then Cursors (F_Padding).Predecessor = F_Server
                          and then Cursors (F_Padding).First = (Cursors (F_Server).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
                             (Cursors (F_Allow_Isolated).Last - Cursors (F_Allow_Isolated).First + 1) = Parpen.Builtin_Types.Boolean_Base'Size
                               and then Cursors (F_Allow_Isolated).Predecessor = F_Padding
                               and then Cursors (F_Allow_Isolated).First = (Cursors (F_Padding).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
                                  (Cursors (F_Dump_Flags).Last - Cursors (F_Dump_Flags).First + 1) = Parpen.Service_Manager.Integer_Base'Size
                                    and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                    and then Cursors (F_Dump_Flags).First = (Cursors (F_Allow_Isolated).Last + 1))))))));

   type Context (Buffer_First, Buffer_Last : Types.Index := Types.Index'First; First, Last : Types.Bit_Index := Types.Bit_Index'First) is
      record
         Buffer : Types.Bytes_Ptr := null;
         Cursors : Field_Cursors := (others => (State => S_Invalid, Predecessor => F_Final));
      end record with
     Dynamic_Predicate =>
       Valid_Context (Context.Buffer_First, Context.Buffer_Last, Context.First, Context.Last, Context.Buffer, Context.Cursors);

   function Valid_Context (Ctx : Context) return Boolean is
     (Valid_Context (Ctx.Buffer_First, Ctx.Buffer_Last, Ctx.First, Ctx.Last, Ctx.Buffer, Ctx.Cursors));

   function Cursor (Ctx : Context; Fld : Field) return Field_Cursor is
     (Ctx.Cursors (Fld));

   function Cursors (Ctx : Context) return Field_Cursors is
     (Ctx.Cursors);

end Parpen.Service_Manager.Generic_Request_Add_Service;
