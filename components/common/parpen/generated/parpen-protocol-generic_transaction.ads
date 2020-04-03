with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Protocol.Generic_Transaction with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Transaction);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Tag, F_Handle, F_Binder, F_Code, F_Method, F_Cookie, F_Send_Offset, F_Send_Length, F_Meta_Offset, F_Meta_Length, F_Recv_Offset, F_Recv_Length, F_Final);

   subtype Field is Virtual_Field range F_Tag .. F_Recv_Length;

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
            when F_Initial | F_Final =>
               null;
            when F_Tag =>
               Tag_Value : Parpen.Protocol.Tag_Base;
            when F_Handle =>
               Handle_Value : Parpen.Protocol.Handle_Base;
            when F_Binder =>
               Binder_Value : Parpen.Protocol.Binder;
            when F_Code =>
               Code_Value : Parpen.Protocol.Status_Base;
            when F_Method =>
               Method_Value : Parpen.Protocol.Method_Base;
            when F_Cookie =>
               Cookie_Value : Parpen.Protocol.Cookie;
            when F_Send_Offset =>
               Send_Offset_Value : Parpen.Protocol.Offset;
            when F_Send_Length =>
               Send_Length_Value : Parpen.Protocol.Length_Base;
            when F_Meta_Offset =>
               Meta_Offset_Value : Parpen.Protocol.Offset;
            when F_Meta_Length =>
               Meta_Length_Value : Parpen.Protocol.Length_Base;
            when F_Recv_Offset =>
               Recv_Offset_Value : Parpen.Protocol.Offset;
            when F_Recv_Length =>
               Recv_Length_Value : Parpen.Protocol.Length_Base;
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

   function Get_Tag (Ctx : Context) return Parpen.Protocol.Tag with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Tag);

   function Get_Handle (Ctx : Context) return Parpen.Protocol.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Handle);

   function Get_Binder (Ctx : Context) return Parpen.Protocol.Binder with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Binder);

   function Get_Code (Ctx : Context) return Parpen.Protocol.Status with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Code);

   function Get_Method (Ctx : Context) return Parpen.Protocol.Method with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Method);

   function Get_Cookie (Ctx : Context) return Parpen.Protocol.Cookie with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Cookie);

   function Get_Send_Offset (Ctx : Context) return Parpen.Protocol.Offset with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Send_Offset);

   function Get_Send_Length (Ctx : Context) return Parpen.Protocol.Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Send_Length);

   function Get_Meta_Offset (Ctx : Context) return Parpen.Protocol.Offset with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Meta_Offset);

   function Get_Meta_Length (Ctx : Context) return Parpen.Protocol.Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Meta_Length);

   function Get_Recv_Offset (Ctx : Context) return Parpen.Protocol.Offset with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Recv_Offset);

   function Get_Recv_Length (Ctx : Context) return Parpen.Protocol.Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Recv_Length);

   procedure Set_Tag (Ctx : in out Context; Val : Parpen.Protocol.Tag) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Tag)
          and then Field_Last (Ctx, F_Tag) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Tag, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Tag) >= Field_Length (Ctx, F_Tag),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Tag)
          and Get_Tag (Ctx) = Val
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Code)
          and Invalid (Ctx, F_Method)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Send_Offset)
          and Invalid (Ctx, F_Send_Length)
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (if Types.Bit_Length (Convert (Get_Tag (Ctx))) = Types.Bit_Length (Convert (T_HANDLE)) then
             Predecessor (Ctx, F_Handle) = F_Tag
               and Valid_Next (Ctx, F_Handle))
          and (if Types.Bit_Length (Convert (Get_Tag (Ctx))) = Types.Bit_Length (Convert (T_BINDER)) then
             Predecessor (Ctx, F_Binder) = F_Tag
               and Valid_Next (Ctx, F_Binder))
          and (if Types.Bit_Length (Convert (Get_Tag (Ctx))) = Types.Bit_Length (Convert (T_STATUS)) then
             Predecessor (Ctx, F_Code) = F_Tag
               and Valid_Next (Ctx, F_Code))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Tag) = Predecessor (Ctx, F_Tag)'Old
          and Valid_Next (Ctx, F_Tag) = Valid_Next (Ctx, F_Tag)'Old;

   procedure Set_Handle (Ctx : in out Context; Val : Parpen.Protocol.Handle) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Handle)
          and then Field_Last (Ctx, F_Handle) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Handle, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Handle) >= Field_Length (Ctx, F_Handle),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Handle)
          and Get_Handle (Ctx) = Val
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Code)
          and Invalid (Ctx, F_Method)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Send_Offset)
          and Invalid (Ctx, F_Send_Length)
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Method) = F_Handle
            and Valid_Next (Ctx, F_Method))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Handle) = Predecessor (Ctx, F_Handle)'Old
          and Valid_Next (Ctx, F_Handle) = Valid_Next (Ctx, F_Handle)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old;

   procedure Set_Binder (Ctx : in out Context; Val : Parpen.Protocol.Binder) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Binder)
          and then Field_Last (Ctx, F_Binder) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Binder, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Binder) >= Field_Length (Ctx, F_Binder),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Binder)
          and Get_Binder (Ctx) = Val
          and Invalid (Ctx, F_Code)
          and Invalid (Ctx, F_Method)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Send_Offset)
          and Invalid (Ctx, F_Send_Length)
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Method) = F_Binder
            and Valid_Next (Ctx, F_Method))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Binder) = Predecessor (Ctx, F_Binder)'Old
          and Valid_Next (Ctx, F_Binder) = Valid_Next (Ctx, F_Binder)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old;

   procedure Set_Code (Ctx : in out Context; Val : Parpen.Protocol.Status) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Code)
          and then Field_Last (Ctx, F_Code) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Code, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Code) >= Field_Length (Ctx, F_Code),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Code)
          and Get_Code (Ctx) = Val
          and Invalid (Ctx, F_Method)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Send_Offset)
          and Invalid (Ctx, F_Send_Length)
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Code) = Predecessor (Ctx, F_Code)'Old
          and Valid_Next (Ctx, F_Code) = Valid_Next (Ctx, F_Code)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old;

   procedure Set_Method (Ctx : in out Context; Val : Parpen.Protocol.Method) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Method)
          and then Field_Last (Ctx, F_Method) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Method, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Method) >= Field_Length (Ctx, F_Method),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Method)
          and Get_Method (Ctx) = Val
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Send_Offset)
          and Invalid (Ctx, F_Send_Length)
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Cookie) = F_Method
            and Valid_Next (Ctx, F_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Method) = Predecessor (Ctx, F_Method)'Old
          and Valid_Next (Ctx, F_Method) = Valid_Next (Ctx, F_Method)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old;

   procedure Set_Cookie (Ctx : in out Context; Val : Parpen.Protocol.Cookie) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Cookie)
          and then Field_Last (Ctx, F_Cookie) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Cookie, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Cookie) >= Field_Length (Ctx, F_Cookie),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Cookie)
          and Get_Cookie (Ctx) = Val
          and Invalid (Ctx, F_Send_Offset)
          and Invalid (Ctx, F_Send_Length)
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Send_Offset) = F_Cookie
            and Valid_Next (Ctx, F_Send_Offset))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Cookie) = Predecessor (Ctx, F_Cookie)'Old
          and Valid_Next (Ctx, F_Cookie) = Valid_Next (Ctx, F_Cookie)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Method (Ctx) = Get_Method (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old
          and Cursor (Ctx, F_Method) = Cursor (Ctx, F_Method)'Old;

   procedure Set_Send_Offset (Ctx : in out Context; Val : Parpen.Protocol.Offset) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Send_Offset)
          and then Field_Last (Ctx, F_Send_Offset) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Send_Offset, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Send_Offset) >= Field_Length (Ctx, F_Send_Offset),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Send_Offset)
          and Get_Send_Offset (Ctx) = Val
          and Invalid (Ctx, F_Send_Length)
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Send_Length) = F_Send_Offset
            and Valid_Next (Ctx, F_Send_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Send_Offset) = Predecessor (Ctx, F_Send_Offset)'Old
          and Valid_Next (Ctx, F_Send_Offset) = Valid_Next (Ctx, F_Send_Offset)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Method (Ctx) = Get_Method (Ctx)'Old
          and Get_Cookie (Ctx) = Get_Cookie (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old
          and Cursor (Ctx, F_Method) = Cursor (Ctx, F_Method)'Old
          and Cursor (Ctx, F_Cookie) = Cursor (Ctx, F_Cookie)'Old;

   procedure Set_Send_Length (Ctx : in out Context; Val : Parpen.Protocol.Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Send_Length)
          and then Field_Last (Ctx, F_Send_Length) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Send_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Send_Length) >= Field_Length (Ctx, F_Send_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Send_Length)
          and Get_Send_Length (Ctx) = Val
          and Invalid (Ctx, F_Meta_Offset)
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Meta_Offset) = F_Send_Length
            and Valid_Next (Ctx, F_Meta_Offset))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Send_Length) = Predecessor (Ctx, F_Send_Length)'Old
          and Valid_Next (Ctx, F_Send_Length) = Valid_Next (Ctx, F_Send_Length)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Method (Ctx) = Get_Method (Ctx)'Old
          and Get_Cookie (Ctx) = Get_Cookie (Ctx)'Old
          and Get_Send_Offset (Ctx) = Get_Send_Offset (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old
          and Cursor (Ctx, F_Method) = Cursor (Ctx, F_Method)'Old
          and Cursor (Ctx, F_Cookie) = Cursor (Ctx, F_Cookie)'Old
          and Cursor (Ctx, F_Send_Offset) = Cursor (Ctx, F_Send_Offset)'Old;

   procedure Set_Meta_Offset (Ctx : in out Context; Val : Parpen.Protocol.Offset) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Meta_Offset)
          and then Field_Last (Ctx, F_Meta_Offset) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Meta_Offset, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Meta_Offset) >= Field_Length (Ctx, F_Meta_Offset),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Meta_Offset)
          and Get_Meta_Offset (Ctx) = Val
          and Invalid (Ctx, F_Meta_Length)
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Meta_Length) = F_Meta_Offset
            and Valid_Next (Ctx, F_Meta_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Meta_Offset) = Predecessor (Ctx, F_Meta_Offset)'Old
          and Valid_Next (Ctx, F_Meta_Offset) = Valid_Next (Ctx, F_Meta_Offset)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Method (Ctx) = Get_Method (Ctx)'Old
          and Get_Cookie (Ctx) = Get_Cookie (Ctx)'Old
          and Get_Send_Offset (Ctx) = Get_Send_Offset (Ctx)'Old
          and Get_Send_Length (Ctx) = Get_Send_Length (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old
          and Cursor (Ctx, F_Method) = Cursor (Ctx, F_Method)'Old
          and Cursor (Ctx, F_Cookie) = Cursor (Ctx, F_Cookie)'Old
          and Cursor (Ctx, F_Send_Offset) = Cursor (Ctx, F_Send_Offset)'Old
          and Cursor (Ctx, F_Send_Length) = Cursor (Ctx, F_Send_Length)'Old;

   procedure Set_Meta_Length (Ctx : in out Context; Val : Parpen.Protocol.Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Meta_Length)
          and then Field_Last (Ctx, F_Meta_Length) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Meta_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Meta_Length) >= Field_Length (Ctx, F_Meta_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Meta_Length)
          and Get_Meta_Length (Ctx) = Val
          and Invalid (Ctx, F_Recv_Offset)
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Recv_Offset) = F_Meta_Length
            and Valid_Next (Ctx, F_Recv_Offset))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Meta_Length) = Predecessor (Ctx, F_Meta_Length)'Old
          and Valid_Next (Ctx, F_Meta_Length) = Valid_Next (Ctx, F_Meta_Length)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Method (Ctx) = Get_Method (Ctx)'Old
          and Get_Cookie (Ctx) = Get_Cookie (Ctx)'Old
          and Get_Send_Offset (Ctx) = Get_Send_Offset (Ctx)'Old
          and Get_Send_Length (Ctx) = Get_Send_Length (Ctx)'Old
          and Get_Meta_Offset (Ctx) = Get_Meta_Offset (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old
          and Cursor (Ctx, F_Method) = Cursor (Ctx, F_Method)'Old
          and Cursor (Ctx, F_Cookie) = Cursor (Ctx, F_Cookie)'Old
          and Cursor (Ctx, F_Send_Offset) = Cursor (Ctx, F_Send_Offset)'Old
          and Cursor (Ctx, F_Send_Length) = Cursor (Ctx, F_Send_Length)'Old
          and Cursor (Ctx, F_Meta_Offset) = Cursor (Ctx, F_Meta_Offset)'Old;

   procedure Set_Recv_Offset (Ctx : in out Context; Val : Parpen.Protocol.Offset) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Recv_Offset)
          and then Field_Last (Ctx, F_Recv_Offset) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Recv_Offset, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Recv_Offset) >= Field_Length (Ctx, F_Recv_Offset),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Recv_Offset)
          and Get_Recv_Offset (Ctx) = Val
          and Invalid (Ctx, F_Recv_Length)
          and (Predecessor (Ctx, F_Recv_Length) = F_Recv_Offset
            and Valid_Next (Ctx, F_Recv_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Recv_Offset) = Predecessor (Ctx, F_Recv_Offset)'Old
          and Valid_Next (Ctx, F_Recv_Offset) = Valid_Next (Ctx, F_Recv_Offset)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Method (Ctx) = Get_Method (Ctx)'Old
          and Get_Cookie (Ctx) = Get_Cookie (Ctx)'Old
          and Get_Send_Offset (Ctx) = Get_Send_Offset (Ctx)'Old
          and Get_Send_Length (Ctx) = Get_Send_Length (Ctx)'Old
          and Get_Meta_Offset (Ctx) = Get_Meta_Offset (Ctx)'Old
          and Get_Meta_Length (Ctx) = Get_Meta_Length (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old
          and Cursor (Ctx, F_Method) = Cursor (Ctx, F_Method)'Old
          and Cursor (Ctx, F_Cookie) = Cursor (Ctx, F_Cookie)'Old
          and Cursor (Ctx, F_Send_Offset) = Cursor (Ctx, F_Send_Offset)'Old
          and Cursor (Ctx, F_Send_Length) = Cursor (Ctx, F_Send_Length)'Old
          and Cursor (Ctx, F_Meta_Offset) = Cursor (Ctx, F_Meta_Offset)'Old
          and Cursor (Ctx, F_Meta_Length) = Cursor (Ctx, F_Meta_Length)'Old;

   procedure Set_Recv_Length (Ctx : in out Context; Val : Parpen.Protocol.Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Recv_Length)
          and then Field_Last (Ctx, F_Recv_Length) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Recv_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Recv_Length) >= Field_Length (Ctx, F_Recv_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Recv_Length)
          and Get_Recv_Length (Ctx) = Val
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Recv_Length) = Predecessor (Ctx, F_Recv_Length)'Old
          and Valid_Next (Ctx, F_Recv_Length) = Valid_Next (Ctx, F_Recv_Length)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Method (Ctx) = Get_Method (Ctx)'Old
          and Get_Cookie (Ctx) = Get_Cookie (Ctx)'Old
          and Get_Send_Offset (Ctx) = Get_Send_Offset (Ctx)'Old
          and Get_Send_Length (Ctx) = Get_Send_Length (Ctx)'Old
          and Get_Meta_Offset (Ctx) = Get_Meta_Offset (Ctx)'Old
          and Get_Meta_Length (Ctx) = Get_Meta_Length (Ctx)'Old
          and Get_Recv_Offset (Ctx) = Get_Recv_Offset (Ctx)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Code) = Cursor (Ctx, F_Code)'Old
          and Cursor (Ctx, F_Method) = Cursor (Ctx, F_Method)'Old
          and Cursor (Ctx, F_Cookie) = Cursor (Ctx, F_Cookie)'Old
          and Cursor (Ctx, F_Send_Offset) = Cursor (Ctx, F_Send_Offset)'Old
          and Cursor (Ctx, F_Send_Length) = Cursor (Ctx, F_Send_Length)'Old
          and Cursor (Ctx, F_Meta_Offset) = Cursor (Ctx, F_Meta_Offset)'Old
          and Cursor (Ctx, F_Meta_Length) = Cursor (Ctx, F_Meta_Length)'Old
          and Cursor (Ctx, F_Recv_Offset) = Cursor (Ctx, F_Recv_Offset)'Old;

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
         when F_Tag =>
            Valid (Val.Tag_Value),
         when F_Handle =>
            Valid (Val.Handle_Value),
         when F_Binder =>
            Valid (Val.Binder_Value),
         when F_Code =>
            Valid (Val.Code_Value),
         when F_Method =>
            Valid (Val.Method_Value),
         when F_Cookie =>
            Valid (Val.Cookie_Value),
         when F_Send_Offset =>
            Valid (Val.Send_Offset_Value),
         when F_Send_Length =>
            Valid (Val.Send_Length_Value),
         when F_Meta_Offset =>
            Valid (Val.Meta_Offset_Value),
         when F_Meta_Length =>
            Valid (Val.Meta_Length_Value),
         when F_Recv_Offset =>
            Valid (Val.Recv_Offset_Value),
         when F_Recv_Length =>
            Valid (Val.Recv_Length_Value),
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
      and then ((if Structural_Valid (Cursors (F_Handle)) then
           (Valid (Cursors (F_Tag))
               and then Cursors (F_Handle).Predecessor = F_Tag
               and then Types.Bit_Length (Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE))))
        and then (if Structural_Valid (Cursors (F_Binder)) then
           (Valid (Cursors (F_Tag))
               and then Cursors (F_Binder).Predecessor = F_Tag
               and then Types.Bit_Length (Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER))))
        and then (if Structural_Valid (Cursors (F_Code)) then
           (Valid (Cursors (F_Tag))
               and then Cursors (F_Code).Predecessor = F_Tag
               and then Types.Bit_Length (Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS))))
        and then (if Structural_Valid (Cursors (F_Method)) then
           (Valid (Cursors (F_Handle))
               and then Cursors (F_Method).Predecessor = F_Handle)
             or (Valid (Cursors (F_Binder))
               and then Cursors (F_Method).Predecessor = F_Binder))
        and then (if Structural_Valid (Cursors (F_Cookie)) then
           (Valid (Cursors (F_Method))
               and then Cursors (F_Cookie).Predecessor = F_Method))
        and then (if Structural_Valid (Cursors (F_Send_Offset)) then
           (Valid (Cursors (F_Cookie))
               and then Cursors (F_Send_Offset).Predecessor = F_Cookie))
        and then (if Structural_Valid (Cursors (F_Send_Length)) then
           (Valid (Cursors (F_Send_Offset))
               and then Cursors (F_Send_Length).Predecessor = F_Send_Offset))
        and then (if Structural_Valid (Cursors (F_Meta_Offset)) then
           (Valid (Cursors (F_Send_Length))
               and then Cursors (F_Meta_Offset).Predecessor = F_Send_Length))
        and then (if Structural_Valid (Cursors (F_Meta_Length)) then
           (Valid (Cursors (F_Meta_Offset))
               and then Cursors (F_Meta_Length).Predecessor = F_Meta_Offset))
        and then (if Structural_Valid (Cursors (F_Recv_Offset)) then
           (Valid (Cursors (F_Meta_Length))
               and then Cursors (F_Recv_Offset).Predecessor = F_Meta_Length))
        and then (if Structural_Valid (Cursors (F_Recv_Length)) then
           (Valid (Cursors (F_Recv_Offset))
               and then Cursors (F_Recv_Length).Predecessor = F_Recv_Offset)))
      and then ((if Invalid (Cursors (F_Tag)) then
           Invalid (Cursors (F_Handle)))
        and then (if Invalid (Cursors (F_Tag)) then
           Invalid (Cursors (F_Binder)))
        and then (if Invalid (Cursors (F_Tag)) then
           Invalid (Cursors (F_Code)))
        and then (if Invalid (Cursors (F_Handle))
             and then Invalid (Cursors (F_Binder)) then
           Invalid (Cursors (F_Method)))
        and then (if Invalid (Cursors (F_Method)) then
           Invalid (Cursors (F_Cookie)))
        and then (if Invalid (Cursors (F_Cookie)) then
           Invalid (Cursors (F_Send_Offset)))
        and then (if Invalid (Cursors (F_Send_Offset)) then
           Invalid (Cursors (F_Send_Length)))
        and then (if Invalid (Cursors (F_Send_Length)) then
           Invalid (Cursors (F_Meta_Offset)))
        and then (if Invalid (Cursors (F_Meta_Offset)) then
           Invalid (Cursors (F_Meta_Length)))
        and then (if Invalid (Cursors (F_Meta_Length)) then
           Invalid (Cursors (F_Recv_Offset)))
        and then (if Invalid (Cursors (F_Recv_Offset)) then
           Invalid (Cursors (F_Recv_Length))))
      and then (if Structural_Valid (Cursors (F_Tag)) then
         (Cursors (F_Tag).Last - Cursors (F_Tag).First + 1) = Parpen.Protocol.Tag_Base'Size
           and then Cursors (F_Tag).Predecessor = F_Initial
           and then Cursors (F_Tag).First = First
           and then (if Structural_Valid (Cursors (F_Handle))
                and then Types.Bit_Length (Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_HANDLE)) then
              (Cursors (F_Handle).Last - Cursors (F_Handle).First + 1) = Parpen.Protocol.Handle_Base'Size
                and then Cursors (F_Handle).Predecessor = F_Tag
                and then Cursors (F_Handle).First = (Cursors (F_Tag).Last + 1)
                and then (if Structural_Valid (Cursors (F_Method)) then
                   (Cursors (F_Method).Last - Cursors (F_Method).First + 1) = Parpen.Protocol.Method_Base'Size
                     and then Cursors (F_Method).Predecessor = F_Handle
                     and then Cursors (F_Method).First = (Cursors (F_Handle).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Cookie)) then
                        (Cursors (F_Cookie).Last - Cursors (F_Cookie).First + 1) = Parpen.Protocol.Cookie'Size
                          and then Cursors (F_Cookie).Predecessor = F_Method
                          and then Cursors (F_Cookie).First = (Cursors (F_Method).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Send_Offset)) then
                             (Cursors (F_Send_Offset).Last - Cursors (F_Send_Offset).First + 1) = Parpen.Protocol.Offset'Size
                               and then Cursors (F_Send_Offset).Predecessor = F_Cookie
                               and then Cursors (F_Send_Offset).First = (Cursors (F_Cookie).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Send_Length)) then
                                  (Cursors (F_Send_Length).Last - Cursors (F_Send_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                    and then Cursors (F_Send_Length).Predecessor = F_Send_Offset
                                    and then Cursors (F_Send_Length).First = (Cursors (F_Send_Offset).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Meta_Offset)) then
                                       (Cursors (F_Meta_Offset).Last - Cursors (F_Meta_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                         and then Cursors (F_Meta_Offset).Predecessor = F_Send_Length
                                         and then Cursors (F_Meta_Offset).First = (Cursors (F_Send_Length).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Meta_Length)) then
                                            (Cursors (F_Meta_Length).Last - Cursors (F_Meta_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                              and then Cursors (F_Meta_Length).Predecessor = F_Meta_Offset
                                              and then Cursors (F_Meta_Length).First = (Cursors (F_Meta_Offset).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Recv_Offset)) then
                                                 (Cursors (F_Recv_Offset).Last - Cursors (F_Recv_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                                   and then Cursors (F_Recv_Offset).Predecessor = F_Meta_Length
                                                   and then Cursors (F_Recv_Offset).First = (Cursors (F_Meta_Length).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Recv_Length)) then
                                                      (Cursors (F_Recv_Length).Last - Cursors (F_Recv_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                                        and then Cursors (F_Recv_Length).Predecessor = F_Recv_Offset
                                                        and then Cursors (F_Recv_Length).First = (Cursors (F_Recv_Offset).Last + 1))))))))))
           and then (if Structural_Valid (Cursors (F_Binder))
                and then Types.Bit_Length (Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_BINDER)) then
              (Cursors (F_Binder).Last - Cursors (F_Binder).First + 1) = Parpen.Protocol.Binder'Size
                and then Cursors (F_Binder).Predecessor = F_Tag
                and then Cursors (F_Binder).First = (Cursors (F_Tag).Last + 1)
                and then (if Structural_Valid (Cursors (F_Method)) then
                   (Cursors (F_Method).Last - Cursors (F_Method).First + 1) = Parpen.Protocol.Method_Base'Size
                     and then Cursors (F_Method).Predecessor = F_Binder
                     and then Cursors (F_Method).First = (Cursors (F_Binder).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Cookie)) then
                        (Cursors (F_Cookie).Last - Cursors (F_Cookie).First + 1) = Parpen.Protocol.Cookie'Size
                          and then Cursors (F_Cookie).Predecessor = F_Method
                          and then Cursors (F_Cookie).First = (Cursors (F_Method).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Send_Offset)) then
                             (Cursors (F_Send_Offset).Last - Cursors (F_Send_Offset).First + 1) = Parpen.Protocol.Offset'Size
                               and then Cursors (F_Send_Offset).Predecessor = F_Cookie
                               and then Cursors (F_Send_Offset).First = (Cursors (F_Cookie).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Send_Length)) then
                                  (Cursors (F_Send_Length).Last - Cursors (F_Send_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                    and then Cursors (F_Send_Length).Predecessor = F_Send_Offset
                                    and then Cursors (F_Send_Length).First = (Cursors (F_Send_Offset).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Meta_Offset)) then
                                       (Cursors (F_Meta_Offset).Last - Cursors (F_Meta_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                         and then Cursors (F_Meta_Offset).Predecessor = F_Send_Length
                                         and then Cursors (F_Meta_Offset).First = (Cursors (F_Send_Length).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Meta_Length)) then
                                            (Cursors (F_Meta_Length).Last - Cursors (F_Meta_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                              and then Cursors (F_Meta_Length).Predecessor = F_Meta_Offset
                                              and then Cursors (F_Meta_Length).First = (Cursors (F_Meta_Offset).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Recv_Offset)) then
                                                 (Cursors (F_Recv_Offset).Last - Cursors (F_Recv_Offset).First + 1) = Parpen.Protocol.Offset'Size
                                                   and then Cursors (F_Recv_Offset).Predecessor = F_Meta_Length
                                                   and then Cursors (F_Recv_Offset).First = (Cursors (F_Meta_Length).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Recv_Length)) then
                                                      (Cursors (F_Recv_Length).Last - Cursors (F_Recv_Length).First + 1) = Parpen.Protocol.Length_Base'Size
                                                        and then Cursors (F_Recv_Length).Predecessor = F_Recv_Offset
                                                        and then Cursors (F_Recv_Length).First = (Cursors (F_Recv_Offset).Last + 1))))))))))
           and then (if Structural_Valid (Cursors (F_Code))
                and then Types.Bit_Length (Cursors (F_Tag).Value.Tag_Value) = Types.Bit_Length (Convert (T_STATUS)) then
              (Cursors (F_Code).Last - Cursors (F_Code).First + 1) = Parpen.Protocol.Status_Base'Size
                and then Cursors (F_Code).Predecessor = F_Tag
                and then Cursors (F_Code).First = (Cursors (F_Tag).Last + 1))));

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

end Parpen.Protocol.Generic_Transaction;
