with Parpen.Builtin_Types;
with Parpen.Builtin_Types.Conversions;
use Parpen.Builtin_Types.Conversions;
with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Service_Manager.Generic_Reply_Get_Service with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Reply_Get_Service);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Result_Kind, F_Result_Arity, F_Result_Tag, F_Result_Legacy_Flags, F_Result_Has_Parent, F_Result_Flags, F_Result_FD, F_Result_Num_FDs, F_Result_Padding, F_Result_Binder, F_Result_Handle, F_Result_Parent, F_Result_Buffer, F_Result_Unused_Padding, F_Result_Parent_Offset, F_Result_Length, F_Result_Cookie, F_Result_Index, F_Final);

   subtype Field is Virtual_Field range F_Result_Kind .. F_Result_Index;

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
            when F_Result_Kind =>
               Result_Kind_Value : Binder.Binder_Kind_Base;
            when F_Result_Arity =>
               Result_Arity_Value : Binder.Binder_Arity_Base;
            when F_Result_Tag =>
               Result_Tag_Value : Binder.Binder_Tag_Base;
            when F_Result_Legacy_Flags =>
               Result_Legacy_Flags_Value : Binder.MBZ32_Base;
            when F_Result_Has_Parent =>
               Result_Has_Parent_Value : Builtin_Types.Boolean_Base;
            when F_Result_Flags =>
               Result_Flags_Value : Binder.Flat_Binder_Flags_Base;
            when F_Result_FD =>
               Result_FD_Value : Binder.Handle_Base;
            when F_Result_Num_FDs =>
               Result_Num_FDs_Value : Binder.Count;
            when F_Result_Padding =>
               Result_Padding_Value : Binder.MBZ31_Base;
            when F_Result_Binder =>
               Result_Binder_Value : Binder.Value;
            when F_Result_Handle =>
               Result_Handle_Value : Binder.Handle_Base;
            when F_Result_Parent =>
               Result_Parent_Value : Binder.Index;
            when F_Result_Buffer =>
               Result_Buffer_Value : Binder.Index;
            when F_Result_Unused_Padding =>
               Result_Unused_Padding_Value : Binder.MBZ32_Base;
            when F_Result_Parent_Offset =>
               Result_Parent_Offset_Value : Binder.Offset;
            when F_Result_Length =>
               Result_Length_Value : Binder.Length_Base;
            when F_Result_Cookie =>
               Result_Cookie_Value : Binder.Cookie;
            when F_Result_Index =>
               Result_Index_Value : Binder.Index;
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

   function Get_Result_Kind (Ctx : Context) return Binder.Binder_Kind with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Kind);

   function Get_Result_Arity (Ctx : Context) return Binder.Binder_Arity with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Arity);

   function Get_Result_Tag (Ctx : Context) return Binder.Binder_Tag with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Tag);

   function Get_Result_Legacy_Flags (Ctx : Context) return Binder.MBZ32 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Legacy_Flags);

   function Get_Result_Has_Parent (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Has_Parent);

   function Get_Result_Flags (Ctx : Context) return Binder.Flat_Binder_Flags with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Flags);

   function Get_Result_FD (Ctx : Context) return Binder.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_FD);

   function Get_Result_Num_FDs (Ctx : Context) return Binder.Count with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Num_FDs);

   function Get_Result_Padding (Ctx : Context) return Binder.MBZ31 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Padding);

   function Get_Result_Binder (Ctx : Context) return Binder.Value with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Binder);

   function Get_Result_Handle (Ctx : Context) return Binder.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Handle);

   function Get_Result_Parent (Ctx : Context) return Binder.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Parent);

   function Get_Result_Buffer (Ctx : Context) return Binder.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Buffer);

   function Get_Result_Unused_Padding (Ctx : Context) return Binder.MBZ32 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Unused_Padding);

   function Get_Result_Parent_Offset (Ctx : Context) return Binder.Offset with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Parent_Offset);

   function Get_Result_Length (Ctx : Context) return Binder.Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Length);

   function Get_Result_Cookie (Ctx : Context) return Binder.Cookie with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Cookie);

   function Get_Result_Index (Ctx : Context) return Binder.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Result_Index);

   procedure Set_Result_Kind (Ctx : in out Context; Val : Binder.Binder_Kind) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Kind)
          and then Field_Last (Ctx, F_Result_Kind) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Kind, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Result_Kind) >= Field_Length (Ctx, F_Result_Kind),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Kind)
          and Get_Result_Kind (Ctx) = Val
          and Invalid (Ctx, F_Result_Arity)
          and Invalid (Ctx, F_Result_Tag)
          and Invalid (Ctx, F_Result_Legacy_Flags)
          and Invalid (Ctx, F_Result_Has_Parent)
          and Invalid (Ctx, F_Result_Flags)
          and Invalid (Ctx, F_Result_FD)
          and Invalid (Ctx, F_Result_Num_FDs)
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Arity) = F_Result_Kind
            and Valid_Next (Ctx, F_Result_Arity))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Kind) = Predecessor (Ctx, F_Result_Kind)'Old
          and Valid_Next (Ctx, F_Result_Kind) = Valid_Next (Ctx, F_Result_Kind)'Old;

   procedure Set_Result_Arity (Ctx : in out Context; Val : Binder.Binder_Arity) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Arity)
          and then Field_Last (Ctx, F_Result_Arity) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Arity, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Result_Arity) >= Field_Length (Ctx, F_Result_Arity),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Arity)
          and Get_Result_Arity (Ctx) = Val
          and Invalid (Ctx, F_Result_Tag)
          and Invalid (Ctx, F_Result_Legacy_Flags)
          and Invalid (Ctx, F_Result_Has_Parent)
          and Invalid (Ctx, F_Result_Flags)
          and Invalid (Ctx, F_Result_FD)
          and Invalid (Ctx, F_Result_Num_FDs)
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (if Types.Bit_Length (Convert (Get_Result_Arity (Ctx))) = Types.Bit_Length (Convert (BA_SINGLE))
               or (Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) = Types.Bit_Length (Convert (BK_FD))
                 and Types.Bit_Length (Convert (Get_Result_Arity (Ctx))) = Types.Bit_Length (Convert (BA_ARRAY))) then
             Predecessor (Ctx, F_Result_Tag) = F_Result_Arity
               and Valid_Next (Ctx, F_Result_Tag))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Arity) = Predecessor (Ctx, F_Result_Arity)'Old
          and Valid_Next (Ctx, F_Result_Arity) = Valid_Next (Ctx, F_Result_Arity)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old;

   procedure Set_Result_Tag (Ctx : in out Context; Val : Binder.Binder_Tag) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Tag)
          and then Field_Last (Ctx, F_Result_Tag) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Tag, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Tag) >= Field_Length (Ctx, F_Result_Tag),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Tag)
          and Get_Result_Tag (Ctx) = Val
          and Invalid (Ctx, F_Result_Legacy_Flags)
          and Invalid (Ctx, F_Result_Has_Parent)
          and Invalid (Ctx, F_Result_Flags)
          and Invalid (Ctx, F_Result_FD)
          and Invalid (Ctx, F_Result_Num_FDs)
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (if Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) = Types.Bit_Length (Convert (BK_FD)) then
             Predecessor (Ctx, F_Result_Legacy_Flags) = F_Result_Tag
               and Valid_Next (Ctx, F_Result_Legacy_Flags))
          and (if Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) = Types.Bit_Length (Convert (BK_POINTER)) then
             Predecessor (Ctx, F_Result_Has_Parent) = F_Result_Tag
               and Valid_Next (Ctx, F_Result_Has_Parent))
          and (if Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) /= Types.Bit_Length (Convert (BK_POINTER))
               and Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) /= Types.Bit_Length (Convert (BK_FD)) then
             Predecessor (Ctx, F_Result_Flags) = F_Result_Tag
               and Valid_Next (Ctx, F_Result_Flags))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Tag) = Predecessor (Ctx, F_Result_Tag)'Old
          and Valid_Next (Ctx, F_Result_Tag) = Valid_Next (Ctx, F_Result_Tag)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old;

   procedure Set_Result_Legacy_Flags (Ctx : in out Context; Val : Binder.MBZ32) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Legacy_Flags)
          and then Field_Last (Ctx, F_Result_Legacy_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Legacy_Flags, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Legacy_Flags) >= Field_Length (Ctx, F_Result_Legacy_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Legacy_Flags)
          and Get_Result_Legacy_Flags (Ctx) = Val
          and Invalid (Ctx, F_Result_Has_Parent)
          and Invalid (Ctx, F_Result_Flags)
          and Invalid (Ctx, F_Result_FD)
          and Invalid (Ctx, F_Result_Num_FDs)
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (if Types.Bit_Length (Convert (Get_Result_Arity (Ctx))) = Types.Bit_Length (Convert (BA_SINGLE)) then
             Predecessor (Ctx, F_Result_FD) = F_Result_Legacy_Flags
               and Valid_Next (Ctx, F_Result_FD))
          and (if Types.Bit_Length (Convert (Get_Result_Arity (Ctx))) = Types.Bit_Length (Convert (BA_ARRAY)) then
             Predecessor (Ctx, F_Result_Num_FDs) = F_Result_Legacy_Flags
               and Valid_Next (Ctx, F_Result_Num_FDs))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Legacy_Flags) = Predecessor (Ctx, F_Result_Legacy_Flags)'Old
          and Valid_Next (Ctx, F_Result_Legacy_Flags) = Valid_Next (Ctx, F_Result_Legacy_Flags)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old;

   procedure Set_Result_Has_Parent (Ctx : in out Context; Val : Boolean) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Has_Parent)
          and then Field_Last (Ctx, F_Result_Has_Parent) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Has_Parent, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Result_Has_Parent) >= Field_Length (Ctx, F_Result_Has_Parent),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Has_Parent)
          and Get_Result_Has_Parent (Ctx) = Val
          and Invalid (Ctx, F_Result_Flags)
          and Invalid (Ctx, F_Result_FD)
          and Invalid (Ctx, F_Result_Num_FDs)
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Padding) = F_Result_Has_Parent
            and Valid_Next (Ctx, F_Result_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Has_Parent) = Predecessor (Ctx, F_Result_Has_Parent)'Old
          and Valid_Next (Ctx, F_Result_Has_Parent) = Valid_Next (Ctx, F_Result_Has_Parent)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old;

   procedure Set_Result_Flags (Ctx : in out Context; Val : Binder.Flat_Binder_Flags) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Flags)
          and then Field_Last (Ctx, F_Result_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Flags, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Result_Flags) >= Field_Length (Ctx, F_Result_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Flags)
          and Get_Result_Flags (Ctx) = Val
          and Invalid (Ctx, F_Result_FD)
          and Invalid (Ctx, F_Result_Num_FDs)
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (if Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
             Predecessor (Ctx, F_Result_Binder) = F_Result_Flags
               and Valid_Next (Ctx, F_Result_Binder))
          and (if Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Convert (Get_Result_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
             Predecessor (Ctx, F_Result_Handle) = F_Result_Flags
               and Valid_Next (Ctx, F_Result_Handle))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Flags) = Predecessor (Ctx, F_Result_Flags)'Old
          and Valid_Next (Ctx, F_Result_Flags) = Valid_Next (Ctx, F_Result_Flags)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old;

   procedure Set_Result_FD (Ctx : in out Context; Val : Binder.Handle) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_FD)
          and then Field_Last (Ctx, F_Result_FD) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_FD, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_FD) >= Field_Length (Ctx, F_Result_FD),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_FD)
          and Get_Result_FD (Ctx) = Val
          and Invalid (Ctx, F_Result_Num_FDs)
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Cookie) = F_Result_FD
            and Valid_Next (Ctx, F_Result_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_FD) = Predecessor (Ctx, F_Result_FD)'Old
          and Valid_Next (Ctx, F_Result_FD) = Valid_Next (Ctx, F_Result_FD)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Legacy_Flags (Ctx) = Get_Result_Legacy_Flags (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old;

   procedure Set_Result_Num_FDs (Ctx : in out Context; Val : Binder.Count) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Num_FDs)
          and then Field_Last (Ctx, F_Result_Num_FDs) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Num_FDs, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Num_FDs) >= Field_Length (Ctx, F_Result_Num_FDs),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Num_FDs)
          and Get_Result_Num_FDs (Ctx) = Val
          and Invalid (Ctx, F_Result_Padding)
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Parent) = F_Result_Num_FDs
            and Valid_Next (Ctx, F_Result_Parent))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Num_FDs) = Predecessor (Ctx, F_Result_Num_FDs)'Old
          and Valid_Next (Ctx, F_Result_Num_FDs) = Valid_Next (Ctx, F_Result_Num_FDs)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Legacy_Flags (Ctx) = Get_Result_Legacy_Flags (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old;

   procedure Set_Result_Padding (Ctx : in out Context; Val : Binder.MBZ31) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Padding)
          and then Field_Last (Ctx, F_Result_Padding) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Padding, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Padding) >= Field_Length (Ctx, F_Result_Padding),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Padding)
          and Get_Result_Padding (Ctx) = Val
          and Invalid (Ctx, F_Result_Binder)
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Buffer) = F_Result_Padding
            and Valid_Next (Ctx, F_Result_Buffer))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Padding) = Predecessor (Ctx, F_Result_Padding)'Old
          and Valid_Next (Ctx, F_Result_Padding) = Valid_Next (Ctx, F_Result_Padding)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Has_Parent (Ctx) = Get_Result_Has_Parent (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old;

   procedure Set_Result_Binder (Ctx : in out Context; Val : Binder.Value) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Binder)
          and then Field_Last (Ctx, F_Result_Binder) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Binder, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Binder) >= Field_Length (Ctx, F_Result_Binder),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Binder)
          and Get_Result_Binder (Ctx) = Val
          and Invalid (Ctx, F_Result_Handle)
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Cookie) = F_Result_Binder
            and Valid_Next (Ctx, F_Result_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Binder) = Predecessor (Ctx, F_Result_Binder)'Old
          and Valid_Next (Ctx, F_Result_Binder) = Valid_Next (Ctx, F_Result_Binder)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Flags (Ctx) = Get_Result_Flags (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old;

   procedure Set_Result_Handle (Ctx : in out Context; Val : Binder.Handle) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Handle)
          and then Field_Last (Ctx, F_Result_Handle) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Handle, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Handle) >= Field_Length (Ctx, F_Result_Handle),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Handle)
          and Get_Result_Handle (Ctx) = Val
          and Invalid (Ctx, F_Result_Parent)
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Unused_Padding) = F_Result_Handle
            and Valid_Next (Ctx, F_Result_Unused_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Handle) = Predecessor (Ctx, F_Result_Handle)'Old
          and Valid_Next (Ctx, F_Result_Handle) = Valid_Next (Ctx, F_Result_Handle)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Flags (Ctx) = Get_Result_Flags (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old;

   procedure Set_Result_Parent (Ctx : in out Context; Val : Binder.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Parent)
          and then Field_Last (Ctx, F_Result_Parent) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Parent, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Parent) >= Field_Length (Ctx, F_Result_Parent),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Parent)
          and Get_Result_Parent (Ctx) = Val
          and Invalid (Ctx, F_Result_Buffer)
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Parent_Offset) = F_Result_Parent
            and Valid_Next (Ctx, F_Result_Parent_Offset))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Parent) = Predecessor (Ctx, F_Result_Parent)'Old
          and Valid_Next (Ctx, F_Result_Parent) = Valid_Next (Ctx, F_Result_Parent)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Legacy_Flags (Ctx) = Get_Result_Legacy_Flags (Ctx)'Old
          and Get_Result_Num_FDs (Ctx) = Get_Result_Num_FDs (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old
          and Cursor (Ctx, F_Result_Handle) = Cursor (Ctx, F_Result_Handle)'Old;

   procedure Set_Result_Buffer (Ctx : in out Context; Val : Binder.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Buffer)
          and then Field_Last (Ctx, F_Result_Buffer) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Buffer, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Buffer) >= Field_Length (Ctx, F_Result_Buffer),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Buffer)
          and Get_Result_Buffer (Ctx) = Val
          and Invalid (Ctx, F_Result_Unused_Padding)
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Length) = F_Result_Buffer
            and Valid_Next (Ctx, F_Result_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Buffer) = Predecessor (Ctx, F_Result_Buffer)'Old
          and Valid_Next (Ctx, F_Result_Buffer) = Valid_Next (Ctx, F_Result_Buffer)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Has_Parent (Ctx) = Get_Result_Has_Parent (Ctx)'Old
          and Get_Result_Padding (Ctx) = Get_Result_Padding (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old
          and Cursor (Ctx, F_Result_Handle) = Cursor (Ctx, F_Result_Handle)'Old
          and Cursor (Ctx, F_Result_Parent) = Cursor (Ctx, F_Result_Parent)'Old;

   procedure Set_Result_Unused_Padding (Ctx : in out Context; Val : Binder.MBZ32) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Unused_Padding)
          and then Field_Last (Ctx, F_Result_Unused_Padding) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Unused_Padding, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Unused_Padding) >= Field_Length (Ctx, F_Result_Unused_Padding),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Unused_Padding)
          and Get_Result_Unused_Padding (Ctx) = Val
          and Invalid (Ctx, F_Result_Parent_Offset)
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (Predecessor (Ctx, F_Result_Cookie) = F_Result_Unused_Padding
            and Valid_Next (Ctx, F_Result_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Unused_Padding) = Predecessor (Ctx, F_Result_Unused_Padding)'Old
          and Valid_Next (Ctx, F_Result_Unused_Padding) = Valid_Next (Ctx, F_Result_Unused_Padding)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Flags (Ctx) = Get_Result_Flags (Ctx)'Old
          and Get_Result_Handle (Ctx) = Get_Result_Handle (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old
          and Cursor (Ctx, F_Result_Handle) = Cursor (Ctx, F_Result_Handle)'Old
          and Cursor (Ctx, F_Result_Parent) = Cursor (Ctx, F_Result_Parent)'Old
          and Cursor (Ctx, F_Result_Buffer) = Cursor (Ctx, F_Result_Buffer)'Old;

   procedure Set_Result_Parent_Offset (Ctx : in out Context; Val : Binder.Offset) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Parent_Offset)
          and then Field_Last (Ctx, F_Result_Parent_Offset) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Parent_Offset, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Parent_Offset) >= Field_Length (Ctx, F_Result_Parent_Offset),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Parent_Offset)
          and Get_Result_Parent_Offset (Ctx) = Val
          and Invalid (Ctx, F_Result_Length)
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Parent_Offset) = Predecessor (Ctx, F_Result_Parent_Offset)'Old
          and Valid_Next (Ctx, F_Result_Parent_Offset) = Valid_Next (Ctx, F_Result_Parent_Offset)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Legacy_Flags (Ctx) = Get_Result_Legacy_Flags (Ctx)'Old
          and Get_Result_Num_FDs (Ctx) = Get_Result_Num_FDs (Ctx)'Old
          and Get_Result_Parent (Ctx) = Get_Result_Parent (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old
          and Cursor (Ctx, F_Result_Handle) = Cursor (Ctx, F_Result_Handle)'Old
          and Cursor (Ctx, F_Result_Parent) = Cursor (Ctx, F_Result_Parent)'Old
          and Cursor (Ctx, F_Result_Buffer) = Cursor (Ctx, F_Result_Buffer)'Old
          and Cursor (Ctx, F_Result_Unused_Padding) = Cursor (Ctx, F_Result_Unused_Padding)'Old;

   procedure Set_Result_Length (Ctx : in out Context; Val : Binder.Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Length)
          and then Field_Last (Ctx, F_Result_Length) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Length) >= Field_Length (Ctx, F_Result_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Length)
          and Get_Result_Length (Ctx) = Val
          and Invalid (Ctx, F_Result_Cookie)
          and Invalid (Ctx, F_Result_Index)
          and (if Types.Bit_Length (Convert (Get_Result_Has_Parent (Ctx))) = Types.Bit_Length (Convert (True)) then
             Predecessor (Ctx, F_Result_Index) = F_Result_Length
               and Valid_Next (Ctx, F_Result_Index))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Length) = Predecessor (Ctx, F_Result_Length)'Old
          and Valid_Next (Ctx, F_Result_Length) = Valid_Next (Ctx, F_Result_Length)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Has_Parent (Ctx) = Get_Result_Has_Parent (Ctx)'Old
          and Get_Result_Padding (Ctx) = Get_Result_Padding (Ctx)'Old
          and Get_Result_Buffer (Ctx) = Get_Result_Buffer (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old
          and Cursor (Ctx, F_Result_Handle) = Cursor (Ctx, F_Result_Handle)'Old
          and Cursor (Ctx, F_Result_Parent) = Cursor (Ctx, F_Result_Parent)'Old
          and Cursor (Ctx, F_Result_Buffer) = Cursor (Ctx, F_Result_Buffer)'Old
          and Cursor (Ctx, F_Result_Unused_Padding) = Cursor (Ctx, F_Result_Unused_Padding)'Old
          and Cursor (Ctx, F_Result_Parent_Offset) = Cursor (Ctx, F_Result_Parent_Offset)'Old;

   procedure Set_Result_Cookie (Ctx : in out Context; Val : Binder.Cookie) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Cookie)
          and then Field_Last (Ctx, F_Result_Cookie) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Cookie, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Cookie) >= Field_Length (Ctx, F_Result_Cookie),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Cookie)
          and Get_Result_Cookie (Ctx) = Val
          and Invalid (Ctx, F_Result_Index)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Cookie) = Predecessor (Ctx, F_Result_Cookie)'Old
          and Valid_Next (Ctx, F_Result_Cookie) = Valid_Next (Ctx, F_Result_Cookie)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old
          and Cursor (Ctx, F_Result_Handle) = Cursor (Ctx, F_Result_Handle)'Old
          and Cursor (Ctx, F_Result_Parent) = Cursor (Ctx, F_Result_Parent)'Old
          and Cursor (Ctx, F_Result_Buffer) = Cursor (Ctx, F_Result_Buffer)'Old
          and Cursor (Ctx, F_Result_Unused_Padding) = Cursor (Ctx, F_Result_Unused_Padding)'Old
          and Cursor (Ctx, F_Result_Parent_Offset) = Cursor (Ctx, F_Result_Parent_Offset)'Old
          and Cursor (Ctx, F_Result_Length) = Cursor (Ctx, F_Result_Length)'Old;

   procedure Set_Result_Index (Ctx : in out Context; Val : Binder.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Result_Index)
          and then Field_Last (Ctx, F_Result_Index) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Result_Index, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Result_Index) >= Field_Length (Ctx, F_Result_Index),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Result_Index)
          and Get_Result_Index (Ctx) = Val
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Result_Index) = Predecessor (Ctx, F_Result_Index)'Old
          and Valid_Next (Ctx, F_Result_Index) = Valid_Next (Ctx, F_Result_Index)'Old
          and Get_Result_Kind (Ctx) = Get_Result_Kind (Ctx)'Old
          and Get_Result_Arity (Ctx) = Get_Result_Arity (Ctx)'Old
          and Get_Result_Tag (Ctx) = Get_Result_Tag (Ctx)'Old
          and Get_Result_Has_Parent (Ctx) = Get_Result_Has_Parent (Ctx)'Old
          and Get_Result_Padding (Ctx) = Get_Result_Padding (Ctx)'Old
          and Get_Result_Buffer (Ctx) = Get_Result_Buffer (Ctx)'Old
          and Get_Result_Length (Ctx) = Get_Result_Length (Ctx)'Old
          and Cursor (Ctx, F_Result_Kind) = Cursor (Ctx, F_Result_Kind)'Old
          and Cursor (Ctx, F_Result_Arity) = Cursor (Ctx, F_Result_Arity)'Old
          and Cursor (Ctx, F_Result_Tag) = Cursor (Ctx, F_Result_Tag)'Old
          and Cursor (Ctx, F_Result_Legacy_Flags) = Cursor (Ctx, F_Result_Legacy_Flags)'Old
          and Cursor (Ctx, F_Result_Has_Parent) = Cursor (Ctx, F_Result_Has_Parent)'Old
          and Cursor (Ctx, F_Result_Flags) = Cursor (Ctx, F_Result_Flags)'Old
          and Cursor (Ctx, F_Result_FD) = Cursor (Ctx, F_Result_FD)'Old
          and Cursor (Ctx, F_Result_Num_FDs) = Cursor (Ctx, F_Result_Num_FDs)'Old
          and Cursor (Ctx, F_Result_Padding) = Cursor (Ctx, F_Result_Padding)'Old
          and Cursor (Ctx, F_Result_Binder) = Cursor (Ctx, F_Result_Binder)'Old
          and Cursor (Ctx, F_Result_Handle) = Cursor (Ctx, F_Result_Handle)'Old
          and Cursor (Ctx, F_Result_Parent) = Cursor (Ctx, F_Result_Parent)'Old
          and Cursor (Ctx, F_Result_Buffer) = Cursor (Ctx, F_Result_Buffer)'Old
          and Cursor (Ctx, F_Result_Unused_Padding) = Cursor (Ctx, F_Result_Unused_Padding)'Old
          and Cursor (Ctx, F_Result_Parent_Offset) = Cursor (Ctx, F_Result_Parent_Offset)'Old
          and Cursor (Ctx, F_Result_Length) = Cursor (Ctx, F_Result_Length)'Old
          and Cursor (Ctx, F_Result_Cookie) = Cursor (Ctx, F_Result_Cookie)'Old;

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
         when F_Result_Kind =>
            Valid (Val.Result_Kind_Value),
         when F_Result_Arity =>
            Valid (Val.Result_Arity_Value),
         when F_Result_Tag =>
            Valid (Val.Result_Tag_Value),
         when F_Result_Legacy_Flags =>
            Valid (Val.Result_Legacy_Flags_Value),
         when F_Result_Has_Parent =>
            Valid (Val.Result_Has_Parent_Value),
         when F_Result_Flags =>
            Valid (Val.Result_Flags_Value),
         when F_Result_FD =>
            Valid (Val.Result_FD_Value),
         when F_Result_Num_FDs =>
            Valid (Val.Result_Num_FDs_Value),
         when F_Result_Padding =>
            Valid (Val.Result_Padding_Value),
         when F_Result_Binder =>
            Valid (Val.Result_Binder_Value),
         when F_Result_Handle =>
            Valid (Val.Result_Handle_Value),
         when F_Result_Parent =>
            Valid (Val.Result_Parent_Value),
         when F_Result_Buffer =>
            Valid (Val.Result_Buffer_Value),
         when F_Result_Unused_Padding =>
            Valid (Val.Result_Unused_Padding_Value),
         when F_Result_Parent_Offset =>
            Valid (Val.Result_Parent_Offset_Value),
         when F_Result_Length =>
            Valid (Val.Result_Length_Value),
         when F_Result_Cookie =>
            Valid (Val.Result_Cookie_Value),
         when F_Result_Index =>
            Valid (Val.Result_Index_Value),
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
      and then ((if Structural_Valid (Cursors (F_Result_Arity)) then
           (Valid (Cursors (F_Result_Kind))
               and then Cursors (F_Result_Arity).Predecessor = F_Result_Kind))
        and then (if Structural_Valid (Cursors (F_Result_Tag)) then
           (Valid (Cursors (F_Result_Arity))
               and then Cursors (F_Result_Tag).Predecessor = F_Result_Arity
               and then (Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                 or (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                   and Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))))
        and then (if Structural_Valid (Cursors (F_Result_Legacy_Flags)) then
           (Valid (Cursors (F_Result_Tag))
               and then Cursors (F_Result_Legacy_Flags).Predecessor = F_Result_Tag
               and then Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))))
        and then (if Structural_Valid (Cursors (F_Result_Has_Parent)) then
           (Valid (Cursors (F_Result_Tag))
               and then Cursors (F_Result_Has_Parent).Predecessor = F_Result_Tag
               and then Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))))
        and then (if Structural_Valid (Cursors (F_Result_Flags)) then
           (Valid (Cursors (F_Result_Tag))
               and then Cursors (F_Result_Flags).Predecessor = F_Result_Tag
               and then (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                 and Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))))
        and then (if Structural_Valid (Cursors (F_Result_FD)) then
           (Valid (Cursors (F_Result_Legacy_Flags))
               and then Cursors (F_Result_FD).Predecessor = F_Result_Legacy_Flags
               and then Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))))
        and then (if Structural_Valid (Cursors (F_Result_Num_FDs)) then
           (Valid (Cursors (F_Result_Legacy_Flags))
               and then Cursors (F_Result_Num_FDs).Predecessor = F_Result_Legacy_Flags
               and then Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
        and then (if Structural_Valid (Cursors (F_Result_Padding)) then
           (Valid (Cursors (F_Result_Has_Parent))
               and then Cursors (F_Result_Padding).Predecessor = F_Result_Has_Parent))
        and then (if Structural_Valid (Cursors (F_Result_Binder)) then
           (Valid (Cursors (F_Result_Flags))
               and then Cursors (F_Result_Binder).Predecessor = F_Result_Flags
               and then (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                 or Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))))
        and then (if Structural_Valid (Cursors (F_Result_Handle)) then
           (Valid (Cursors (F_Result_Flags))
               and then Cursors (F_Result_Handle).Predecessor = F_Result_Flags
               and then (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                 or Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))))
        and then (if Structural_Valid (Cursors (F_Result_Parent)) then
           (Valid (Cursors (F_Result_Num_FDs))
               and then Cursors (F_Result_Parent).Predecessor = F_Result_Num_FDs))
        and then (if Structural_Valid (Cursors (F_Result_Buffer)) then
           (Valid (Cursors (F_Result_Padding))
               and then Cursors (F_Result_Buffer).Predecessor = F_Result_Padding))
        and then (if Structural_Valid (Cursors (F_Result_Unused_Padding)) then
           (Valid (Cursors (F_Result_Handle))
               and then Cursors (F_Result_Unused_Padding).Predecessor = F_Result_Handle))
        and then (if Structural_Valid (Cursors (F_Result_Parent_Offset)) then
           (Valid (Cursors (F_Result_Parent))
               and then Cursors (F_Result_Parent_Offset).Predecessor = F_Result_Parent))
        and then (if Structural_Valid (Cursors (F_Result_Length)) then
           (Valid (Cursors (F_Result_Buffer))
               and then Cursors (F_Result_Length).Predecessor = F_Result_Buffer))
        and then (if Structural_Valid (Cursors (F_Result_Cookie)) then
           (Valid (Cursors (F_Result_FD))
               and then Cursors (F_Result_Cookie).Predecessor = F_Result_FD)
             or (Valid (Cursors (F_Result_Binder))
               and then Cursors (F_Result_Cookie).Predecessor = F_Result_Binder)
             or (Valid (Cursors (F_Result_Unused_Padding))
               and then Cursors (F_Result_Cookie).Predecessor = F_Result_Unused_Padding))
        and then (if Structural_Valid (Cursors (F_Result_Index)) then
           (Valid (Cursors (F_Result_Length))
               and then Cursors (F_Result_Index).Predecessor = F_Result_Length
               and then Types.Bit_Length (Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)))))
      and then ((if Invalid (Cursors (F_Result_Kind)) then
           Invalid (Cursors (F_Result_Arity)))
        and then (if Invalid (Cursors (F_Result_Arity)) then
           Invalid (Cursors (F_Result_Tag)))
        and then (if Invalid (Cursors (F_Result_Tag)) then
           Invalid (Cursors (F_Result_Legacy_Flags)))
        and then (if Invalid (Cursors (F_Result_Tag)) then
           Invalid (Cursors (F_Result_Has_Parent)))
        and then (if Invalid (Cursors (F_Result_Tag)) then
           Invalid (Cursors (F_Result_Flags)))
        and then (if Invalid (Cursors (F_Result_Legacy_Flags)) then
           Invalid (Cursors (F_Result_FD)))
        and then (if Invalid (Cursors (F_Result_Legacy_Flags)) then
           Invalid (Cursors (F_Result_Num_FDs)))
        and then (if Invalid (Cursors (F_Result_Has_Parent)) then
           Invalid (Cursors (F_Result_Padding)))
        and then (if Invalid (Cursors (F_Result_Flags)) then
           Invalid (Cursors (F_Result_Binder)))
        and then (if Invalid (Cursors (F_Result_Flags)) then
           Invalid (Cursors (F_Result_Handle)))
        and then (if Invalid (Cursors (F_Result_Num_FDs)) then
           Invalid (Cursors (F_Result_Parent)))
        and then (if Invalid (Cursors (F_Result_Padding)) then
           Invalid (Cursors (F_Result_Buffer)))
        and then (if Invalid (Cursors (F_Result_Handle)) then
           Invalid (Cursors (F_Result_Unused_Padding)))
        and then (if Invalid (Cursors (F_Result_Parent)) then
           Invalid (Cursors (F_Result_Parent_Offset)))
        and then (if Invalid (Cursors (F_Result_Buffer)) then
           Invalid (Cursors (F_Result_Length)))
        and then (if Invalid (Cursors (F_Result_FD))
             and then Invalid (Cursors (F_Result_Binder))
             and then Invalid (Cursors (F_Result_Unused_Padding)) then
           Invalid (Cursors (F_Result_Cookie)))
        and then (if Invalid (Cursors (F_Result_Length)) then
           Invalid (Cursors (F_Result_Index))))
      and then (if Structural_Valid (Cursors (F_Result_Kind)) then
         (Cursors (F_Result_Kind).Last - Cursors (F_Result_Kind).First + 1) = Binder.Binder_Kind_Base'Size
           and then Cursors (F_Result_Kind).Predecessor = F_Initial
           and then Cursors (F_Result_Kind).First = First
           and then (if Structural_Valid (Cursors (F_Result_Arity)) then
              (Cursors (F_Result_Arity).Last - Cursors (F_Result_Arity).First + 1) = Binder.Binder_Arity_Base'Size
                and then Cursors (F_Result_Arity).Predecessor = F_Result_Kind
                and then Cursors (F_Result_Arity).First = (Cursors (F_Result_Kind).Last + 1)
                and then (if Structural_Valid (Cursors (F_Result_Tag))
                     and then (Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                       or (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                         and Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                   (Cursors (F_Result_Tag).Last - Cursors (F_Result_Tag).First + 1) = Binder.Binder_Tag_Base'Size
                     and then Cursors (F_Result_Tag).Predecessor = F_Result_Arity
                     and then Cursors (F_Result_Tag).First = (Cursors (F_Result_Arity).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Result_Legacy_Flags))
                          and then Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                        (Cursors (F_Result_Legacy_Flags).Last - Cursors (F_Result_Legacy_Flags).First + 1) = Binder.MBZ32_Base'Size
                          and then Cursors (F_Result_Legacy_Flags).Predecessor = F_Result_Tag
                          and then Cursors (F_Result_Legacy_Flags).First = (Cursors (F_Result_Tag).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Result_FD))
                               and then Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                             (Cursors (F_Result_FD).Last - Cursors (F_Result_FD).First + 1) = Binder.Handle_Base'Size
                               and then Cursors (F_Result_FD).Predecessor = F_Result_Legacy_Flags
                               and then Cursors (F_Result_FD).First = (Cursors (F_Result_Legacy_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Result_Cookie)) then
                                  (Cursors (F_Result_Cookie).Last - Cursors (F_Result_Cookie).First + 1) = Binder.Cookie'Size
                                    and then Cursors (F_Result_Cookie).Predecessor = F_Result_FD
                                    and then Cursors (F_Result_Cookie).First = (Cursors (F_Result_FD).Last + 1)))
                          and then (if Structural_Valid (Cursors (F_Result_Num_FDs))
                               and then Types.Bit_Length (Cursors (F_Result_Arity).Value.Result_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                             (Cursors (F_Result_Num_FDs).Last - Cursors (F_Result_Num_FDs).First + 1) = Binder.Count'Size
                               and then Cursors (F_Result_Num_FDs).Predecessor = F_Result_Legacy_Flags
                               and then Cursors (F_Result_Num_FDs).First = (Cursors (F_Result_Legacy_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Result_Parent)) then
                                  (Cursors (F_Result_Parent).Last - Cursors (F_Result_Parent).First + 1) = Binder.Index'Size
                                    and then Cursors (F_Result_Parent).Predecessor = F_Result_Num_FDs
                                    and then Cursors (F_Result_Parent).First = (Cursors (F_Result_Num_FDs).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Result_Parent_Offset)) then
                                       (Cursors (F_Result_Parent_Offset).Last - Cursors (F_Result_Parent_Offset).First + 1) = Binder.Offset'Size
                                         and then Cursors (F_Result_Parent_Offset).Predecessor = F_Result_Parent
                                         and then Cursors (F_Result_Parent_Offset).First = (Cursors (F_Result_Parent).Last + 1)))))
                     and then (if Structural_Valid (Cursors (F_Result_Has_Parent))
                          and then Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                        (Cursors (F_Result_Has_Parent).Last - Cursors (F_Result_Has_Parent).First + 1) = Builtin_Types.Boolean_Base'Size
                          and then Cursors (F_Result_Has_Parent).Predecessor = F_Result_Tag
                          and then Cursors (F_Result_Has_Parent).First = (Cursors (F_Result_Tag).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Result_Padding)) then
                             (Cursors (F_Result_Padding).Last - Cursors (F_Result_Padding).First + 1) = Binder.MBZ31_Base'Size
                               and then Cursors (F_Result_Padding).Predecessor = F_Result_Has_Parent
                               and then Cursors (F_Result_Padding).First = (Cursors (F_Result_Has_Parent).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Result_Buffer)) then
                                  (Cursors (F_Result_Buffer).Last - Cursors (F_Result_Buffer).First + 1) = Binder.Index'Size
                                    and then Cursors (F_Result_Buffer).Predecessor = F_Result_Padding
                                    and then Cursors (F_Result_Buffer).First = (Cursors (F_Result_Padding).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Result_Length)) then
                                       (Cursors (F_Result_Length).Last - Cursors (F_Result_Length).First + 1) = Binder.Length_Base'Size
                                         and then Cursors (F_Result_Length).Predecessor = F_Result_Buffer
                                         and then Cursors (F_Result_Length).First = (Cursors (F_Result_Buffer).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Result_Index))
                                              and then Types.Bit_Length (Cursors (F_Result_Has_Parent).Value.Result_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                                            (Cursors (F_Result_Index).Last - Cursors (F_Result_Index).First + 1) = Binder.Index'Size
                                              and then Cursors (F_Result_Index).Predecessor = F_Result_Length
                                              and then Cursors (F_Result_Index).First = (Cursors (F_Result_Length).Last + 1))))))
                     and then (if Structural_Valid (Cursors (F_Result_Flags))
                          and then (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                            and Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) /= Types.Bit_Length (Convert (BK_FD))) then
                        (Cursors (F_Result_Flags).Last - Cursors (F_Result_Flags).First + 1) = Binder.Flat_Binder_Flags_Base'Size
                          and then Cursors (F_Result_Flags).Predecessor = F_Result_Tag
                          and then Cursors (F_Result_Flags).First = (Cursors (F_Result_Tag).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Result_Binder))
                               and then (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                 or Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                             (Cursors (F_Result_Binder).Last - Cursors (F_Result_Binder).First + 1) = Binder.Value'Size
                               and then Cursors (F_Result_Binder).Predecessor = F_Result_Flags
                               and then Cursors (F_Result_Binder).First = (Cursors (F_Result_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Result_Cookie)) then
                                  (Cursors (F_Result_Cookie).Last - Cursors (F_Result_Cookie).First + 1) = Binder.Cookie'Size
                                    and then Cursors (F_Result_Cookie).Predecessor = F_Result_Binder
                                    and then Cursors (F_Result_Cookie).First = (Cursors (F_Result_Binder).Last + 1)))
                          and then (if Structural_Valid (Cursors (F_Result_Handle))
                               and then (Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                 or Types.Bit_Length (Cursors (F_Result_Kind).Value.Result_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                             (Cursors (F_Result_Handle).Last - Cursors (F_Result_Handle).First + 1) = Binder.Handle_Base'Size
                               and then Cursors (F_Result_Handle).Predecessor = F_Result_Flags
                               and then Cursors (F_Result_Handle).First = (Cursors (F_Result_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Result_Unused_Padding)) then
                                  (Cursors (F_Result_Unused_Padding).Last - Cursors (F_Result_Unused_Padding).First + 1) = Binder.MBZ32_Base'Size
                                    and then Cursors (F_Result_Unused_Padding).Predecessor = F_Result_Handle
                                    and then Cursors (F_Result_Unused_Padding).First = (Cursors (F_Result_Handle).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Result_Cookie)) then
                                       (Cursors (F_Result_Cookie).Last - Cursors (F_Result_Cookie).First + 1) = Binder.Cookie'Size
                                         and then Cursors (F_Result_Cookie).Predecessor = F_Result_Unused_Padding
                                         and then Cursors (F_Result_Cookie).First = (Cursors (F_Result_Unused_Padding).Last + 1)))))))));

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

end Parpen.Service_Manager.Generic_Reply_Get_Service;
