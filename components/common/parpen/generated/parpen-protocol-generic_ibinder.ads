with Parpen.Builtin_Types;
with Parpen.Builtin_Types.Conversions;
use Parpen.Builtin_Types.Conversions;
with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Protocol.Generic_IBinder with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_IBinder);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Kind, F_Arity, F_Tag, F_Legacy_Flags, F_Has_Parent, F_Flags, F_FD, F_Num_FDs, F_Padding, F_Binder, F_Handle, F_Parent, F_Buffer, F_Unused_Padding, F_Parent_Offset, F_Length, F_Cookie, F_Index, F_Final);

   subtype Field is Virtual_Field range F_Kind .. F_Index;

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
            when F_Kind =>
               Kind_Value : Protocol.Binder_Kind_Base;
            when F_Arity =>
               Arity_Value : Protocol.Binder_Arity_Base;
            when F_Tag =>
               Tag_Value : Protocol.Binder_Tag_Base;
            when F_Legacy_Flags =>
               Legacy_Flags_Value : Protocol.MBZ32_Base;
            when F_Has_Parent =>
               Has_Parent_Value : Builtin_Types.Boolean_Base;
            when F_Flags =>
               Flags_Value : Protocol.Flat_Binder_Flags_Base;
            when F_FD =>
               FD_Value : Protocol.Handle_Base;
            when F_Num_FDs =>
               Num_FDs_Value : Protocol.Count;
            when F_Padding =>
               Padding_Value : Protocol.MBZ31_Base;
            when F_Binder =>
               Binder_Value : Protocol.Binder;
            when F_Handle =>
               Handle_Value : Protocol.Handle_Base;
            when F_Parent =>
               Parent_Value : Protocol.Index;
            when F_Buffer =>
               Buffer_Value : Protocol.Index;
            when F_Unused_Padding =>
               Unused_Padding_Value : Protocol.MBZ32_Base;
            when F_Parent_Offset =>
               Parent_Offset_Value : Protocol.Offset_Base;
            when F_Length =>
               Length_Value : Protocol.Length_Base;
            when F_Cookie =>
               Cookie_Value : Protocol.Cookie;
            when F_Index =>
               Index_Value : Protocol.Index;
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

   function Get_Kind (Ctx : Context) return Protocol.Binder_Kind with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Kind);

   function Get_Arity (Ctx : Context) return Protocol.Binder_Arity with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Arity);

   function Get_Tag (Ctx : Context) return Protocol.Binder_Tag with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Tag);

   function Get_Legacy_Flags (Ctx : Context) return Protocol.MBZ32 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Legacy_Flags);

   function Get_Has_Parent (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Has_Parent);

   function Get_Flags (Ctx : Context) return Protocol.Flat_Binder_Flags with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Flags);

   function Get_FD (Ctx : Context) return Protocol.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_FD);

   function Get_Num_FDs (Ctx : Context) return Protocol.Count with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Num_FDs);

   function Get_Padding (Ctx : Context) return Protocol.MBZ31 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Padding);

   function Get_Binder (Ctx : Context) return Protocol.Binder with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Binder);

   function Get_Handle (Ctx : Context) return Protocol.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Handle);

   function Get_Parent (Ctx : Context) return Protocol.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Parent);

   function Get_Buffer (Ctx : Context) return Protocol.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Buffer);

   function Get_Unused_Padding (Ctx : Context) return Protocol.MBZ32 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Unused_Padding);

   function Get_Parent_Offset (Ctx : Context) return Protocol.Offset with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Parent_Offset);

   function Get_Length (Ctx : Context) return Protocol.Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Length);

   function Get_Cookie (Ctx : Context) return Protocol.Cookie with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Cookie);

   function Get_Index (Ctx : Context) return Protocol.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Index);

   procedure Set_Kind (Ctx : in out Context; Val : Protocol.Binder_Kind) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Kind)
          and then Field_Last (Ctx, F_Kind) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Kind, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Kind) >= Field_Length (Ctx, F_Kind),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Kind)
          and Get_Kind (Ctx) = Val
          and Invalid (Ctx, F_Arity)
          and Invalid (Ctx, F_Tag)
          and Invalid (Ctx, F_Legacy_Flags)
          and Invalid (Ctx, F_Has_Parent)
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_FD)
          and Invalid (Ctx, F_Num_FDs)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Arity) = F_Kind
            and Valid_Next (Ctx, F_Arity))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Kind) = Predecessor (Ctx, F_Kind)'Old
          and Valid_Next (Ctx, F_Kind) = Valid_Next (Ctx, F_Kind)'Old;

   procedure Set_Arity (Ctx : in out Context; Val : Protocol.Binder_Arity) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Arity)
          and then Field_Last (Ctx, F_Arity) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Arity, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Arity) >= Field_Length (Ctx, F_Arity),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Arity)
          and Get_Arity (Ctx) = Val
          and Invalid (Ctx, F_Tag)
          and Invalid (Ctx, F_Legacy_Flags)
          and Invalid (Ctx, F_Has_Parent)
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_FD)
          and Invalid (Ctx, F_Num_FDs)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (if Types.Bit_Length (Convert (Get_Arity (Ctx))) = Types.Bit_Length (Convert (BA_SINGLE))
               or (Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_FD))
                 and Types.Bit_Length (Convert (Get_Arity (Ctx))) = Types.Bit_Length (Convert (BA_ARRAY))) then
             Predecessor (Ctx, F_Tag) = F_Arity
               and Valid_Next (Ctx, F_Tag))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Arity) = Predecessor (Ctx, F_Arity)'Old
          and Valid_Next (Ctx, F_Arity) = Valid_Next (Ctx, F_Arity)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old;

   procedure Set_Tag (Ctx : in out Context; Val : Protocol.Binder_Tag) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Tag)
          and then Field_Last (Ctx, F_Tag) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Tag, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Tag) >= Field_Length (Ctx, F_Tag),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Tag)
          and Get_Tag (Ctx) = Val
          and Invalid (Ctx, F_Legacy_Flags)
          and Invalid (Ctx, F_Has_Parent)
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_FD)
          and Invalid (Ctx, F_Num_FDs)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (if Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_FD)) then
             Predecessor (Ctx, F_Legacy_Flags) = F_Tag
               and Valid_Next (Ctx, F_Legacy_Flags))
          and (if Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_POINTER)) then
             Predecessor (Ctx, F_Has_Parent) = F_Tag
               and Valid_Next (Ctx, F_Has_Parent))
          and (if Types.Bit_Length (Convert (Get_Kind (Ctx))) /= Types.Bit_Length (Convert (BK_POINTER))
               and Types.Bit_Length (Convert (Get_Kind (Ctx))) /= Types.Bit_Length (Convert (BK_FD)) then
             Predecessor (Ctx, F_Flags) = F_Tag
               and Valid_Next (Ctx, F_Flags))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Tag) = Predecessor (Ctx, F_Tag)'Old
          and Valid_Next (Ctx, F_Tag) = Valid_Next (Ctx, F_Tag)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old;

   procedure Set_Legacy_Flags (Ctx : in out Context; Val : Protocol.MBZ32) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Legacy_Flags)
          and then Field_Last (Ctx, F_Legacy_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Legacy_Flags, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Legacy_Flags) >= Field_Length (Ctx, F_Legacy_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Legacy_Flags)
          and Get_Legacy_Flags (Ctx) = Val
          and Invalid (Ctx, F_Has_Parent)
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_FD)
          and Invalid (Ctx, F_Num_FDs)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (if Types.Bit_Length (Convert (Get_Arity (Ctx))) = Types.Bit_Length (Convert (BA_SINGLE)) then
             Predecessor (Ctx, F_FD) = F_Legacy_Flags
               and Valid_Next (Ctx, F_FD))
          and (if Types.Bit_Length (Convert (Get_Arity (Ctx))) = Types.Bit_Length (Convert (BA_ARRAY)) then
             Predecessor (Ctx, F_Num_FDs) = F_Legacy_Flags
               and Valid_Next (Ctx, F_Num_FDs))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Legacy_Flags) = Predecessor (Ctx, F_Legacy_Flags)'Old
          and Valid_Next (Ctx, F_Legacy_Flags) = Valid_Next (Ctx, F_Legacy_Flags)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old;

   procedure Set_Has_Parent (Ctx : in out Context; Val : Boolean) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Has_Parent)
          and then Field_Last (Ctx, F_Has_Parent) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Has_Parent, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Has_Parent) >= Field_Length (Ctx, F_Has_Parent),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Has_Parent)
          and Get_Has_Parent (Ctx) = Val
          and Invalid (Ctx, F_Flags)
          and Invalid (Ctx, F_FD)
          and Invalid (Ctx, F_Num_FDs)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Padding) = F_Has_Parent
            and Valid_Next (Ctx, F_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Has_Parent) = Predecessor (Ctx, F_Has_Parent)'Old
          and Valid_Next (Ctx, F_Has_Parent) = Valid_Next (Ctx, F_Has_Parent)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old;

   procedure Set_Flags (Ctx : in out Context; Val : Protocol.Flat_Binder_Flags) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Flags)
          and then Field_Last (Ctx, F_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Flags, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Flags) >= Field_Length (Ctx, F_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Flags)
          and Get_Flags (Ctx) = Val
          and Invalid (Ctx, F_FD)
          and Invalid (Ctx, F_Num_FDs)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (if Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
             Predecessor (Ctx, F_Binder) = F_Flags
               and Valid_Next (Ctx, F_Binder))
          and (if Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Convert (Get_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
             Predecessor (Ctx, F_Handle) = F_Flags
               and Valid_Next (Ctx, F_Handle))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Flags) = Predecessor (Ctx, F_Flags)'Old
          and Valid_Next (Ctx, F_Flags) = Valid_Next (Ctx, F_Flags)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old;

   procedure Set_FD (Ctx : in out Context; Val : Protocol.Handle) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_FD)
          and then Field_Last (Ctx, F_FD) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_FD, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_FD) >= Field_Length (Ctx, F_FD),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_FD)
          and Get_FD (Ctx) = Val
          and Invalid (Ctx, F_Num_FDs)
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Cookie) = F_FD
            and Valid_Next (Ctx, F_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_FD) = Predecessor (Ctx, F_FD)'Old
          and Valid_Next (Ctx, F_FD) = Valid_Next (Ctx, F_FD)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Legacy_Flags (Ctx) = Get_Legacy_Flags (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old;

   procedure Set_Num_FDs (Ctx : in out Context; Val : Protocol.Count) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Num_FDs)
          and then Field_Last (Ctx, F_Num_FDs) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Num_FDs, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Num_FDs) >= Field_Length (Ctx, F_Num_FDs),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Num_FDs)
          and Get_Num_FDs (Ctx) = Val
          and Invalid (Ctx, F_Padding)
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Parent) = F_Num_FDs
            and Valid_Next (Ctx, F_Parent))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Num_FDs) = Predecessor (Ctx, F_Num_FDs)'Old
          and Valid_Next (Ctx, F_Num_FDs) = Valid_Next (Ctx, F_Num_FDs)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Legacy_Flags (Ctx) = Get_Legacy_Flags (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old;

   procedure Set_Padding (Ctx : in out Context; Val : Protocol.MBZ31) with
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
          and Invalid (Ctx, F_Binder)
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Buffer) = F_Padding
            and Valid_Next (Ctx, F_Buffer))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Padding) = Predecessor (Ctx, F_Padding)'Old
          and Valid_Next (Ctx, F_Padding) = Valid_Next (Ctx, F_Padding)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Has_Parent (Ctx) = Get_Has_Parent (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old;

   procedure Set_Binder (Ctx : in out Context; Val : Protocol.Binder) with
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
          and Invalid (Ctx, F_Handle)
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Cookie) = F_Binder
            and Valid_Next (Ctx, F_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Binder) = Predecessor (Ctx, F_Binder)'Old
          and Valid_Next (Ctx, F_Binder) = Valid_Next (Ctx, F_Binder)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Flags (Ctx) = Get_Flags (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old;

   procedure Set_Handle (Ctx : in out Context; Val : Protocol.Handle) with
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
          and Invalid (Ctx, F_Parent)
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Unused_Padding) = F_Handle
            and Valid_Next (Ctx, F_Unused_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Handle) = Predecessor (Ctx, F_Handle)'Old
          and Valid_Next (Ctx, F_Handle) = Valid_Next (Ctx, F_Handle)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Flags (Ctx) = Get_Flags (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old;

   procedure Set_Parent (Ctx : in out Context; Val : Protocol.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Parent)
          and then Field_Last (Ctx, F_Parent) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Parent, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Parent) >= Field_Length (Ctx, F_Parent),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Parent)
          and Get_Parent (Ctx) = Val
          and Invalid (Ctx, F_Buffer)
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Parent_Offset) = F_Parent
            and Valid_Next (Ctx, F_Parent_Offset))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Parent) = Predecessor (Ctx, F_Parent)'Old
          and Valid_Next (Ctx, F_Parent) = Valid_Next (Ctx, F_Parent)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Legacy_Flags (Ctx) = Get_Legacy_Flags (Ctx)'Old
          and Get_Num_FDs (Ctx) = Get_Num_FDs (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old;

   procedure Set_Buffer (Ctx : in out Context; Val : Protocol.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Buffer)
          and then Field_Last (Ctx, F_Buffer) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Buffer, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Buffer) >= Field_Length (Ctx, F_Buffer),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Buffer)
          and Get_Buffer (Ctx) = Val
          and Invalid (Ctx, F_Unused_Padding)
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Length) = F_Buffer
            and Valid_Next (Ctx, F_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Buffer) = Predecessor (Ctx, F_Buffer)'Old
          and Valid_Next (Ctx, F_Buffer) = Valid_Next (Ctx, F_Buffer)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Has_Parent (Ctx) = Get_Has_Parent (Ctx)'Old
          and Get_Padding (Ctx) = Get_Padding (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Parent) = Cursor (Ctx, F_Parent)'Old;

   procedure Set_Unused_Padding (Ctx : in out Context; Val : Protocol.MBZ32) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Unused_Padding)
          and then Field_Last (Ctx, F_Unused_Padding) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Unused_Padding, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Unused_Padding) >= Field_Length (Ctx, F_Unused_Padding),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Unused_Padding)
          and Get_Unused_Padding (Ctx) = Val
          and Invalid (Ctx, F_Parent_Offset)
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (Predecessor (Ctx, F_Cookie) = F_Unused_Padding
            and Valid_Next (Ctx, F_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Unused_Padding) = Predecessor (Ctx, F_Unused_Padding)'Old
          and Valid_Next (Ctx, F_Unused_Padding) = Valid_Next (Ctx, F_Unused_Padding)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Flags (Ctx) = Get_Flags (Ctx)'Old
          and Get_Handle (Ctx) = Get_Handle (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Parent) = Cursor (Ctx, F_Parent)'Old
          and Cursor (Ctx, F_Buffer) = Cursor (Ctx, F_Buffer)'Old;

   procedure Set_Parent_Offset (Ctx : in out Context; Val : Protocol.Offset) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Parent_Offset)
          and then Field_Last (Ctx, F_Parent_Offset) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Parent_Offset, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Parent_Offset) >= Field_Length (Ctx, F_Parent_Offset),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Parent_Offset)
          and Get_Parent_Offset (Ctx) = Val
          and Invalid (Ctx, F_Length)
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Parent_Offset) = Predecessor (Ctx, F_Parent_Offset)'Old
          and Valid_Next (Ctx, F_Parent_Offset) = Valid_Next (Ctx, F_Parent_Offset)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Legacy_Flags (Ctx) = Get_Legacy_Flags (Ctx)'Old
          and Get_Num_FDs (Ctx) = Get_Num_FDs (Ctx)'Old
          and Get_Parent (Ctx) = Get_Parent (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Parent) = Cursor (Ctx, F_Parent)'Old
          and Cursor (Ctx, F_Buffer) = Cursor (Ctx, F_Buffer)'Old
          and Cursor (Ctx, F_Unused_Padding) = Cursor (Ctx, F_Unused_Padding)'Old;

   procedure Set_Length (Ctx : in out Context; Val : Protocol.Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Length)
          and then Field_Last (Ctx, F_Length) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Length) >= Field_Length (Ctx, F_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Length)
          and Get_Length (Ctx) = Val
          and Invalid (Ctx, F_Cookie)
          and Invalid (Ctx, F_Index)
          and (if Types.Bit_Length (Convert (Get_Has_Parent (Ctx))) = Types.Bit_Length (Convert (True)) then
             Predecessor (Ctx, F_Index) = F_Length
               and Valid_Next (Ctx, F_Index))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Length) = Predecessor (Ctx, F_Length)'Old
          and Valid_Next (Ctx, F_Length) = Valid_Next (Ctx, F_Length)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Has_Parent (Ctx) = Get_Has_Parent (Ctx)'Old
          and Get_Padding (Ctx) = Get_Padding (Ctx)'Old
          and Get_Buffer (Ctx) = Get_Buffer (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Parent) = Cursor (Ctx, F_Parent)'Old
          and Cursor (Ctx, F_Buffer) = Cursor (Ctx, F_Buffer)'Old
          and Cursor (Ctx, F_Unused_Padding) = Cursor (Ctx, F_Unused_Padding)'Old
          and Cursor (Ctx, F_Parent_Offset) = Cursor (Ctx, F_Parent_Offset)'Old;

   procedure Set_Cookie (Ctx : in out Context; Val : Protocol.Cookie) with
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
          and Invalid (Ctx, F_Index)
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Cookie) = Predecessor (Ctx, F_Cookie)'Old
          and Valid_Next (Ctx, F_Cookie) = Valid_Next (Ctx, F_Cookie)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Parent) = Cursor (Ctx, F_Parent)'Old
          and Cursor (Ctx, F_Buffer) = Cursor (Ctx, F_Buffer)'Old
          and Cursor (Ctx, F_Unused_Padding) = Cursor (Ctx, F_Unused_Padding)'Old
          and Cursor (Ctx, F_Parent_Offset) = Cursor (Ctx, F_Parent_Offset)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old;

   procedure Set_Index (Ctx : in out Context; Val : Protocol.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Index)
          and then Field_Last (Ctx, F_Index) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Index, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Index) >= Field_Length (Ctx, F_Index),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Index)
          and Get_Index (Ctx) = Val
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Index) = Predecessor (Ctx, F_Index)'Old
          and Valid_Next (Ctx, F_Index) = Valid_Next (Ctx, F_Index)'Old
          and Get_Kind (Ctx) = Get_Kind (Ctx)'Old
          and Get_Arity (Ctx) = Get_Arity (Ctx)'Old
          and Get_Tag (Ctx) = Get_Tag (Ctx)'Old
          and Get_Has_Parent (Ctx) = Get_Has_Parent (Ctx)'Old
          and Get_Padding (Ctx) = Get_Padding (Ctx)'Old
          and Get_Buffer (Ctx) = Get_Buffer (Ctx)'Old
          and Get_Length (Ctx) = Get_Length (Ctx)'Old
          and Cursor (Ctx, F_Kind) = Cursor (Ctx, F_Kind)'Old
          and Cursor (Ctx, F_Arity) = Cursor (Ctx, F_Arity)'Old
          and Cursor (Ctx, F_Tag) = Cursor (Ctx, F_Tag)'Old
          and Cursor (Ctx, F_Legacy_Flags) = Cursor (Ctx, F_Legacy_Flags)'Old
          and Cursor (Ctx, F_Has_Parent) = Cursor (Ctx, F_Has_Parent)'Old
          and Cursor (Ctx, F_Flags) = Cursor (Ctx, F_Flags)'Old
          and Cursor (Ctx, F_FD) = Cursor (Ctx, F_FD)'Old
          and Cursor (Ctx, F_Num_FDs) = Cursor (Ctx, F_Num_FDs)'Old
          and Cursor (Ctx, F_Padding) = Cursor (Ctx, F_Padding)'Old
          and Cursor (Ctx, F_Binder) = Cursor (Ctx, F_Binder)'Old
          and Cursor (Ctx, F_Handle) = Cursor (Ctx, F_Handle)'Old
          and Cursor (Ctx, F_Parent) = Cursor (Ctx, F_Parent)'Old
          and Cursor (Ctx, F_Buffer) = Cursor (Ctx, F_Buffer)'Old
          and Cursor (Ctx, F_Unused_Padding) = Cursor (Ctx, F_Unused_Padding)'Old
          and Cursor (Ctx, F_Parent_Offset) = Cursor (Ctx, F_Parent_Offset)'Old
          and Cursor (Ctx, F_Length) = Cursor (Ctx, F_Length)'Old
          and Cursor (Ctx, F_Cookie) = Cursor (Ctx, F_Cookie)'Old;

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
         when F_Kind =>
            Valid (Val.Kind_Value),
         when F_Arity =>
            Valid (Val.Arity_Value),
         when F_Tag =>
            Valid (Val.Tag_Value),
         when F_Legacy_Flags =>
            Valid (Val.Legacy_Flags_Value),
         when F_Has_Parent =>
            Valid (Val.Has_Parent_Value),
         when F_Flags =>
            Valid (Val.Flags_Value),
         when F_FD =>
            Valid (Val.FD_Value),
         when F_Num_FDs =>
            Valid (Val.Num_FDs_Value),
         when F_Padding =>
            Valid (Val.Padding_Value),
         when F_Binder =>
            Valid (Val.Binder_Value),
         when F_Handle =>
            Valid (Val.Handle_Value),
         when F_Parent =>
            Valid (Val.Parent_Value),
         when F_Buffer =>
            Valid (Val.Buffer_Value),
         when F_Unused_Padding =>
            Valid (Val.Unused_Padding_Value),
         when F_Parent_Offset =>
            Valid (Val.Parent_Offset_Value),
         when F_Length =>
            Valid (Val.Length_Value),
         when F_Cookie =>
            Valid (Val.Cookie_Value),
         when F_Index =>
            Valid (Val.Index_Value),
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
      and then ((if Structural_Valid (Cursors (F_Arity)) then
           (Valid (Cursors (F_Kind))
               and then Cursors (F_Arity).Predecessor = F_Kind))
        and then (if Structural_Valid (Cursors (F_Tag)) then
           (Valid (Cursors (F_Arity))
               and then Cursors (F_Tag).Predecessor = F_Arity
               and then (Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                 or (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                   and Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))))
        and then (if Structural_Valid (Cursors (F_Legacy_Flags)) then
           (Valid (Cursors (F_Tag))
               and then Cursors (F_Legacy_Flags).Predecessor = F_Tag
               and then Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))))
        and then (if Structural_Valid (Cursors (F_Has_Parent)) then
           (Valid (Cursors (F_Tag))
               and then Cursors (F_Has_Parent).Predecessor = F_Tag
               and then Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))))
        and then (if Structural_Valid (Cursors (F_Flags)) then
           (Valid (Cursors (F_Tag))
               and then Cursors (F_Flags).Predecessor = F_Tag
               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                 and Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))))
        and then (if Structural_Valid (Cursors (F_FD)) then
           (Valid (Cursors (F_Legacy_Flags))
               and then Cursors (F_FD).Predecessor = F_Legacy_Flags
               and then Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))))
        and then (if Structural_Valid (Cursors (F_Num_FDs)) then
           (Valid (Cursors (F_Legacy_Flags))
               and then Cursors (F_Num_FDs).Predecessor = F_Legacy_Flags
               and then Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
        and then (if Structural_Valid (Cursors (F_Padding)) then
           (Valid (Cursors (F_Has_Parent))
               and then Cursors (F_Padding).Predecessor = F_Has_Parent))
        and then (if Structural_Valid (Cursors (F_Binder)) then
           (Valid (Cursors (F_Flags))
               and then Cursors (F_Binder).Predecessor = F_Flags
               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))))
        and then (if Structural_Valid (Cursors (F_Handle)) then
           (Valid (Cursors (F_Flags))
               and then Cursors (F_Handle).Predecessor = F_Flags
               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))))
        and then (if Structural_Valid (Cursors (F_Parent)) then
           (Valid (Cursors (F_Num_FDs))
               and then Cursors (F_Parent).Predecessor = F_Num_FDs))
        and then (if Structural_Valid (Cursors (F_Buffer)) then
           (Valid (Cursors (F_Padding))
               and then Cursors (F_Buffer).Predecessor = F_Padding))
        and then (if Structural_Valid (Cursors (F_Unused_Padding)) then
           (Valid (Cursors (F_Handle))
               and then Cursors (F_Unused_Padding).Predecessor = F_Handle))
        and then (if Structural_Valid (Cursors (F_Parent_Offset)) then
           (Valid (Cursors (F_Parent))
               and then Cursors (F_Parent_Offset).Predecessor = F_Parent))
        and then (if Structural_Valid (Cursors (F_Length)) then
           (Valid (Cursors (F_Buffer))
               and then Cursors (F_Length).Predecessor = F_Buffer))
        and then (if Structural_Valid (Cursors (F_Cookie)) then
           (Valid (Cursors (F_FD))
               and then Cursors (F_Cookie).Predecessor = F_FD)
             or (Valid (Cursors (F_Binder))
               and then Cursors (F_Cookie).Predecessor = F_Binder)
             or (Valid (Cursors (F_Unused_Padding))
               and then Cursors (F_Cookie).Predecessor = F_Unused_Padding))
        and then (if Structural_Valid (Cursors (F_Index)) then
           (Valid (Cursors (F_Length))
               and then Cursors (F_Index).Predecessor = F_Length
               and then Types.Bit_Length (Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)))))
      and then ((if Invalid (Cursors (F_Kind)) then
           Invalid (Cursors (F_Arity)))
        and then (if Invalid (Cursors (F_Arity)) then
           Invalid (Cursors (F_Tag)))
        and then (if Invalid (Cursors (F_Tag)) then
           Invalid (Cursors (F_Legacy_Flags)))
        and then (if Invalid (Cursors (F_Tag)) then
           Invalid (Cursors (F_Has_Parent)))
        and then (if Invalid (Cursors (F_Tag)) then
           Invalid (Cursors (F_Flags)))
        and then (if Invalid (Cursors (F_Legacy_Flags)) then
           Invalid (Cursors (F_FD)))
        and then (if Invalid (Cursors (F_Legacy_Flags)) then
           Invalid (Cursors (F_Num_FDs)))
        and then (if Invalid (Cursors (F_Has_Parent)) then
           Invalid (Cursors (F_Padding)))
        and then (if Invalid (Cursors (F_Flags)) then
           Invalid (Cursors (F_Binder)))
        and then (if Invalid (Cursors (F_Flags)) then
           Invalid (Cursors (F_Handle)))
        and then (if Invalid (Cursors (F_Num_FDs)) then
           Invalid (Cursors (F_Parent)))
        and then (if Invalid (Cursors (F_Padding)) then
           Invalid (Cursors (F_Buffer)))
        and then (if Invalid (Cursors (F_Handle)) then
           Invalid (Cursors (F_Unused_Padding)))
        and then (if Invalid (Cursors (F_Parent)) then
           Invalid (Cursors (F_Parent_Offset)))
        and then (if Invalid (Cursors (F_Buffer)) then
           Invalid (Cursors (F_Length)))
        and then (if Invalid (Cursors (F_FD))
             and then Invalid (Cursors (F_Binder))
             and then Invalid (Cursors (F_Unused_Padding)) then
           Invalid (Cursors (F_Cookie)))
        and then (if Invalid (Cursors (F_Length)) then
           Invalid (Cursors (F_Index))))
      and then (if Structural_Valid (Cursors (F_Kind)) then
         (Cursors (F_Kind).Last - Cursors (F_Kind).First + 1) = Protocol.Binder_Kind_Base'Size
           and then Cursors (F_Kind).Predecessor = F_Initial
           and then Cursors (F_Kind).First = First
           and then (if Structural_Valid (Cursors (F_Arity)) then
              (Cursors (F_Arity).Last - Cursors (F_Arity).First + 1) = Protocol.Binder_Arity_Base'Size
                and then Cursors (F_Arity).Predecessor = F_Kind
                and then Cursors (F_Arity).First = (Cursors (F_Kind).Last + 1)
                and then (if Structural_Valid (Cursors (F_Tag))
                     and then (Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                       or (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                         and Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                   (Cursors (F_Tag).Last - Cursors (F_Tag).First + 1) = Protocol.Binder_Tag_Base'Size
                     and then Cursors (F_Tag).Predecessor = F_Arity
                     and then Cursors (F_Tag).First = (Cursors (F_Arity).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Legacy_Flags))
                          and then Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                        (Cursors (F_Legacy_Flags).Last - Cursors (F_Legacy_Flags).First + 1) = Protocol.MBZ32_Base'Size
                          and then Cursors (F_Legacy_Flags).Predecessor = F_Tag
                          and then Cursors (F_Legacy_Flags).First = (Cursors (F_Tag).Last + 1)
                          and then (if Structural_Valid (Cursors (F_FD))
                               and then Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                             (Cursors (F_FD).Last - Cursors (F_FD).First + 1) = Protocol.Handle_Base'Size
                               and then Cursors (F_FD).Predecessor = F_Legacy_Flags
                               and then Cursors (F_FD).First = (Cursors (F_Legacy_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Cookie)) then
                                  (Cursors (F_Cookie).Last - Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                    and then Cursors (F_Cookie).Predecessor = F_FD
                                    and then Cursors (F_Cookie).First = (Cursors (F_FD).Last + 1)))
                          and then (if Structural_Valid (Cursors (F_Num_FDs))
                               and then Types.Bit_Length (Cursors (F_Arity).Value.Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                             (Cursors (F_Num_FDs).Last - Cursors (F_Num_FDs).First + 1) = Protocol.Count'Size
                               and then Cursors (F_Num_FDs).Predecessor = F_Legacy_Flags
                               and then Cursors (F_Num_FDs).First = (Cursors (F_Legacy_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Parent)) then
                                  (Cursors (F_Parent).Last - Cursors (F_Parent).First + 1) = Protocol.Index'Size
                                    and then Cursors (F_Parent).Predecessor = F_Num_FDs
                                    and then Cursors (F_Parent).First = (Cursors (F_Num_FDs).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Parent_Offset)) then
                                       (Cursors (F_Parent_Offset).Last - Cursors (F_Parent_Offset).First + 1) = Protocol.Offset_Base'Size
                                         and then Cursors (F_Parent_Offset).Predecessor = F_Parent
                                         and then Cursors (F_Parent_Offset).First = (Cursors (F_Parent).Last + 1)))))
                     and then (if Structural_Valid (Cursors (F_Has_Parent))
                          and then Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                        (Cursors (F_Has_Parent).Last - Cursors (F_Has_Parent).First + 1) = Builtin_Types.Boolean_Base'Size
                          and then Cursors (F_Has_Parent).Predecessor = F_Tag
                          and then Cursors (F_Has_Parent).First = (Cursors (F_Tag).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Padding)) then
                             (Cursors (F_Padding).Last - Cursors (F_Padding).First + 1) = Protocol.MBZ31_Base'Size
                               and then Cursors (F_Padding).Predecessor = F_Has_Parent
                               and then Cursors (F_Padding).First = (Cursors (F_Has_Parent).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Buffer)) then
                                  (Cursors (F_Buffer).Last - Cursors (F_Buffer).First + 1) = Protocol.Index'Size
                                    and then Cursors (F_Buffer).Predecessor = F_Padding
                                    and then Cursors (F_Buffer).First = (Cursors (F_Padding).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Length)) then
                                       (Cursors (F_Length).Last - Cursors (F_Length).First + 1) = Protocol.Length_Base'Size
                                         and then Cursors (F_Length).Predecessor = F_Buffer
                                         and then Cursors (F_Length).First = (Cursors (F_Buffer).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Index))
                                              and then Types.Bit_Length (Cursors (F_Has_Parent).Value.Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                                            (Cursors (F_Index).Last - Cursors (F_Index).First + 1) = Protocol.Index'Size
                                              and then Cursors (F_Index).Predecessor = F_Length
                                              and then Cursors (F_Index).First = (Cursors (F_Length).Last + 1))))))
                     and then (if Structural_Valid (Cursors (F_Flags))
                          and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                            and Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) /= Types.Bit_Length (Convert (BK_FD))) then
                        (Cursors (F_Flags).Last - Cursors (F_Flags).First + 1) = Protocol.Flat_Binder_Flags_Base'Size
                          and then Cursors (F_Flags).Predecessor = F_Tag
                          and then Cursors (F_Flags).First = (Cursors (F_Tag).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Binder))
                               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                             (Cursors (F_Binder).Last - Cursors (F_Binder).First + 1) = Protocol.Binder'Size
                               and then Cursors (F_Binder).Predecessor = F_Flags
                               and then Cursors (F_Binder).First = (Cursors (F_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Cookie)) then
                                  (Cursors (F_Cookie).Last - Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                    and then Cursors (F_Cookie).Predecessor = F_Binder
                                    and then Cursors (F_Cookie).First = (Cursors (F_Binder).Last + 1)))
                          and then (if Structural_Valid (Cursors (F_Handle))
                               and then (Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                 or Types.Bit_Length (Cursors (F_Kind).Value.Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                             (Cursors (F_Handle).Last - Cursors (F_Handle).First + 1) = Protocol.Handle_Base'Size
                               and then Cursors (F_Handle).Predecessor = F_Flags
                               and then Cursors (F_Handle).First = (Cursors (F_Flags).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Unused_Padding)) then
                                  (Cursors (F_Unused_Padding).Last - Cursors (F_Unused_Padding).First + 1) = Protocol.MBZ32_Base'Size
                                    and then Cursors (F_Unused_Padding).Predecessor = F_Handle
                                    and then Cursors (F_Unused_Padding).First = (Cursors (F_Handle).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Cookie)) then
                                       (Cursors (F_Cookie).Last - Cursors (F_Cookie).First + 1) = Protocol.Cookie'Size
                                         and then Cursors (F_Cookie).Predecessor = F_Unused_Padding
                                         and then Cursors (F_Cookie).First = (Cursors (F_Unused_Padding).Last + 1)))))))));

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

end Parpen.Protocol.Generic_IBinder;
