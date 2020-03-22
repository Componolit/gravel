with Parpen.Builtin_Types;
with Parpen.Builtin_Types.Conversions;
use Parpen.Builtin_Types.Conversions;
with Parpen.Generic_Types;

generic
   with package Types is new Parpen.Generic_Types (<>);
package Parpen.Name_Service.Generic_Request_Add_Service with
  SPARK_Mode
is

   pragma Annotate (GNATprove, Terminating, Generic_Request_Add_Service);

   use type Types.Bytes, Types.Bytes_Ptr, Types.Index, Types.Length, Types.Bit_Index, Types.Bit_Length;

   type Virtual_Field is (F_Initial, F_Len, F_Name, F_Server_Kind, F_Server_Arity, F_Server_Tag, F_Server_Legacy_Flags, F_Server_Has_Parent, F_Server_Flags, F_Server_FD, F_Server_Num_FDs, F_Server_Padding, F_Server_Binder, F_Server_Handle, F_Server_Parent, F_Server_Buffer, F_Server_Unused_Padding, F_Server_Parent_Offset, F_Server_Length, F_Server_Cookie, F_Server_Index, F_Allow_Isolated, F_Dump_Flags, F_Final);

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
            when F_Initial | F_Name | F_Final =>
               null;
            when F_Len =>
               Len_Value : Name_Service.Len_Base;
            when F_Server_Kind =>
               Server_Kind_Value : Binder.Binder_Kind_Base;
            when F_Server_Arity =>
               Server_Arity_Value : Binder.Binder_Arity_Base;
            when F_Server_Tag =>
               Server_Tag_Value : Binder.Binder_Tag_Base;
            when F_Server_Legacy_Flags =>
               Server_Legacy_Flags_Value : Binder.MBZ32_Base;
            when F_Server_Has_Parent =>
               Server_Has_Parent_Value : Builtin_Types.Boolean_Base;
            when F_Server_Flags =>
               Server_Flags_Value : Binder.Flat_Binder_Flags_Base;
            when F_Server_FD =>
               Server_FD_Value : Binder.Handle_Base;
            when F_Server_Num_FDs =>
               Server_Num_FDs_Value : Binder.Count;
            when F_Server_Padding =>
               Server_Padding_Value : Binder.MBZ31_Base;
            when F_Server_Binder =>
               Server_Binder_Value : Binder.Value;
            when F_Server_Handle =>
               Server_Handle_Value : Binder.Handle_Base;
            when F_Server_Parent =>
               Server_Parent_Value : Binder.Index;
            when F_Server_Buffer =>
               Server_Buffer_Value : Binder.Index;
            when F_Server_Unused_Padding =>
               Server_Unused_Padding_Value : Binder.MBZ32_Base;
            when F_Server_Parent_Offset =>
               Server_Parent_Offset_Value : Binder.Offset;
            when F_Server_Length =>
               Server_Length_Value : Binder.Length_Base;
            when F_Server_Cookie =>
               Server_Cookie_Value : Binder.Cookie;
            when F_Server_Index =>
               Server_Index_Value : Binder.Index;
            when F_Allow_Isolated =>
               Allow_Isolated_Value : Builtin_Types.Boolean_Base;
            when F_Dump_Flags =>
               Dump_Flags_Value : Name_Service.Integer_Base;
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

   function Get_Len (Ctx : Context) return Name_Service.Len with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Len);

   function Get_Server_Kind (Ctx : Context) return Binder.Binder_Kind with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Kind);

   function Get_Server_Arity (Ctx : Context) return Binder.Binder_Arity with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Arity);

   function Get_Server_Tag (Ctx : Context) return Binder.Binder_Tag with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Tag);

   function Get_Server_Legacy_Flags (Ctx : Context) return Binder.MBZ32 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Legacy_Flags);

   function Get_Server_Has_Parent (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Has_Parent);

   function Get_Server_Flags (Ctx : Context) return Binder.Flat_Binder_Flags with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Flags);

   function Get_Server_FD (Ctx : Context) return Binder.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_FD);

   function Get_Server_Num_FDs (Ctx : Context) return Binder.Count with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Num_FDs);

   function Get_Server_Padding (Ctx : Context) return Binder.MBZ31 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Padding);

   function Get_Server_Binder (Ctx : Context) return Binder.Value with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Binder);

   function Get_Server_Handle (Ctx : Context) return Binder.Handle with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Handle);

   function Get_Server_Parent (Ctx : Context) return Binder.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Parent);

   function Get_Server_Buffer (Ctx : Context) return Binder.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Buffer);

   function Get_Server_Unused_Padding (Ctx : Context) return Binder.MBZ32 with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Unused_Padding);

   function Get_Server_Parent_Offset (Ctx : Context) return Binder.Offset with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Parent_Offset);

   function Get_Server_Length (Ctx : Context) return Binder.Length with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Length);

   function Get_Server_Cookie (Ctx : Context) return Binder.Cookie with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Cookie);

   function Get_Server_Index (Ctx : Context) return Binder.Index with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Server_Index);

   function Get_Allow_Isolated (Ctx : Context) return Boolean with
     Pre =>
       Valid_Context (Ctx)
          and Valid (Ctx, F_Allow_Isolated);

   function Get_Dump_Flags (Ctx : Context) return Name_Service.Integer with
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

   procedure Set_Len (Ctx : in out Context; Val : Name_Service.Len) with
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
          and Invalid (Ctx, F_Server_Kind)
          and Invalid (Ctx, F_Server_Arity)
          and Invalid (Ctx, F_Server_Tag)
          and Invalid (Ctx, F_Server_Legacy_Flags)
          and Invalid (Ctx, F_Server_Has_Parent)
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Name) = F_Len
            and Valid_Next (Ctx, F_Name))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Len) = Predecessor (Ctx, F_Len)'Old
          and Valid_Next (Ctx, F_Len) = Valid_Next (Ctx, F_Len)'Old;

   procedure Set_Server_Kind (Ctx : in out Context; Val : Binder.Binder_Kind) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Kind)
          and then Field_Last (Ctx, F_Server_Kind) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Kind, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Server_Kind) >= Field_Length (Ctx, F_Server_Kind),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Kind)
          and Get_Server_Kind (Ctx) = Val
          and Invalid (Ctx, F_Server_Arity)
          and Invalid (Ctx, F_Server_Tag)
          and Invalid (Ctx, F_Server_Legacy_Flags)
          and Invalid (Ctx, F_Server_Has_Parent)
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Arity) = F_Server_Kind
            and Valid_Next (Ctx, F_Server_Arity))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Kind) = Predecessor (Ctx, F_Server_Kind)'Old
          and Valid_Next (Ctx, F_Server_Kind) = Valid_Next (Ctx, F_Server_Kind)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old;

   procedure Set_Server_Arity (Ctx : in out Context; Val : Binder.Binder_Arity) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Arity)
          and then Field_Last (Ctx, F_Server_Arity) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Arity, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Server_Arity) >= Field_Length (Ctx, F_Server_Arity),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Arity)
          and Get_Server_Arity (Ctx) = Val
          and Invalid (Ctx, F_Server_Tag)
          and Invalid (Ctx, F_Server_Legacy_Flags)
          and Invalid (Ctx, F_Server_Has_Parent)
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (if Types.Bit_Length (Convert (Get_Server_Arity (Ctx))) = Types.Bit_Length (Convert (BA_SINGLE))
               or (Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) = Types.Bit_Length (Convert (BK_FD))
                 and Types.Bit_Length (Convert (Get_Server_Arity (Ctx))) = Types.Bit_Length (Convert (BA_ARRAY))) then
             Predecessor (Ctx, F_Server_Tag) = F_Server_Arity
               and Valid_Next (Ctx, F_Server_Tag))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Arity) = Predecessor (Ctx, F_Server_Arity)'Old
          and Valid_Next (Ctx, F_Server_Arity) = Valid_Next (Ctx, F_Server_Arity)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old;

   procedure Set_Server_Tag (Ctx : in out Context; Val : Binder.Binder_Tag) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Tag)
          and then Field_Last (Ctx, F_Server_Tag) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Tag, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Tag) >= Field_Length (Ctx, F_Server_Tag),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Tag)
          and Get_Server_Tag (Ctx) = Val
          and Invalid (Ctx, F_Server_Legacy_Flags)
          and Invalid (Ctx, F_Server_Has_Parent)
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (if Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) = Types.Bit_Length (Convert (BK_FD)) then
             Predecessor (Ctx, F_Server_Legacy_Flags) = F_Server_Tag
               and Valid_Next (Ctx, F_Server_Legacy_Flags))
          and (if Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) = Types.Bit_Length (Convert (BK_POINTER)) then
             Predecessor (Ctx, F_Server_Has_Parent) = F_Server_Tag
               and Valid_Next (Ctx, F_Server_Has_Parent))
          and (if Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) /= Types.Bit_Length (Convert (BK_POINTER))
               and Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) /= Types.Bit_Length (Convert (BK_FD)) then
             Predecessor (Ctx, F_Server_Flags) = F_Server_Tag
               and Valid_Next (Ctx, F_Server_Flags))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Tag) = Predecessor (Ctx, F_Server_Tag)'Old
          and Valid_Next (Ctx, F_Server_Tag) = Valid_Next (Ctx, F_Server_Tag)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old;

   procedure Set_Server_Legacy_Flags (Ctx : in out Context; Val : Binder.MBZ32) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Legacy_Flags)
          and then Field_Last (Ctx, F_Server_Legacy_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Legacy_Flags, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Legacy_Flags) >= Field_Length (Ctx, F_Server_Legacy_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Legacy_Flags)
          and Get_Server_Legacy_Flags (Ctx) = Val
          and Invalid (Ctx, F_Server_Has_Parent)
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (if Types.Bit_Length (Convert (Get_Server_Arity (Ctx))) = Types.Bit_Length (Convert (BA_SINGLE)) then
             Predecessor (Ctx, F_Server_FD) = F_Server_Legacy_Flags
               and Valid_Next (Ctx, F_Server_FD))
          and (if Types.Bit_Length (Convert (Get_Server_Arity (Ctx))) = Types.Bit_Length (Convert (BA_ARRAY)) then
             Predecessor (Ctx, F_Server_Num_FDs) = F_Server_Legacy_Flags
               and Valid_Next (Ctx, F_Server_Num_FDs))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Legacy_Flags) = Predecessor (Ctx, F_Server_Legacy_Flags)'Old
          and Valid_Next (Ctx, F_Server_Legacy_Flags) = Valid_Next (Ctx, F_Server_Legacy_Flags)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old;

   procedure Set_Server_Has_Parent (Ctx : in out Context; Val : Boolean) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Has_Parent)
          and then Field_Last (Ctx, F_Server_Has_Parent) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Has_Parent, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Server_Has_Parent) >= Field_Length (Ctx, F_Server_Has_Parent),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Has_Parent)
          and Get_Server_Has_Parent (Ctx) = Val
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Padding) = F_Server_Has_Parent
            and Valid_Next (Ctx, F_Server_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Has_Parent) = Predecessor (Ctx, F_Server_Has_Parent)'Old
          and Valid_Next (Ctx, F_Server_Has_Parent) = Valid_Next (Ctx, F_Server_Has_Parent)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old;

   procedure Set_Server_Flags (Ctx : in out Context; Val : Binder.Flat_Binder_Flags) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Flags)
          and then Field_Last (Ctx, F_Server_Flags) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Flags, Convert (Val)))
          and then True
          and then Available_Space (Ctx, F_Server_Flags) >= Field_Length (Ctx, F_Server_Flags),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Flags)
          and Get_Server_Flags (Ctx) = Val
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (if Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
               or Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_BINDER)) then
             Predecessor (Ctx, F_Server_Binder) = F_Server_Flags
               and Valid_Next (Ctx, F_Server_Binder))
          and (if Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
               or Types.Bit_Length (Convert (Get_Server_Kind (Ctx))) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)) then
             Predecessor (Ctx, F_Server_Handle) = F_Server_Flags
               and Valid_Next (Ctx, F_Server_Handle))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Flags) = Predecessor (Ctx, F_Server_Flags)'Old
          and Valid_Next (Ctx, F_Server_Flags) = Valid_Next (Ctx, F_Server_Flags)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old;

   procedure Set_Server_FD (Ctx : in out Context; Val : Binder.Handle) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_FD)
          and then Field_Last (Ctx, F_Server_FD) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_FD, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_FD) >= Field_Length (Ctx, F_Server_FD),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_FD)
          and Get_Server_FD (Ctx) = Val
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Cookie) = F_Server_FD
            and Valid_Next (Ctx, F_Server_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_FD) = Predecessor (Ctx, F_Server_FD)'Old
          and Valid_Next (Ctx, F_Server_FD) = Valid_Next (Ctx, F_Server_FD)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Legacy_Flags (Ctx) = Get_Server_Legacy_Flags (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old;

   procedure Set_Server_Num_FDs (Ctx : in out Context; Val : Binder.Count) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Num_FDs)
          and then Field_Last (Ctx, F_Server_Num_FDs) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Num_FDs, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Num_FDs) >= Field_Length (Ctx, F_Server_Num_FDs),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Num_FDs)
          and Get_Server_Num_FDs (Ctx) = Val
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Parent) = F_Server_Num_FDs
            and Valid_Next (Ctx, F_Server_Parent))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Num_FDs) = Predecessor (Ctx, F_Server_Num_FDs)'Old
          and Valid_Next (Ctx, F_Server_Num_FDs) = Valid_Next (Ctx, F_Server_Num_FDs)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Legacy_Flags (Ctx) = Get_Server_Legacy_Flags (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old;

   procedure Set_Server_Padding (Ctx : in out Context; Val : Binder.MBZ31) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Padding)
          and then Field_Last (Ctx, F_Server_Padding) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Padding, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Padding) >= Field_Length (Ctx, F_Server_Padding),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Padding)
          and Get_Server_Padding (Ctx) = Val
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Buffer) = F_Server_Padding
            and Valid_Next (Ctx, F_Server_Buffer))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Padding) = Predecessor (Ctx, F_Server_Padding)'Old
          and Valid_Next (Ctx, F_Server_Padding) = Valid_Next (Ctx, F_Server_Padding)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Has_Parent (Ctx) = Get_Server_Has_Parent (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old;

   procedure Set_Server_Binder (Ctx : in out Context; Val : Binder.Value) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Binder)
          and then Field_Last (Ctx, F_Server_Binder) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Binder, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Binder) >= Field_Length (Ctx, F_Server_Binder),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Binder)
          and Get_Server_Binder (Ctx) = Val
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Cookie) = F_Server_Binder
            and Valid_Next (Ctx, F_Server_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Binder) = Predecessor (Ctx, F_Server_Binder)'Old
          and Valid_Next (Ctx, F_Server_Binder) = Valid_Next (Ctx, F_Server_Binder)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Flags (Ctx) = Get_Server_Flags (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old;

   procedure Set_Server_Handle (Ctx : in out Context; Val : Binder.Handle) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Handle)
          and then Field_Last (Ctx, F_Server_Handle) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Handle, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Handle) >= Field_Length (Ctx, F_Server_Handle),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Handle)
          and Get_Server_Handle (Ctx) = Val
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Unused_Padding) = F_Server_Handle
            and Valid_Next (Ctx, F_Server_Unused_Padding))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Handle) = Predecessor (Ctx, F_Server_Handle)'Old
          and Valid_Next (Ctx, F_Server_Handle) = Valid_Next (Ctx, F_Server_Handle)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Flags (Ctx) = Get_Server_Flags (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old;

   procedure Set_Server_Parent (Ctx : in out Context; Val : Binder.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Parent)
          and then Field_Last (Ctx, F_Server_Parent) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Parent, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Parent) >= Field_Length (Ctx, F_Server_Parent),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Parent)
          and Get_Server_Parent (Ctx) = Val
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Parent_Offset) = F_Server_Parent
            and Valid_Next (Ctx, F_Server_Parent_Offset))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Parent) = Predecessor (Ctx, F_Server_Parent)'Old
          and Valid_Next (Ctx, F_Server_Parent) = Valid_Next (Ctx, F_Server_Parent)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Legacy_Flags (Ctx) = Get_Server_Legacy_Flags (Ctx)'Old
          and Get_Server_Num_FDs (Ctx) = Get_Server_Num_FDs (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old;

   procedure Set_Server_Buffer (Ctx : in out Context; Val : Binder.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Buffer)
          and then Field_Last (Ctx, F_Server_Buffer) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Buffer, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Buffer) >= Field_Length (Ctx, F_Server_Buffer),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Buffer)
          and Get_Server_Buffer (Ctx) = Val
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Length) = F_Server_Buffer
            and Valid_Next (Ctx, F_Server_Length))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Buffer) = Predecessor (Ctx, F_Server_Buffer)'Old
          and Valid_Next (Ctx, F_Server_Buffer) = Valid_Next (Ctx, F_Server_Buffer)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Has_Parent (Ctx) = Get_Server_Has_Parent (Ctx)'Old
          and Get_Server_Padding (Ctx) = Get_Server_Padding (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old;

   procedure Set_Server_Unused_Padding (Ctx : in out Context; Val : Binder.MBZ32) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Unused_Padding)
          and then Field_Last (Ctx, F_Server_Unused_Padding) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Unused_Padding, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Unused_Padding) >= Field_Length (Ctx, F_Server_Unused_Padding),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Unused_Padding)
          and Get_Server_Unused_Padding (Ctx) = Val
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Cookie) = F_Server_Unused_Padding
            and Valid_Next (Ctx, F_Server_Cookie))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Unused_Padding) = Predecessor (Ctx, F_Server_Unused_Padding)'Old
          and Valid_Next (Ctx, F_Server_Unused_Padding) = Valid_Next (Ctx, F_Server_Unused_Padding)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Flags (Ctx) = Get_Server_Flags (Ctx)'Old
          and Get_Server_Handle (Ctx) = Get_Server_Handle (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old
          and Cursor (Ctx, F_Server_Buffer) = Cursor (Ctx, F_Server_Buffer)'Old;

   procedure Set_Server_Parent_Offset (Ctx : in out Context; Val : Binder.Offset) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Parent_Offset)
          and then Field_Last (Ctx, F_Server_Parent_Offset) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Parent_Offset, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Parent_Offset) >= Field_Length (Ctx, F_Server_Parent_Offset),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Parent_Offset)
          and Get_Server_Parent_Offset (Ctx) = Val
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Allow_Isolated) = F_Server_Parent_Offset
            and Valid_Next (Ctx, F_Allow_Isolated))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Parent_Offset) = Predecessor (Ctx, F_Server_Parent_Offset)'Old
          and Valid_Next (Ctx, F_Server_Parent_Offset) = Valid_Next (Ctx, F_Server_Parent_Offset)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Legacy_Flags (Ctx) = Get_Server_Legacy_Flags (Ctx)'Old
          and Get_Server_Num_FDs (Ctx) = Get_Server_Num_FDs (Ctx)'Old
          and Get_Server_Parent (Ctx) = Get_Server_Parent (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old
          and Cursor (Ctx, F_Server_Buffer) = Cursor (Ctx, F_Server_Buffer)'Old
          and Cursor (Ctx, F_Server_Unused_Padding) = Cursor (Ctx, F_Server_Unused_Padding)'Old;

   procedure Set_Server_Length (Ctx : in out Context; Val : Binder.Length) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Length)
          and then Field_Last (Ctx, F_Server_Length) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Length, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Length) >= Field_Length (Ctx, F_Server_Length),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Length)
          and Get_Server_Length (Ctx) = Val
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (if Types.Bit_Length (Convert (Get_Server_Has_Parent (Ctx))) = Types.Bit_Length (Convert (False)) then
             Predecessor (Ctx, F_Allow_Isolated) = F_Server_Length
               and Valid_Next (Ctx, F_Allow_Isolated))
          and (if Types.Bit_Length (Convert (Get_Server_Has_Parent (Ctx))) = Types.Bit_Length (Convert (True)) then
             Predecessor (Ctx, F_Server_Index) = F_Server_Length
               and Valid_Next (Ctx, F_Server_Index))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Length) = Predecessor (Ctx, F_Server_Length)'Old
          and Valid_Next (Ctx, F_Server_Length) = Valid_Next (Ctx, F_Server_Length)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Has_Parent (Ctx) = Get_Server_Has_Parent (Ctx)'Old
          and Get_Server_Padding (Ctx) = Get_Server_Padding (Ctx)'Old
          and Get_Server_Buffer (Ctx) = Get_Server_Buffer (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old
          and Cursor (Ctx, F_Server_Buffer) = Cursor (Ctx, F_Server_Buffer)'Old
          and Cursor (Ctx, F_Server_Unused_Padding) = Cursor (Ctx, F_Server_Unused_Padding)'Old
          and Cursor (Ctx, F_Server_Parent_Offset) = Cursor (Ctx, F_Server_Parent_Offset)'Old;

   procedure Set_Server_Cookie (Ctx : in out Context; Val : Binder.Cookie) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Cookie)
          and then Field_Last (Ctx, F_Server_Cookie) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Cookie, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Cookie) >= Field_Length (Ctx, F_Server_Cookie),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Cookie)
          and Get_Server_Cookie (Ctx) = Val
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Allow_Isolated) = F_Server_Cookie
            and Valid_Next (Ctx, F_Allow_Isolated))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Cookie) = Predecessor (Ctx, F_Server_Cookie)'Old
          and Valid_Next (Ctx, F_Server_Cookie) = Valid_Next (Ctx, F_Server_Cookie)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old
          and Cursor (Ctx, F_Server_Buffer) = Cursor (Ctx, F_Server_Buffer)'Old
          and Cursor (Ctx, F_Server_Unused_Padding) = Cursor (Ctx, F_Server_Unused_Padding)'Old
          and Cursor (Ctx, F_Server_Parent_Offset) = Cursor (Ctx, F_Server_Parent_Offset)'Old
          and Cursor (Ctx, F_Server_Length) = Cursor (Ctx, F_Server_Length)'Old;

   procedure Set_Server_Index (Ctx : in out Context; Val : Binder.Index) with
     Pre =>
       Valid_Context (Ctx)
          and then not Ctx'Constrained
          and then Has_Buffer (Ctx)
          and then Valid_Next (Ctx, F_Server_Index)
          and then Field_Last (Ctx, F_Server_Index) <= Types.Bit_Index'Last / 2
          and then Field_Condition (Ctx, (F_Server_Index, Val))
          and then Valid (Val)
          and then Available_Space (Ctx, F_Server_Index) >= Field_Length (Ctx, F_Server_Index),
     Post =>
       Valid_Context (Ctx)
          and Has_Buffer (Ctx)
          and Valid (Ctx, F_Server_Index)
          and Get_Server_Index (Ctx) = Val
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Allow_Isolated) = F_Server_Index
            and Valid_Next (Ctx, F_Allow_Isolated))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Server_Index) = Predecessor (Ctx, F_Server_Index)'Old
          and Valid_Next (Ctx, F_Server_Index) = Valid_Next (Ctx, F_Server_Index)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Server_Has_Parent (Ctx) = Get_Server_Has_Parent (Ctx)'Old
          and Get_Server_Padding (Ctx) = Get_Server_Padding (Ctx)'Old
          and Get_Server_Buffer (Ctx) = Get_Server_Buffer (Ctx)'Old
          and Get_Server_Length (Ctx) = Get_Server_Length (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old
          and Cursor (Ctx, F_Server_Buffer) = Cursor (Ctx, F_Server_Buffer)'Old
          and Cursor (Ctx, F_Server_Unused_Padding) = Cursor (Ctx, F_Server_Unused_Padding)'Old
          and Cursor (Ctx, F_Server_Parent_Offset) = Cursor (Ctx, F_Server_Parent_Offset)'Old
          and Cursor (Ctx, F_Server_Length) = Cursor (Ctx, F_Server_Length)'Old
          and Cursor (Ctx, F_Server_Cookie) = Cursor (Ctx, F_Server_Cookie)'Old;

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
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old
          and Cursor (Ctx, F_Server_Buffer) = Cursor (Ctx, F_Server_Buffer)'Old
          and Cursor (Ctx, F_Server_Unused_Padding) = Cursor (Ctx, F_Server_Unused_Padding)'Old
          and Cursor (Ctx, F_Server_Parent_Offset) = Cursor (Ctx, F_Server_Parent_Offset)'Old
          and Cursor (Ctx, F_Server_Length) = Cursor (Ctx, F_Server_Length)'Old
          and Cursor (Ctx, F_Server_Cookie) = Cursor (Ctx, F_Server_Cookie)'Old
          and Cursor (Ctx, F_Server_Index) = Cursor (Ctx, F_Server_Index)'Old;

   procedure Set_Dump_Flags (Ctx : in out Context; Val : Name_Service.Integer) with
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
          and Get_Server_Kind (Ctx) = Get_Server_Kind (Ctx)'Old
          and Get_Server_Arity (Ctx) = Get_Server_Arity (Ctx)'Old
          and Get_Server_Tag (Ctx) = Get_Server_Tag (Ctx)'Old
          and Get_Allow_Isolated (Ctx) = Get_Allow_Isolated (Ctx)'Old
          and Cursor (Ctx, F_Len) = Cursor (Ctx, F_Len)'Old
          and Cursor (Ctx, F_Name) = Cursor (Ctx, F_Name)'Old
          and Cursor (Ctx, F_Server_Kind) = Cursor (Ctx, F_Server_Kind)'Old
          and Cursor (Ctx, F_Server_Arity) = Cursor (Ctx, F_Server_Arity)'Old
          and Cursor (Ctx, F_Server_Tag) = Cursor (Ctx, F_Server_Tag)'Old
          and Cursor (Ctx, F_Server_Legacy_Flags) = Cursor (Ctx, F_Server_Legacy_Flags)'Old
          and Cursor (Ctx, F_Server_Has_Parent) = Cursor (Ctx, F_Server_Has_Parent)'Old
          and Cursor (Ctx, F_Server_Flags) = Cursor (Ctx, F_Server_Flags)'Old
          and Cursor (Ctx, F_Server_FD) = Cursor (Ctx, F_Server_FD)'Old
          and Cursor (Ctx, F_Server_Num_FDs) = Cursor (Ctx, F_Server_Num_FDs)'Old
          and Cursor (Ctx, F_Server_Padding) = Cursor (Ctx, F_Server_Padding)'Old
          and Cursor (Ctx, F_Server_Binder) = Cursor (Ctx, F_Server_Binder)'Old
          and Cursor (Ctx, F_Server_Handle) = Cursor (Ctx, F_Server_Handle)'Old
          and Cursor (Ctx, F_Server_Parent) = Cursor (Ctx, F_Server_Parent)'Old
          and Cursor (Ctx, F_Server_Buffer) = Cursor (Ctx, F_Server_Buffer)'Old
          and Cursor (Ctx, F_Server_Unused_Padding) = Cursor (Ctx, F_Server_Unused_Padding)'Old
          and Cursor (Ctx, F_Server_Parent_Offset) = Cursor (Ctx, F_Server_Parent_Offset)'Old
          and Cursor (Ctx, F_Server_Length) = Cursor (Ctx, F_Server_Length)'Old
          and Cursor (Ctx, F_Server_Cookie) = Cursor (Ctx, F_Server_Cookie)'Old
          and Cursor (Ctx, F_Server_Index) = Cursor (Ctx, F_Server_Index)'Old
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
          and Invalid (Ctx, F_Server_Kind)
          and Invalid (Ctx, F_Server_Arity)
          and Invalid (Ctx, F_Server_Tag)
          and Invalid (Ctx, F_Server_Legacy_Flags)
          and Invalid (Ctx, F_Server_Has_Parent)
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Kind) = F_Name
            and Valid_Next (Ctx, F_Server_Kind))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Name) = Predecessor (Ctx, F_Name)'Old
          and Valid_Next (Ctx, F_Name) = Valid_Next (Ctx, F_Name)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Structural_Valid (Ctx, F_Name);

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
          and Invalid (Ctx, F_Server_Kind)
          and Invalid (Ctx, F_Server_Arity)
          and Invalid (Ctx, F_Server_Tag)
          and Invalid (Ctx, F_Server_Legacy_Flags)
          and Invalid (Ctx, F_Server_Has_Parent)
          and Invalid (Ctx, F_Server_Flags)
          and Invalid (Ctx, F_Server_FD)
          and Invalid (Ctx, F_Server_Num_FDs)
          and Invalid (Ctx, F_Server_Padding)
          and Invalid (Ctx, F_Server_Binder)
          and Invalid (Ctx, F_Server_Handle)
          and Invalid (Ctx, F_Server_Parent)
          and Invalid (Ctx, F_Server_Buffer)
          and Invalid (Ctx, F_Server_Unused_Padding)
          and Invalid (Ctx, F_Server_Parent_Offset)
          and Invalid (Ctx, F_Server_Length)
          and Invalid (Ctx, F_Server_Cookie)
          and Invalid (Ctx, F_Server_Index)
          and Invalid (Ctx, F_Allow_Isolated)
          and Invalid (Ctx, F_Dump_Flags)
          and (Predecessor (Ctx, F_Server_Kind) = F_Name
            and Valid_Next (Ctx, F_Server_Kind))
          and Ctx.Buffer_First = Ctx.Buffer_First'Old
          and Ctx.Buffer_Last = Ctx.Buffer_Last'Old
          and Ctx.First = Ctx.First'Old
          and Predecessor (Ctx, F_Name) = Predecessor (Ctx, F_Name)'Old
          and Valid_Next (Ctx, F_Name) = Valid_Next (Ctx, F_Name)'Old
          and Get_Len (Ctx) = Get_Len (Ctx)'Old
          and Structural_Valid (Ctx, F_Name);

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
         when F_Name =>
            True,
         when F_Server_Kind =>
            Valid (Val.Server_Kind_Value),
         when F_Server_Arity =>
            Valid (Val.Server_Arity_Value),
         when F_Server_Tag =>
            Valid (Val.Server_Tag_Value),
         when F_Server_Legacy_Flags =>
            Valid (Val.Server_Legacy_Flags_Value),
         when F_Server_Has_Parent =>
            Valid (Val.Server_Has_Parent_Value),
         when F_Server_Flags =>
            Valid (Val.Server_Flags_Value),
         when F_Server_FD =>
            Valid (Val.Server_FD_Value),
         when F_Server_Num_FDs =>
            Valid (Val.Server_Num_FDs_Value),
         when F_Server_Padding =>
            Valid (Val.Server_Padding_Value),
         when F_Server_Binder =>
            Valid (Val.Server_Binder_Value),
         when F_Server_Handle =>
            Valid (Val.Server_Handle_Value),
         when F_Server_Parent =>
            Valid (Val.Server_Parent_Value),
         when F_Server_Buffer =>
            Valid (Val.Server_Buffer_Value),
         when F_Server_Unused_Padding =>
            Valid (Val.Server_Unused_Padding_Value),
         when F_Server_Parent_Offset =>
            Valid (Val.Server_Parent_Offset_Value),
         when F_Server_Length =>
            Valid (Val.Server_Length_Value),
         when F_Server_Cookie =>
            Valid (Val.Server_Cookie_Value),
         when F_Server_Index =>
            Valid (Val.Server_Index_Value),
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
        and then (if Structural_Valid (Cursors (F_Server_Kind)) then
           (Structural_Valid (Cursors (F_Name))
               and then Cursors (F_Server_Kind).Predecessor = F_Name))
        and then (if Structural_Valid (Cursors (F_Server_Arity)) then
           (Valid (Cursors (F_Server_Kind))
               and then Cursors (F_Server_Arity).Predecessor = F_Server_Kind))
        and then (if Structural_Valid (Cursors (F_Server_Tag)) then
           (Valid (Cursors (F_Server_Arity))
               and then Cursors (F_Server_Tag).Predecessor = F_Server_Arity
               and then (Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                 or (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                   and Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))))
        and then (if Structural_Valid (Cursors (F_Server_Legacy_Flags)) then
           (Valid (Cursors (F_Server_Tag))
               and then Cursors (F_Server_Legacy_Flags).Predecessor = F_Server_Tag
               and then Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))))
        and then (if Structural_Valid (Cursors (F_Server_Has_Parent)) then
           (Valid (Cursors (F_Server_Tag))
               and then Cursors (F_Server_Has_Parent).Predecessor = F_Server_Tag
               and then Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER))))
        and then (if Structural_Valid (Cursors (F_Server_Flags)) then
           (Valid (Cursors (F_Server_Tag))
               and then Cursors (F_Server_Flags).Predecessor = F_Server_Tag
               and then (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                 and Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD)))))
        and then (if Structural_Valid (Cursors (F_Server_FD)) then
           (Valid (Cursors (F_Server_Legacy_Flags))
               and then Cursors (F_Server_FD).Predecessor = F_Server_Legacy_Flags
               and then Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))))
        and then (if Structural_Valid (Cursors (F_Server_Num_FDs)) then
           (Valid (Cursors (F_Server_Legacy_Flags))
               and then Cursors (F_Server_Num_FDs).Predecessor = F_Server_Legacy_Flags
               and then Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY))))
        and then (if Structural_Valid (Cursors (F_Server_Padding)) then
           (Valid (Cursors (F_Server_Has_Parent))
               and then Cursors (F_Server_Padding).Predecessor = F_Server_Has_Parent))
        and then (if Structural_Valid (Cursors (F_Server_Binder)) then
           (Valid (Cursors (F_Server_Flags))
               and then Cursors (F_Server_Binder).Predecessor = F_Server_Flags
               and then (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                 or Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER)))))
        and then (if Structural_Valid (Cursors (F_Server_Handle)) then
           (Valid (Cursors (F_Server_Flags))
               and then Cursors (F_Server_Handle).Predecessor = F_Server_Flags
               and then (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                 or Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE)))))
        and then (if Structural_Valid (Cursors (F_Server_Parent)) then
           (Valid (Cursors (F_Server_Num_FDs))
               and then Cursors (F_Server_Parent).Predecessor = F_Server_Num_FDs))
        and then (if Structural_Valid (Cursors (F_Server_Buffer)) then
           (Valid (Cursors (F_Server_Padding))
               and then Cursors (F_Server_Buffer).Predecessor = F_Server_Padding))
        and then (if Structural_Valid (Cursors (F_Server_Unused_Padding)) then
           (Valid (Cursors (F_Server_Handle))
               and then Cursors (F_Server_Unused_Padding).Predecessor = F_Server_Handle))
        and then (if Structural_Valid (Cursors (F_Server_Parent_Offset)) then
           (Valid (Cursors (F_Server_Parent))
               and then Cursors (F_Server_Parent_Offset).Predecessor = F_Server_Parent))
        and then (if Structural_Valid (Cursors (F_Server_Length)) then
           (Valid (Cursors (F_Server_Buffer))
               and then Cursors (F_Server_Length).Predecessor = F_Server_Buffer))
        and then (if Structural_Valid (Cursors (F_Server_Cookie)) then
           (Valid (Cursors (F_Server_FD))
               and then Cursors (F_Server_Cookie).Predecessor = F_Server_FD)
             or (Valid (Cursors (F_Server_Binder))
               and then Cursors (F_Server_Cookie).Predecessor = F_Server_Binder)
             or (Valid (Cursors (F_Server_Unused_Padding))
               and then Cursors (F_Server_Cookie).Predecessor = F_Server_Unused_Padding))
        and then (if Structural_Valid (Cursors (F_Server_Index)) then
           (Valid (Cursors (F_Server_Length))
               and then Cursors (F_Server_Index).Predecessor = F_Server_Length
               and then Types.Bit_Length (Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True))))
        and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
           (Valid (Cursors (F_Server_Length))
               and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Length
               and then Types.Bit_Length (Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False)))
             or (Valid (Cursors (F_Server_Index))
               and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Index)
             or (Valid (Cursors (F_Server_Parent_Offset))
               and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Parent_Offset)
             or (Valid (Cursors (F_Server_Cookie))
               and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Cookie))
        and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
           (Valid (Cursors (F_Allow_Isolated))
               and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated)))
      and then ((if Invalid (Cursors (F_Len)) then
           Invalid (Cursors (F_Name)))
        and then (if Invalid (Cursors (F_Name)) then
           Invalid (Cursors (F_Server_Kind)))
        and then (if Invalid (Cursors (F_Server_Kind)) then
           Invalid (Cursors (F_Server_Arity)))
        and then (if Invalid (Cursors (F_Server_Arity)) then
           Invalid (Cursors (F_Server_Tag)))
        and then (if Invalid (Cursors (F_Server_Tag)) then
           Invalid (Cursors (F_Server_Legacy_Flags)))
        and then (if Invalid (Cursors (F_Server_Tag)) then
           Invalid (Cursors (F_Server_Has_Parent)))
        and then (if Invalid (Cursors (F_Server_Tag)) then
           Invalid (Cursors (F_Server_Flags)))
        and then (if Invalid (Cursors (F_Server_Legacy_Flags)) then
           Invalid (Cursors (F_Server_FD)))
        and then (if Invalid (Cursors (F_Server_Legacy_Flags)) then
           Invalid (Cursors (F_Server_Num_FDs)))
        and then (if Invalid (Cursors (F_Server_Has_Parent)) then
           Invalid (Cursors (F_Server_Padding)))
        and then (if Invalid (Cursors (F_Server_Flags)) then
           Invalid (Cursors (F_Server_Binder)))
        and then (if Invalid (Cursors (F_Server_Flags)) then
           Invalid (Cursors (F_Server_Handle)))
        and then (if Invalid (Cursors (F_Server_Num_FDs)) then
           Invalid (Cursors (F_Server_Parent)))
        and then (if Invalid (Cursors (F_Server_Padding)) then
           Invalid (Cursors (F_Server_Buffer)))
        and then (if Invalid (Cursors (F_Server_Handle)) then
           Invalid (Cursors (F_Server_Unused_Padding)))
        and then (if Invalid (Cursors (F_Server_Parent)) then
           Invalid (Cursors (F_Server_Parent_Offset)))
        and then (if Invalid (Cursors (F_Server_Buffer)) then
           Invalid (Cursors (F_Server_Length)))
        and then (if Invalid (Cursors (F_Server_FD))
             and then Invalid (Cursors (F_Server_Binder))
             and then Invalid (Cursors (F_Server_Unused_Padding)) then
           Invalid (Cursors (F_Server_Cookie)))
        and then (if Invalid (Cursors (F_Server_Length)) then
           Invalid (Cursors (F_Server_Index)))
        and then (if Invalid (Cursors (F_Server_Length))
             and then Invalid (Cursors (F_Server_Index))
             and then Invalid (Cursors (F_Server_Parent_Offset))
             and then Invalid (Cursors (F_Server_Cookie)) then
           Invalid (Cursors (F_Allow_Isolated)))
        and then (if Invalid (Cursors (F_Allow_Isolated)) then
           Invalid (Cursors (F_Dump_Flags))))
      and then (if Structural_Valid (Cursors (F_Len)) then
         (Cursors (F_Len).Last - Cursors (F_Len).First + 1) = Name_Service.Len_Base'Size
           and then Cursors (F_Len).Predecessor = F_Initial
           and then Cursors (F_Len).First = First
           and then (if Structural_Valid (Cursors (F_Name)) then
              (Cursors (F_Name).Last - Cursors (F_Name).First + 1) = Types.Bit_Length (Cursors (F_Len).Value.Len_Value)
                and then Cursors (F_Name).Predecessor = F_Len
                and then Cursors (F_Name).First = (Cursors (F_Len).Last + 1)
                and then (if Structural_Valid (Cursors (F_Server_Kind)) then
                   (Cursors (F_Server_Kind).Last - Cursors (F_Server_Kind).First + 1) = Binder.Binder_Kind_Base'Size
                     and then Cursors (F_Server_Kind).Predecessor = F_Name
                     and then Cursors (F_Server_Kind).First = (Cursors (F_Name).Last + 1)
                     and then (if Structural_Valid (Cursors (F_Server_Arity)) then
                        (Cursors (F_Server_Arity).Last - Cursors (F_Server_Arity).First + 1) = Binder.Binder_Arity_Base'Size
                          and then Cursors (F_Server_Arity).Predecessor = F_Server_Kind
                          and then Cursors (F_Server_Arity).First = (Cursors (F_Server_Kind).Last + 1)
                          and then (if Structural_Valid (Cursors (F_Server_Tag))
                               and then (Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE))
                                 or (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD))
                                   and Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)))) then
                             (Cursors (F_Server_Tag).Last - Cursors (F_Server_Tag).First + 1) = Binder.Binder_Tag_Base'Size
                               and then Cursors (F_Server_Tag).Predecessor = F_Server_Arity
                               and then Cursors (F_Server_Tag).First = (Cursors (F_Server_Arity).Last + 1)
                               and then (if Structural_Valid (Cursors (F_Server_Legacy_Flags))
                                    and then Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_FD)) then
                                  (Cursors (F_Server_Legacy_Flags).Last - Cursors (F_Server_Legacy_Flags).First + 1) = Binder.MBZ32_Base'Size
                                    and then Cursors (F_Server_Legacy_Flags).Predecessor = F_Server_Tag
                                    and then Cursors (F_Server_Legacy_Flags).First = (Cursors (F_Server_Tag).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Server_FD))
                                         and then Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_SINGLE)) then
                                       (Cursors (F_Server_FD).Last - Cursors (F_Server_FD).First + 1) = Binder.Handle_Base'Size
                                         and then Cursors (F_Server_FD).Predecessor = F_Server_Legacy_Flags
                                         and then Cursors (F_Server_FD).First = (Cursors (F_Server_Legacy_Flags).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Server_Cookie)) then
                                            (Cursors (F_Server_Cookie).Last - Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                              and then Cursors (F_Server_Cookie).Predecessor = F_Server_FD
                                              and then Cursors (F_Server_Cookie).First = (Cursors (F_Server_FD).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
                                                 (Cursors (F_Allow_Isolated).Last - Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                   and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Cookie
                                                   and then Cursors (F_Allow_Isolated).First = (Cursors (F_Server_Cookie).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
                                                      (Cursors (F_Dump_Flags).Last - Cursors (F_Dump_Flags).First + 1) = Name_Service.Integer_Base'Size
                                                        and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                        and then Cursors (F_Dump_Flags).First = (Cursors (F_Allow_Isolated).Last + 1)))))
                                    and then (if Structural_Valid (Cursors (F_Server_Num_FDs))
                                         and then Types.Bit_Length (Cursors (F_Server_Arity).Value.Server_Arity_Value) = Types.Bit_Length (Convert (BA_ARRAY)) then
                                       (Cursors (F_Server_Num_FDs).Last - Cursors (F_Server_Num_FDs).First + 1) = Binder.Count'Size
                                         and then Cursors (F_Server_Num_FDs).Predecessor = F_Server_Legacy_Flags
                                         and then Cursors (F_Server_Num_FDs).First = (Cursors (F_Server_Legacy_Flags).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Server_Parent)) then
                                            (Cursors (F_Server_Parent).Last - Cursors (F_Server_Parent).First + 1) = Binder.Index'Size
                                              and then Cursors (F_Server_Parent).Predecessor = F_Server_Num_FDs
                                              and then Cursors (F_Server_Parent).First = (Cursors (F_Server_Num_FDs).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Server_Parent_Offset)) then
                                                 (Cursors (F_Server_Parent_Offset).Last - Cursors (F_Server_Parent_Offset).First + 1) = Binder.Offset'Size
                                                   and then Cursors (F_Server_Parent_Offset).Predecessor = F_Server_Parent
                                                   and then Cursors (F_Server_Parent_Offset).First = (Cursors (F_Server_Parent).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
                                                      (Cursors (F_Allow_Isolated).Last - Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                        and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Parent_Offset
                                                        and then Cursors (F_Allow_Isolated).First = (Cursors (F_Server_Parent_Offset).Last + 1)
                                                        and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
                                                           (Cursors (F_Dump_Flags).Last - Cursors (F_Dump_Flags).First + 1) = Name_Service.Integer_Base'Size
                                                             and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                             and then Cursors (F_Dump_Flags).First = (Cursors (F_Allow_Isolated).Last + 1)))))))
                               and then (if Structural_Valid (Cursors (F_Server_Has_Parent))
                                    and then Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_POINTER)) then
                                  (Cursors (F_Server_Has_Parent).Last - Cursors (F_Server_Has_Parent).First + 1) = Builtin_Types.Boolean_Base'Size
                                    and then Cursors (F_Server_Has_Parent).Predecessor = F_Server_Tag
                                    and then Cursors (F_Server_Has_Parent).First = (Cursors (F_Server_Tag).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Server_Padding)) then
                                       (Cursors (F_Server_Padding).Last - Cursors (F_Server_Padding).First + 1) = Binder.MBZ31_Base'Size
                                         and then Cursors (F_Server_Padding).Predecessor = F_Server_Has_Parent
                                         and then Cursors (F_Server_Padding).First = (Cursors (F_Server_Has_Parent).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Server_Buffer)) then
                                            (Cursors (F_Server_Buffer).Last - Cursors (F_Server_Buffer).First + 1) = Binder.Index'Size
                                              and then Cursors (F_Server_Buffer).Predecessor = F_Server_Padding
                                              and then Cursors (F_Server_Buffer).First = (Cursors (F_Server_Padding).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Server_Length)) then
                                                 (Cursors (F_Server_Length).Last - Cursors (F_Server_Length).First + 1) = Binder.Length_Base'Size
                                                   and then Cursors (F_Server_Length).Predecessor = F_Server_Buffer
                                                   and then Cursors (F_Server_Length).First = (Cursors (F_Server_Buffer).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Allow_Isolated))
                                                        and then Types.Bit_Length (Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (False)) then
                                                      (Cursors (F_Allow_Isolated).Last - Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                        and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Length
                                                        and then Cursors (F_Allow_Isolated).First = (Cursors (F_Server_Length).Last + 1)
                                                        and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
                                                           (Cursors (F_Dump_Flags).Last - Cursors (F_Dump_Flags).First + 1) = Name_Service.Integer_Base'Size
                                                             and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                             and then Cursors (F_Dump_Flags).First = (Cursors (F_Allow_Isolated).Last + 1)))
                                                   and then (if Structural_Valid (Cursors (F_Server_Index))
                                                        and then Types.Bit_Length (Cursors (F_Server_Has_Parent).Value.Server_Has_Parent_Value) = Types.Bit_Length (Convert (True)) then
                                                      (Cursors (F_Server_Index).Last - Cursors (F_Server_Index).First + 1) = Binder.Index'Size
                                                        and then Cursors (F_Server_Index).Predecessor = F_Server_Length
                                                        and then Cursors (F_Server_Index).First = (Cursors (F_Server_Length).Last + 1)
                                                        and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
                                                           (Cursors (F_Allow_Isolated).Last - Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                             and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Index
                                                             and then Cursors (F_Allow_Isolated).First = (Cursors (F_Server_Index).Last + 1)
                                                             and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
                                                                (Cursors (F_Dump_Flags).Last - Cursors (F_Dump_Flags).First + 1) = Name_Service.Integer_Base'Size
                                                                  and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                                  and then Cursors (F_Dump_Flags).First = (Cursors (F_Allow_Isolated).Last + 1))))))))
                               and then (if Structural_Valid (Cursors (F_Server_Flags))
                                    and then (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_POINTER))
                                      and Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) /= Types.Bit_Length (Convert (BK_FD))) then
                                  (Cursors (F_Server_Flags).Last - Cursors (F_Server_Flags).First + 1) = Binder.Flat_Binder_Flags_Base'Size
                                    and then Cursors (F_Server_Flags).Predecessor = F_Server_Tag
                                    and then Cursors (F_Server_Flags).First = (Cursors (F_Server_Tag).Last + 1)
                                    and then (if Structural_Valid (Cursors (F_Server_Binder))
                                         and then (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_BINDER))
                                           or Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_BINDER))) then
                                       (Cursors (F_Server_Binder).Last - Cursors (F_Server_Binder).First + 1) = Binder.Value'Size
                                         and then Cursors (F_Server_Binder).Predecessor = F_Server_Flags
                                         and then Cursors (F_Server_Binder).First = (Cursors (F_Server_Flags).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Server_Cookie)) then
                                            (Cursors (F_Server_Cookie).Last - Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                              and then Cursors (F_Server_Cookie).Predecessor = F_Server_Binder
                                              and then Cursors (F_Server_Cookie).First = (Cursors (F_Server_Binder).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
                                                 (Cursors (F_Allow_Isolated).Last - Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                   and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Cookie
                                                   and then Cursors (F_Allow_Isolated).First = (Cursors (F_Server_Cookie).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
                                                      (Cursors (F_Dump_Flags).Last - Cursors (F_Dump_Flags).First + 1) = Name_Service.Integer_Base'Size
                                                        and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                        and then Cursors (F_Dump_Flags).First = (Cursors (F_Allow_Isolated).Last + 1)))))
                                    and then (if Structural_Valid (Cursors (F_Server_Handle))
                                         and then (Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_STRONG_HANDLE))
                                           or Types.Bit_Length (Cursors (F_Server_Kind).Value.Server_Kind_Value) = Types.Bit_Length (Convert (BK_WEAK_HANDLE))) then
                                       (Cursors (F_Server_Handle).Last - Cursors (F_Server_Handle).First + 1) = Binder.Handle_Base'Size
                                         and then Cursors (F_Server_Handle).Predecessor = F_Server_Flags
                                         and then Cursors (F_Server_Handle).First = (Cursors (F_Server_Flags).Last + 1)
                                         and then (if Structural_Valid (Cursors (F_Server_Unused_Padding)) then
                                            (Cursors (F_Server_Unused_Padding).Last - Cursors (F_Server_Unused_Padding).First + 1) = Binder.MBZ32_Base'Size
                                              and then Cursors (F_Server_Unused_Padding).Predecessor = F_Server_Handle
                                              and then Cursors (F_Server_Unused_Padding).First = (Cursors (F_Server_Handle).Last + 1)
                                              and then (if Structural_Valid (Cursors (F_Server_Cookie)) then
                                                 (Cursors (F_Server_Cookie).Last - Cursors (F_Server_Cookie).First + 1) = Binder.Cookie'Size
                                                   and then Cursors (F_Server_Cookie).Predecessor = F_Server_Unused_Padding
                                                   and then Cursors (F_Server_Cookie).First = (Cursors (F_Server_Unused_Padding).Last + 1)
                                                   and then (if Structural_Valid (Cursors (F_Allow_Isolated)) then
                                                      (Cursors (F_Allow_Isolated).Last - Cursors (F_Allow_Isolated).First + 1) = Builtin_Types.Boolean_Base'Size
                                                        and then Cursors (F_Allow_Isolated).Predecessor = F_Server_Cookie
                                                        and then Cursors (F_Allow_Isolated).First = (Cursors (F_Server_Cookie).Last + 1)
                                                        and then (if Structural_Valid (Cursors (F_Dump_Flags)) then
                                                           (Cursors (F_Dump_Flags).Last - Cursors (F_Dump_Flags).First + 1) = Name_Service.Integer_Base'Size
                                                             and then Cursors (F_Dump_Flags).Predecessor = F_Allow_Isolated
                                                             and then Cursors (F_Dump_Flags).First = (Cursors (F_Allow_Isolated).Last + 1)))))))))))));

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

end Parpen.Name_Service.Generic_Request_Add_Service;
