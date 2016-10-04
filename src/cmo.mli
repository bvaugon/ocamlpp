(*************************************************************************)
(*                                                                       *)
(*                               OCamlPP                                 *)
(*                                                                       *)
(*                            Benoit Vaugon                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

type constant =
| Const_int       of int
| Const_char      of char
| Const_string    of string * string option
| Const_float     of string
| Const_int32     of int32
| Const_int64     of int64
| Const_nativeint of nativeint

type ident = { stamp: int; name: string; mutable flags: int }

type structured_constant =
| Const_base        of constant
| Const_pointer     of int
| Const_block       of int * structured_constant list
| Const_float_array of string list
| Const_immstring   of string

type reloc_info =
| Reloc_literal   of structured_constant  (* structured constant    *)
| Reloc_getglobal of ident                (* reference to a global  *)
| Reloc_setglobal of ident                (* definition of a global *)
| Reloc_primitive of string               (* C primitive number     *)
    
type compilation_unit = {
  cu_name: string;                             (* Name of compilation unit         *)
  mutable cu_pos: int;                         (* Absolute position in file        *)
  cu_codesize: int;                            (* Size of code block               *)
  cu_reloc: (reloc_info * int) list;           (* Relocation information           *)
  cu_imports: (string * Digest.t option) list; (* Names and CRC of intfs imported  *)
  cu_primitives: string list;                  (* Primitives declared inside       *)
  mutable cu_force_link: bool;                 (* Must be linked even if unref'ed  *)
  mutable cu_debug: int;                       (* Position of debugging info, or 0 *)
  cu_debugsize: int;                           (* Length of debugging info         *)
}
