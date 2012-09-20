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

open Printf;;
open Cmo;;

let print_list p oc l =
  let rec f l =
    match l with
      | [] -> ()
      | [ last ] -> p oc last
      | x :: rest -> fprintf oc "%a ; " p x; f rest
  in
    fprintf oc "[ "; f l; fprintf oc "]";
;;

let print_vlist i p oc l =
  fprintf oc "[\n";
  List.iter (fprintf oc "%s%a;\n" (String.make (i+2) ' ') p) l;
  fprintf oc "%s]" (String.make i ' ');
;;

let print_ident oc { stamp = stamp; name = name; flags = flags } =
  fprintf oc "{ stamp = %d; name = %S; flags = %d }" stamp name flags
;;

let print_constant oc c =
  match c with
    | Const_int n -> fprintf oc "Const_int %d" n
    | Const_char c -> fprintf oc "Const_char %C" c
    | Const_string s -> fprintf oc "Const_string %S" s
    | Const_float f -> fprintf oc "Const_float %S" f
    | Const_int32 n -> fprintf oc "Const_int32 %ldl" n
    | Const_int64 n -> fprintf oc "Const_int64 %LdL" n
    | Const_nativeint n -> fprintf oc "Const_nativeint %ndn" n
;;

let rec print_structured_constant oc sc =
  match sc with
    | Const_base c -> fprintf oc "Const_base (%a)" print_constant c
    | Const_pointer n -> fprintf oc "Const_pointer %d" n
    | Const_block (n, scs) ->
        fprintf oc "Const_block (%d, %a)" n
          (print_list print_structured_constant) scs;
    | Const_float_array fs ->
        fprintf oc "Const_float_array %a"
          (print_list (fun oc s -> fprintf oc "%S" s)) fs;
    | Const_immstring s -> fprintf oc "Const_immstring %S" s
;;

let print_reloc_info oc ri =
  match ri with
    | Reloc_literal sc ->
        fprintf oc "Reloc_literal (%a)" print_structured_constant sc
    | Reloc_getglobal id ->
        fprintf oc "Reloc_getglobal %a" print_ident id
    | Reloc_setglobal id ->
        fprintf oc "Reloc_setglobal %a" print_ident id
    | Reloc_primitive s ->
        fprintf oc "Reloc_primitive %S" s
;;

let print_compilation_unit oc {
  cu_name = cu_name;
  cu_pos = cu_pos;
  cu_codesize = cu_codesize;
  cu_reloc = cu_reloc;
  cu_imports = cu_imports;
  cu_primitives = cu_primitives;
  cu_force_link = cu_force_link;
  cu_debug = cu_debug;
  cu_debugsize = cu_debugsize;
} =
  fprintf oc "\
**************************\n\
***  Compilation Unit  ***\n\
**************************\n\
\n\
{\n\
  cu_name = %S;\n\
  cu_pos = %d;\n\
  cu_codesize = %d;\n\
  cu_reloc = %a;\n\
  cu_imports = %a;\n\
  cu_primitives = %a;\n\
  cu_force_link = %b;\n\
  cu_debug = %d;\n\
  cu_debugsize = %d;\n\
}\n\
" cu_name cu_pos cu_codesize
    (print_vlist 2
       (fun oc (ri, n) -> fprintf oc "(%a, %d)" print_reloc_info ri n))
    cu_reloc
    (print_vlist 2 (fun oc (s, d) -> fprintf oc "(%S, %S)" s (Digest.to_hex d)))
    cu_imports (print_vlist 2 (fun oc s -> fprintf oc "%S" s)) cu_primitives
    cu_force_link cu_debug cu_debugsize;
  ignore (cu_reloc, cu_imports, cu_primitives);
;;

let print oc (ci, code) =
  print_compilation_unit oc ci;
  Code.print oc code;
;;
