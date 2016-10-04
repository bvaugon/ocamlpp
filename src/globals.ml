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

(* Utilities for finding the names of primitives and printing (part of)
   global values *)

open Cmo
open Data

type request = Primitive | Global
    
type globals =
  | Glob  of string array * elem array
  | Reloc of Cmo.compilation_unit
      
let string_of_constant c = match c with
  | Const_int n               -> Printf.sprintf "%d" n
  | Const_char c              -> Printf.sprintf "%C" c
  | Const_string (s, None)    -> string_of_string s
  | Const_string (s, Some s') -> Printf.sprintf "{%s|%s|%s}" s' (string_of_string s) s'
  | Const_float str           -> Printf.sprintf "%s" str
  | Const_int32 n             -> Printf.sprintf "%ldl" n
  | Const_int64 n             -> Printf.sprintf "%LdL" n
  | Const_nativeint ni        -> Printf.sprintf "%nx : nativeint" ni
    
let string_of_structured_constant sc =
  let rec show depth sc =
    if depth > 2 then "..." else
      match sc with
        | Const_base c -> string_of_constant c
        | Const_pointer p -> Printf.sprintf "ptr:%n" p
        | Const_block (tag, struct_consts) ->
          begin match struct_consts with
            | [] -> Printf.sprintf "[%d:%d| ]" tag 0
            | [c] ->
              Printf.sprintf "[%d:%d| %a ]" tag 1 (fun () -> show (depth + 1)) c
            | [c1; c2] ->
              Printf.sprintf "[%d:%d| %a, %a ]" tag 2 
                (fun () -> show (depth + 1)) c1
                (fun () -> show (depth + 1)) c2
            | c1 :: c2 :: _ ->
              Printf.sprintf "[%d:%d| %a, %a, ... ]" tag
                (List.length struct_consts)
                (fun () -> show (depth + 1)) c1
                (fun () -> show (depth + 1)) c2
          end
        | Const_float_array strlist ->
          begin match strlist with
            | [] -> "[| |]"
            | [s] -> Printf.sprintf "[| %s |]" s
            | [s1; s2] -> Printf.sprintf "[| %s; %s |]" s1 s2
            | s1 :: s2 :: _ -> Printf.sprintf "[| %s; %s; ... |]" s1 s2
          end
        | Const_immstring str -> Printf.sprintf "immutable:%S" str
  in
  show 0 sc

let rec find_primitive idx relocs = match relocs with
  | (Reloc_primitive s, i) :: locs ->
    if i = idx then s else find_primitive idx locs
  | (_, _) :: locs -> find_primitive idx locs
  | [] -> raise Not_found
    
let rec find_global idx relocs = match relocs with
  | (Reloc_getglobal g, i) :: locs ->
    if i = idx then g.name else find_global idx locs
  | (Reloc_literal lit, i) :: locs ->
    if i = idx then string_of_structured_constant lit
    else find_global idx locs
  | (_, _) :: locs -> find_global idx locs
  | _ -> raise Not_found
    
let find globals req slot code_idx =
  match globals with
    | Glob(prims, elems) ->
      begin match req with
        | Primitive -> prims.(slot)
        | Global -> string_of_elem elems.(slot)
      end
    | Reloc compunit ->
      let reloc = compunit.cu_reloc in
      let cidx = (code_idx + 1) * 4 in
      match req with
        | Primitive ->
          begin try find_primitive cidx reloc with Not_found ->
            Printf.sprintf "[Could not find primitive @%d (%d)]" code_idx cidx
          end
        | Global ->
          begin try find_global cidx reloc with Not_found ->
            Printf.sprintf "[Could not find global @%d (%d)]" code_idx cidx
          end
