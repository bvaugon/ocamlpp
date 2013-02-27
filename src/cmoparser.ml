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

exception Not_a_cmo

let cmo_magic_number = "Caml1999O007";;

let parse file_name =
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length cmo_magic_number) in
    really_input ic buffer 0 (String.length cmo_magic_number);
    if buffer = cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : Cmo.compilation_unit) in
      let code =
        Code.parse_segment ic compunit.Cmo.cu_pos compunit.Cmo.cu_codesize
      in
      close_in ic;
      (compunit, code)
    end else raise Not_a_cmo
  with
    | End_of_file -> close_in ic; raise Not_a_cmo
    | x -> close_in ic; raise x
;;
