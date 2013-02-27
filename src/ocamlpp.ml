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

let error msg =
  Printf.eprintf "Error: %s\n" msg;
  exit 1;
;;

let usage () =
  Printf.eprintf "Usage: %s [ -version ] (<file.cmo> | <file.byte>)\n"
    Sys.argv.(0);
  exit 1;
;;

if Array.length Sys.argv <> 2 then usage ();;

if Sys.argv.(1) = "-version" then (
  print_endline Config.version;
  exit 0;
);;

begin try
  let ((compunit, _code) as cmo) = Cmoparser.parse Sys.argv.(1) in
  Cmoprinter.print (Globals.find (Globals.Reloc compunit)) stdout cmo;
with Cmoparser.Not_a_cmo -> begin try
  let ic = open_in_bin Sys.argv.(1) in
  let index = Index.parse ic in Index.print stdout index;
  let prims = Prim.parse ic index in Prim.print stdout prims;
  let data = Data.parse ic index in Data.print stdout data;
  let code = Code.parse ic index in
  let globnames = Globals.find (Globals.Glob (prims, Array.of_list data)) in
  Code.print globnames stdout code;
  close_in ic;
with Index.Not_a_byte ->
  error "not a bytecode executable file nor an OCaml object file"
| Failure msg -> error msg end
| Failure msg -> error msg end
