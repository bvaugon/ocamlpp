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
  Printf.eprintf "Usage: %s [ -version ] <file.byte>\n" Sys.argv.(0);
  exit 1;
;;

if Array.length Sys.argv <> 2 then usage ();;

if Sys.argv.(1) = "-version" then (
  print_endline Config.version;
  exit 0;
);;

try
  let ic = open_in_bin Sys.argv.(1) in
  let index = Index.parse ic in Index.print stdout index;
  let prims = Prim.parse ic index in Prim.print stdout prims;
  let data = Data.parse ic index in Data.print stdout data;
  let code = Code.parse ic index in Code.print stdout code;
  close_in ic;
with Failure msg -> error msg;;
