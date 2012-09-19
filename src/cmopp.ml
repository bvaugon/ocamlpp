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
  Printf.eprintf "Usage: %s [ -version ] <file.cmo>\n" Sys.argv.(0);
  exit 1;
;;

if Array.length Sys.argv <> 2 then usage ();;

if Sys.argv.(1) = "-version" then (
  print_endline Config.version;
  exit 0;
);;

try Cmoprinter.print stdout (Cmoparser.parse Sys.argv.(1));
with Failure msg -> error msg;
