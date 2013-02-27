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

type t = string array
    
let parse ic index =
  let (offset, length) =
    try Index.find_section index Index.Prim
    with Not_found -> failwith "prim section not found"
  in
  seek_in ic offset;
  let buf = Buffer.create 16 in
  let rec f i res =
    if i <> length then
      let c = input_char ic in
      if int_of_char c <> 0 then
        begin
          Buffer.add_char buf c;
          f (i + 1) res
        end
      else
        let name = Buffer.contents buf in
        Buffer.clear buf;
        f (i + 1) (name :: res)
    else if Buffer.length buf <> 0 then
      failwith "unexpected end of prim section"
    else
      res
  in
  Array.of_list (List.rev (f 0 []))
;;

let print oc prim =
  Printf.fprintf oc "\n\
*******************\n\
***  Externals  ***\n\
*******************\n\
\n\
";
  Array.iteri (Printf.fprintf oc "%-3d   %s\n") prim
;;
