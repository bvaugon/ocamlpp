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

open Instr

let compute_ptrs code =
  let nb_instr = Array.length code in
  let nb_bc = code.(nb_instr - 1).addr + 1 in
  let indirect = Array.make nb_bc None in
  let grep_instr instr =
    indirect.(instr.addr) <- Some instr;
  in
  let search bc_ind =
    if bc_ind < 0 || bc_ind >= nb_bc then failwith "invalid offset";
    match indirect.(bc_ind) with
      | None -> failwith "invalid offset";
      | Some instr -> instr
  in
  let affect_ptr instr =
    let update_pointed delta ptr =
      let pointed = search (instr.addr + delta + ptr.ofs) in
        ptr.pointed <- pointed;
        pointed.is_pointed <- true;
    in
      match instr.bc with
        | Pushretaddr ptr | Branch ptr | Branchif ptr | Branchifnot ptr
        | Pushtrap ptr ->
            update_pointed 1 ptr;

        | Closure (_, ptr) | Beq (_, ptr) | Bneq (_, ptr) | Blint (_, ptr)
        | Bleint (_, ptr) | Bgtint (_, ptr) | Bgeint (_, ptr) | Bultint (_, ptr)
        | Bugeint (_, ptr) ->
            update_pointed 2 ptr;

        | Closurerec (_, _, ptr, tab) ->
            update_pointed 3 ptr;
            Array.iter (update_pointed 3) tab;

        | Switch (_, tab) ->
            Array.iter (update_pointed 2) tab

        | _ -> ();
  in
    Array.iter grep_instr code;
    Array.iter affect_ptr code;
;;

let parse_segment ic offset length =
    seek_in ic offset;
    let cpt = ref 0 in
    let nb_bc = length lsr 2 in
    let read =
      let buf4 = String.create 4 in
        fun () ->
          incr cpt;
          if !cpt > nb_bc then raise End_of_file;
          really_input ic buf4 0 4;
          let res =
            (int_of_char buf4.[0]) lor (int_of_char buf4.[1] lsl 8) lor
            (int_of_char buf4.[2] lsl 16) lor (int_of_char buf4.[3] lsl 24)
          in
            match Sys.word_size with
              | 32 -> res
              | 64 -> (res lsl 32) asr 32
              | ws -> failwith (
                  Printf.sprintf "Unsupported architecture: \
word size is %d" ws)
    in
    let rec f i acc =
      let addr = !cpt in
        match
          try Some (Instr.parse read)
          with End_of_file -> None
        with
          | Some bc ->
              let instr = { addr = addr; bc = bc; is_pointed = false; } in
                f (i + 1) (instr :: acc)
          | None -> acc
    in
    let code = Array.of_list (f 0 []) in
    let nb_instr = Array.length code in
      for i = 0 to nb_instr / 2 - 1 do
        let s = nb_instr - i - 1 in
        let tmp = code.(i) in
          code.(i) <- code.(s);
          code.(s) <- tmp;
      done;
      compute_ptrs code;
      code
;;

let parse ic index =
  let (offset, length) =
    try Index.find_section index Index.Code
    with Not_found -> failwith "code section not found"
  in
    parse_segment ic offset length
;;

let print globnames oc code =
  Printf.fprintf oc "\n\
******************\n\
***  Bytecode  ***\n\
******************\n\
\n\
";
  Array.iteri
    (fun i -> Printf.fprintf oc "%-5d  %a\n" i (print_instr globnames))
    code;
;;
