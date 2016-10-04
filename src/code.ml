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
  
module Int = struct type t = int let compare (x : t) (y : t) = compare x y end;;
module ISet = Set.Make (Int);;

let compute_ptrs instrs =
  let nb_instr = Array.length instrs in
  let nb_bc = instrs.(nb_instr - 1).addr + 1 in
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
  Array.iter grep_instr instrs;
  Array.iter affect_ptr instrs;
;;

let compute_function_starts instrs =
  let add acc ptr =
    let ind = ptr.pointed.index in
    if ind > 0 then
      match instrs.(ind - 1).bc with
        | Branch ptr' when ptr'.pointed.index > ind ->
          ISet.add ptr.pointed.addr (ISet.add ptr'.pointed.addr acc)
        | Restart ->
          if ind > 1 then
            match instrs.(ind - 2).bc with
              | Branch ptr' when ptr'.pointed.index > ind ->
                ISet.add instrs.(ind - 1).addr (ISet.add ptr'.pointed.addr acc)
              | _ ->
                ISet.add instrs.(ind - 1).addr acc
          else
            acc
        | _ ->
          ISet.add ptr.pointed.addr acc
    else
      acc
  in
  let f acc instr =
    match instr.bc with
      | Closure (_, ptr) -> add acc ptr
      | Closurerec (_, _, ptr, ptrs) -> Array.fold_left add (add acc ptr) ptrs
      | _ -> acc
  in
  Array.fold_left f ISet.empty instrs
;;

let parse_segment ic offset length =
  seek_in ic offset;
  let cpt = ref 0 in
  let nb_bc = length lsr 2 in
  let next_word =
    let buf4 = Bytes.create 4 in
    fun () ->
      incr cpt;
      if !cpt > nb_bc then raise End_of_file;
      really_input ic buf4 0 4;
      let res =
        (int_of_char (Bytes.get buf4 0)) lor (int_of_char (Bytes.get buf4 1) lsl 8) lor
          (int_of_char (Bytes.get buf4 2) lsl 16) lor (int_of_char (Bytes.get buf4 3) lsl 24)
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
      try Some (Instr.parse next_word)
      with End_of_file -> None
    with
      | Some bc ->
        let instr = {
          addr = addr;
          index = 0;
          bc = bc;
          is_pointed = false;
        } in
        f (i + 1) (instr :: acc)
      | None -> acc
  in
  let instrs = Array.of_list (f 0 []) in
  let nb_instr = Array.length instrs in
  for i = 0 to nb_instr / 2 - 1 do
    let s = nb_instr - i - 1 in
    let tmp = instrs.(i) in
    instrs.(i) <- instrs.(s);
    instrs.(s) <- tmp;
  done;
  compute_ptrs instrs; 
  Array.iteri (fun i instr -> instr.index <- i) instrs;
  (instrs, compute_function_starts instrs)
;;

let parse ic index =
  let (offset, length) =
    try Index.find_section index Index.Code
    with Not_found -> failwith "code section not found"
  in
  parse_segment ic offset length
;;

let print globnames oc (instrs, fstarts) =
  let f i instr =
    if ISet.mem instr.addr fstarts then Printf.fprintf oc "\n";
    Printf.fprintf oc "%-5d  %a\n" i (print_instr globnames) instr;
  in
  Printf.fprintf oc "\n\
******************\n\
***  Bytecode  ***\n\
******************\n\
\n\
";
  Array.iteri f instrs;
;;
