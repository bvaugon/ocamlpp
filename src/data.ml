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

type elem =
  | String of string
  | Float of float
  | Floats of float array
  | Int of int
  | Int_32 of Int32.t
  | Int_64 of Int64.t
  | Custom of int array
  | Block of elem array * int
  | Closure of elem array
  | Out_of_heap of int
;;

let parse ic index =
  let (offset, _) =
    try Index.find_section index Index.Data
    with Not_found -> failwith "code section not found"
  in
    seek_in ic offset;
    let (tbl : Obj.t array) = input_value ic in
    let fold obj acc =
      let rec elem_of_obj obj =
        let tag = Obj.tag obj in
          if tag = Obj.string_tag then String (Obj.obj obj)
          else if tag = Obj.double_tag then Float (Obj.obj obj)
          else if tag = Obj.double_array_tag then Floats (Obj.obj obj)
          else if tag = Obj.int_tag then Int (Obj.obj obj)
          else if tag = Obj.out_of_heap_tag then Out_of_heap (Obj.obj obj)
          else if tag = Obj.custom_tag then
            let key = Obj.field obj 0 in
              if key = Obj.field (Obj.repr 0l) 0 then
                Int_32 (Obj.obj obj)
              else if key = Obj.field (Obj.repr 0L) 0 then
                Int_64 (Obj.obj obj)
              else
                let size = Obj.size obj in
                let tab = Array.make (4 * size) 0 in
                  for i = 0 to size - 1 do
                    let d = Obj.field obj i in
                    let o = Obj.obj d in
                      tab.(4*i) <-
                        if Obj.tag obj = Obj.int_tag then ((2 * o + 1) land 255)
                        else (2 * o) land 255;
                      tab.(4*i+1) <- (o lsr 7) land 255;
                      tab.(4*i+2) <- (o lsr 15) land 255;
                      tab.(4*i+3) <- (o lsr 23) land 255;
                  done;
                  Custom tab
          else
            let size = Obj.size obj in
            let tab =
              Array.init size (fun i -> elem_of_obj (Obj.field obj i))
            in
              if tag = Obj.closure_tag then Closure tab
              else Block (tab, tag)
      in
        elem_of_obj obj :: acc
    in
      Array.fold_right fold tbl []
;;

let print oc data =
  let rec print_elem elem =
    match elem with
      | String str -> Printf.fprintf oc "%S" str
      | Float f    -> Printf.fprintf oc "%f" f
      | Floats tbl ->
          Printf.fprintf oc "[|";
          Array.iter (Printf.fprintf oc " %f ") tbl;
          Printf.fprintf oc "|]";
      | Int n      -> Printf.fprintf oc "%d" n
      | Int_32 n -> Printf.fprintf oc "%ld : Int32.t" n
      | Int_64 n -> Printf.fprintf oc "%Ld : Int64.t" n
      | Custom tab ->
          output_string oc "custom block: [| ";
          for i = 0 to Array.length tab - 1 do
            Printf.fprintf oc "%02x" tab.(i / 4 * 4 + 3 - i mod 4);
            if i mod 4 = 3 then output_char oc ' ';
          done;
          output_string oc "|]";
      | Block (tab, tag) ->
          Printf.fprintf oc "[%d|" tag;
          Array.iter
            (fun e -> output_char oc ' ' ; print_elem e ; output_char oc ' ')
            tab;
          Printf.fprintf oc "|%d]" tag;
      | Closure tab ->
          Printf.fprintf oc "Closure [|";
          Array.iter
            (fun e -> output_char oc ' ' ; print_elem e ; output_char oc ' ')
            tab;
          Printf.fprintf oc "|]";
      | Out_of_heap p -> Printf.fprintf oc "@0x%08x" p;
  in
  let rec print_elems i elems =
    match elems with
      | elem :: others ->
          Printf.fprintf oc "%-3d   " i;
          print_elem elem;
          output_char oc '\n';
          print_elems (i + 1) others;
      | [] -> ()
  in
    Printf.fprintf oc "\n\
*********************\n\
***  Global data  ***\n\
*********************\n\
\n\
";
    print_elems 0 data
;;
