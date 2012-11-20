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

type instr = {
  addr : int;
  mutable bc : bc;
  mutable is_pointed : bool;
}
and ptr = {
  ofs : int;
  mutable pointed : instr;
}
and bc =
  | Acc of int
  | Push
  | Pushacc of int
  | Pop of int
  | Assign of int
  | Envacc of int
  | Pushenvacc of int
  | Pushretaddr of ptr
  | Apply of int
  | Appterm of int * int
  | Return of int
  | Restart
  | Grab of int
  | Closure of int * ptr
  | Closurerec of int * int * ptr * ptr array
  | Offsetclosurem2
  | Offsetclosure of int
  | Pushoffsetclosurem2
  | Pushoffsetclosure of int
  | Getglobal of int
  | Pushgetglobal of int
  | Getglobalfield of int * int
  | Pushgetglobalfield of int * int
  | Setglobal of int
  | Atom of int
  | Pushatom of int
  | Makeblock of int * int
  | Makefloatblock of int
  | Getfield of int
  | Getfloatfield of int
  | Setfield of int
  | Setfloatfield of int
  | Vectlength
  | Getvectitem
  | Setvectitem
  | Getstringchar
  | Setstringchar
  | Branch of ptr
  | Branchif of ptr
  | Branchifnot of ptr
  | Switch of int * ptr array
  | Boolnot
  | Pushtrap of ptr
  | Poptrap
  | Raise
  | Checksignals
  | Ccall of int * int
  | Const of int
  | Pushconst of int
  | Negint
  | Addint
  | Subint
  | Mulint
  | Divint
  | Modint
  | Andint
  | Orint
  | Xorint
  | Lslint
  | Lsrint
  | Asrint
  | Eq
  | Neq
  | Ltint
  | Leint
  | Gtint
  | Geint
  | Offsetint of int
  | Offsetref of int
  | Isint
  | Getmethod
  | Beq of int * ptr
  | Bneq of int * ptr
  | Blint of int * ptr
  | Bleint of int * ptr
  | Bgtint of int * ptr
  | Bgeint of int * ptr
  | Ultint
  | Ugeint
  | Bultint of int * ptr
  | Bugeint of int * ptr
  | Getpubmet of int * int
  | Getdynmet
  | Stop
  | Event
  | Break
;;

let parse read =
  let read_ptr () = {
    ofs = read ();
    pointed = { addr = -1 ; bc = Break ; is_pointed = false };
  } in
  let opcode = read () in
    try
      match opcode with
        | 0 ->   Acc 0
        | 1 ->   Acc 1
        | 2 ->   Acc 2
        | 3 ->   Acc 3
        | 4 ->   Acc 4
        | 5 ->   Acc 5
        | 6 ->   Acc 6
        | 7 ->   Acc 7
        | 8 ->   Acc (read ())
        | 9 ->   Push
        | 10 ->  Pushacc 0
        | 11 ->  Pushacc 1
        | 12 ->  Pushacc 2
        | 13 ->  Pushacc 3
        | 14 ->  Pushacc 4
        | 15 ->  Pushacc 5
        | 16 ->  Pushacc 6
        | 17 ->  Pushacc 7
        | 18 ->  Pushacc (read ())
        | 19 ->  Pop (read ())
        | 20 ->  Assign (read ())
        | 21 ->  Envacc 1
        | 22 ->  Envacc 2
        | 23 ->  Envacc 3
        | 24 ->  Envacc 4
        | 25 ->  Envacc (read ())
        | 26 ->  Pushenvacc 1
        | 27 ->  Pushenvacc 2
        | 28 ->  Pushenvacc 3
        | 29 ->  Pushenvacc 4
        | 30 ->  Pushenvacc (read ())
        | 31 ->  Pushretaddr (read_ptr ())
        | 32 ->  Apply (read ())
        | 33 ->  Apply 1
        | 34 ->  Apply 2
        | 35 ->  Apply 3
        | 36 ->  let n = read () in let s = read () in Appterm (n, s)
        | 37 ->  Appterm (1, (read ()))
        | 38 ->  Appterm (2, (read ()))
        | 39 ->  Appterm (3, (read ()))
        | 40 ->  Return (read ())
        | 41 ->  Restart
        | 42 ->  Grab (read ())
        | 43 ->  let n = read () in let ptr = read_ptr () in Closure (n, ptr)
        | 44 ->
            let f = read () in
            let v = read () in
            let o = read_ptr () in
            let t = if f = 1 then [||] else
              let t = Array.make (f - 1) (read_ptr ()) in
                for i = 1 to f - 2 do t.(i) <- read_ptr () done ; t
            in
              Closurerec (f, v, o, t)
        | 45 ->  Offsetclosurem2
        | 46 ->  Offsetclosure 0
        | 47 ->  Offsetclosure 2
        | 48 ->  Offsetclosure (read ())
        | 49 ->  Pushoffsetclosurem2
        | 50 ->  Pushoffsetclosure 0
        | 51 ->  Pushoffsetclosure 2
        | 52 ->  Pushoffsetclosure (read ())
        | 53 ->  Getglobal (read ())
        | 54 ->  Pushgetglobal (read ())
        | 55 ->  let n = read () in let p = read () in Getglobalfield (n, p)
        | 56 ->  let n = read () in let p = read () in Pushgetglobalfield (n, p)
        | 57 ->  Setglobal (read ())
        | 58 ->  Atom 0
        | 59 ->  Atom (read ())
        | 60 ->  Pushatom 0
        | 61 ->  Pushatom (read ())
        | 62 ->  let n = read () in let t = read () in Makeblock (n, t)
        | 63 ->  Makeblock (1, read ())
        | 64 ->  Makeblock (2, read ())
        | 65 ->  Makeblock (3, read ())
        | 66 ->  Makefloatblock (read ())
        | 67 ->  Getfield 0
        | 68 ->  Getfield 1
        | 69 ->  Getfield 2
        | 70 ->  Getfield 3
        | 71 ->  Getfield (read ())
        | 72 ->  Getfloatfield (read ())
        | 73 ->  Setfield 0
        | 74 ->  Setfield 1
        | 75 ->  Setfield 2
        | 76 ->  Setfield 3
        | 77 ->  Setfield (read ())
        | 78 ->  Setfloatfield (read ())
        | 79 ->  Vectlength
        | 80 ->  Getvectitem
        | 81 ->  Setvectitem
        | 82 ->  Getstringchar
        | 83 ->  Setstringchar
        | 84 ->  Branch (read_ptr ())
        | 85 ->  Branchif (read_ptr ())
        | 86 ->  Branchifnot (read_ptr ())
        | 87 ->
            let n = read () in
            let size_tag = n lsr 16 in
            let size_long = n land (1 lsl 16 - 1) in
            let size = size_tag + size_long in
            let tab = Array.init size (fun _ -> read_ptr ()) in
              Switch (n, tab)
        | 88 ->  Boolnot
        | 89 ->  Pushtrap (read_ptr ())
        | 90 ->  Poptrap
        | 91 ->  Raise
        | 92 ->  Checksignals
        | 93 ->  Ccall (1, read ())
        | 94 ->  Ccall (2, read ())
        | 95 ->  Ccall (3, read ())
        | 96 ->  Ccall (4, read ())
        | 97 ->  Ccall (5, read ())
        | 98 ->  let n = read () in let p = read () in Ccall (n, p)
        | 99 ->  Const 0
        | 100 -> Const 1
        | 101 -> Const 2
        | 102 -> Const 3
        | 103 -> Const (read ())
        | 104 -> Pushconst 0
        | 105 -> Pushconst 1
        | 106 -> Pushconst 2
        | 107 -> Pushconst 3
        | 108 -> Pushconst (read ())
        | 109 -> Negint
        | 110 -> Addint
        | 111 -> Subint
        | 112 -> Mulint
        | 113 -> Divint
        | 114 -> Modint
        | 115 -> Andint
        | 116 -> Orint
        | 117 -> Xorint
        | 118 -> Lslint
        | 119 -> Lsrint
        | 120 -> Asrint
        | 121 -> Eq
        | 122 -> Neq
        | 123 -> Ltint
        | 124 -> Leint
        | 125 -> Gtint
        | 126 -> Geint
        | 127 -> Offsetint (read ())
        | 128 -> Offsetref (read ())
        | 129 -> Isint
        | 130 -> Getmethod
        | 131 -> let v = read () in let ptr = read_ptr () in Beq (v, ptr)
        | 132 -> let v = read () in let ptr = read_ptr () in Bneq (v, ptr)
        | 133 -> let v = read () in let ptr = read_ptr () in Blint (v, ptr)
        | 134 -> let v = read () in let ptr = read_ptr () in Bleint (v, ptr)
        | 135 -> let v = read () in let ptr = read_ptr () in Bgtint (v, ptr)
        | 136 -> let v = read () in let ptr = read_ptr () in Bgeint (v, ptr)
        | 137 -> Ultint
        | 138 -> Ugeint
        | 139 -> let v = read () in let ptr = read_ptr () in Bultint (v, ptr)
        | 140 -> let v = read () in let ptr = read_ptr () in Bugeint (v, ptr)
        | 141 -> let v = read () in let ofs = read () in Getpubmet (v, ofs)
        | 142 -> Getdynmet
        | 143 -> Stop
        | 144 -> Event
        | 145 -> Break
        | _ -> failwith (Printf.sprintf "invalid opcode: %d" opcode)
    with End_of_file -> failwith "unexpected end of code section"
;;

let opcode_of_bc bc =
  match bc with
    | Acc 0 -> 0
    | Acc 1 -> 1
    | Acc 2 -> 2
    | Acc 3 -> 3
    | Acc 4 -> 4
    | Acc 5 -> 5
    | Acc 6 -> 6
    | Acc 7 -> 7
    | Acc _ -> 8
    | Push -> 9
    | Pushacc 0 -> 10
    | Pushacc 1 -> 11
    | Pushacc 2 -> 12
    | Pushacc 3 -> 13
    | Pushacc 4 -> 14
    | Pushacc 5 -> 15
    | Pushacc 6 -> 16
    | Pushacc 7 -> 17
    | Pushacc _ -> 18
    | Pop _ -> 19
    | Assign _ -> 20
    | Envacc 1 -> 21
    | Envacc 2 -> 22
    | Envacc 3 -> 23
    | Envacc 4 -> 24
    | Envacc _ -> 25
    | Pushenvacc 1 -> 26
    | Pushenvacc 2 -> 27
    | Pushenvacc 3 -> 28
    | Pushenvacc 4 -> 29
    | Pushenvacc _ -> 30
    | Pushretaddr _ -> 31
    | Apply 1 -> 33
    | Apply 2 -> 34
    | Apply 3 -> 35
    | Apply _ -> 32
    | Appterm (1, _) -> 37
    | Appterm (2, _) -> 38
    | Appterm (3, _) -> 39
    | Appterm (_, _) -> 36
    | Return _ -> 40
    | Restart -> 41
    | Grab _ -> 42
    | Closure (_, _)  -> 43
    | Closurerec (_, _, _, _) -> 44
    | Offsetclosurem2 -> 45
    | Offsetclosure 0 -> 46
    | Offsetclosure 2 -> 47
    | Offsetclosure _ -> 48
    | Pushoffsetclosurem2 -> 49
    | Pushoffsetclosure 0 -> 50
    | Pushoffsetclosure 2 -> 51
    | Pushoffsetclosure _ -> 52
    | Getglobal _ -> 53
    | Pushgetglobal _ -> 54
    | Getglobalfield (_, _) -> 55
    | Pushgetglobalfield (_, _) -> 56
    | Setglobal _ -> 57
    | Atom 0 -> 58
    | Atom _ -> 59
    | Pushatom 0 -> 60
    | Pushatom _ -> 61
    | Makeblock (1, _) -> 63
    | Makeblock (2, _) -> 64
    | Makeblock (3, _) -> 65
    | Makeblock (_, _) -> 62
    | Makefloatblock _ -> 66
    | Getfield 0 -> 67
    | Getfield 1 -> 68
    | Getfield 2 -> 69
    | Getfield 3 -> 70
    | Getfield _ -> 71
    | Getfloatfield _ -> 72
    | Setfield 0 -> 73
    | Setfield 1 -> 74
    | Setfield 2 -> 75
    | Setfield 3 -> 76
    | Setfield _ -> 77
    | Setfloatfield _ -> 78
    | Vectlength -> 79
    | Getvectitem -> 80
    | Setvectitem -> 81
    | Getstringchar -> 82
    | Setstringchar -> 83
    | Branch _ -> 84
    | Branchif _ -> 85
    | Branchifnot _ -> 86
    | Switch (_, _) -> 87
    | Boolnot -> 88
    | Pushtrap _ -> 89
    | Poptrap -> 90
    | Raise -> 91
    | Checksignals -> 92
    | Ccall (1, _) -> 93
    | Ccall (2, _) -> 94
    | Ccall (3, _) -> 95
    | Ccall (4, _) -> 96
    | Ccall (5, _) -> 97
    | Ccall (_, _) -> 98
    | Const 0 -> 99
    | Const 1 -> 100
    | Const 2 -> 101
    | Const 3 -> 102
    | Const _ -> 103
    | Pushconst 0 -> 104
    | Pushconst 1 -> 105
    | Pushconst 2 -> 106
    | Pushconst 3 -> 107
    | Pushconst _ -> 108
    | Negint -> 109
    | Addint -> 110
    | Subint -> 111
    | Mulint -> 112
    | Divint -> 113
    | Modint -> 114
    | Andint -> 115
    | Orint  -> 116
    | Xorint -> 117
    | Lslint -> 118
    | Lsrint -> 119
    | Asrint -> 120
    | Eq  -> 121
    | Neq -> 122
    | Ltint -> 123
    | Leint -> 124
    | Gtint -> 125
    | Geint -> 126
    | Offsetint _ -> 127
    | Offsetref _ -> 128
    | Isint -> 129
    | Getmethod -> 130
    | Beq (_, _) -> 131
    | Bneq (_, _) -> 132
    | Blint (_, _) -> 133
    | Bleint (_, _) -> 134
    | Bgtint (_, _) -> 135
    | Bgeint (_, _) -> 136
    | Ultint -> 137
    | Ugeint -> 138
    | Bultint (_, _) -> 139
    | Bugeint (_, _) -> 140
    | Getpubmet (_, _) -> 141
    | Getdynmet -> 142
    | Stop -> 143
    | Event -> 144
    | Break -> 145
;;

let string_of_bc globname bc =
  match bc with
    | Acc n             -> Printf.sprintf "ACC %d" n
    | Push              -> Printf.sprintf "PUSH"
    | Pushacc n         -> Printf.sprintf "PUSHACC %d" n
    | Pop n             -> Printf.sprintf "POP %d" n
    | Assign n          -> Printf.sprintf "ASSIGN %d" n
    | Envacc n          -> Printf.sprintf "ENVACC %d" n
    | Pushenvacc n      -> Printf.sprintf "PUSHENVACC %d" n
    | Pushretaddr ptr   -> Printf.sprintf "PUSHRETADDR %d" ptr.pointed.addr
    | Apply n           -> Printf.sprintf "APPLY %d" n
    | Appterm (n,s)     -> Printf.sprintf "APPTERM %d %d" n s
    | Return n          -> Printf.sprintf "RETURN %d" n
    | Restart           -> Printf.sprintf "RESTART"
    | Grab n            -> Printf.sprintf "GRAB %d" n
    | Closure (n,ptr)   -> Printf.sprintf "CLOSURE %d %d" n ptr.pointed.addr
    | Closurerec (f,v,{pointed={addr=a; bc=_;is_pointed=_}; ofs=_},t) ->
        let b = Buffer.create 16 in
          Printf.bprintf b "CLOSUREREC %d %d %d [" f v a;
          Array.iter(fun ptr -> Printf.bprintf b " %d " ptr.pointed.addr) t;
          Printf.bprintf b "]";
          Buffer.contents b
    | Offsetclosurem2       -> Printf.sprintf "OFFSETCLOSUREM2"
    | Offsetclosure n       -> Printf.sprintf "OFFSETCLOSURE %d" n
    | Pushoffsetclosurem2   -> Printf.sprintf "PUSHOFFSETCLOSUREM2"
    | Pushoffsetclosure n   -> Printf.sprintf "PUSHOFFSETCLOSURE %d" n
    | Getglobal n           ->
        Printf.sprintf "GETGLOBAL %d (* %s *)" n (globname Globals.Global n)
    | Pushgetglobal n       ->
        Printf.sprintf "PUSHGETGLOBAL %d (* %s *)" n (globname Globals.Global n)
    | Getglobalfield (n,p)  -> Printf.sprintf "GETGLOBALFIELD %d %d" n p
    | Pushgetglobalfield (n,p) -> Printf.sprintf "PUSHGETGLOBALFIELD %d %d" n p
    | Setglobal n       -> Printf.sprintf "SETGLOBAL %d" n
    | Atom n            -> Printf.sprintf "ATOM %d" n
    | Pushatom n        -> Printf.sprintf "PUSHATOM %d" n
    | Makeblock (n,t)   -> Printf.sprintf "MAKEBLOCK %d %d" n t
    | Makefloatblock n  -> Printf.sprintf "MAKEFLOATBLOCK %d" n
    | Getfield n        -> Printf.sprintf "GETFIELD %d" n
    | Getfloatfield n   -> Printf.sprintf "GETFLOATFIELD %d" n
    | Setfield n        -> Printf.sprintf "SETFIELD %d" n
    | Setfloatfield n   -> Printf.sprintf "SETFLOATFIELD %d" n
    | Vectlength        -> Printf.sprintf "VECTLENGTH"
    | Getvectitem       -> Printf.sprintf "GETVECTITEM"
    | Setvectitem       -> Printf.sprintf "SETVECTITEM"
    | Getstringchar     -> Printf.sprintf "GETSTRINGCHAR"
    | Setstringchar     -> Printf.sprintf "SETSTRINGCHAR"
    | Branch ptr        -> Printf.sprintf "BRANCH %d" ptr.pointed.addr
    | Branchif ptr      -> Printf.sprintf "BRANCHIF %d" ptr.pointed.addr
    | Branchifnot ptr   -> Printf.sprintf "BRANCHIFNOT %d" ptr.pointed.addr
    | Switch (n, tab) ->
        let size_tag = n lsr 16 in
        let size_long = n land 0xFFFF in
        let b = Buffer.create 16 in
          Printf.bprintf b "SWITCH %d %d [" size_tag size_long;
          Array.iter
            (fun ptr -> Printf.bprintf b " %d " ptr.pointed.addr) tab;
          Printf.bprintf b "]";
          Buffer.contents b
    | Boolnot           -> Printf.sprintf "BOOLNOT"
    | Pushtrap ptr      -> Printf.sprintf "PUSHTRAP %d" ptr.pointed.addr
    | Poptrap           -> Printf.sprintf "POPTRAP"
    | Raise             -> Printf.sprintf "RAISE"
    | Checksignals      -> Printf.sprintf "CHECKSIGNALS"
    | Ccall (n, ind)    ->
        Printf.sprintf "CCALL %d %d (* %s() *)" n ind
          (globname Globals.Primitive ind)
    | Const n           -> Printf.sprintf "CONST %d" n
    | Pushconst n       -> Printf.sprintf "PUSHCONST %d" n
    | Negint            -> Printf.sprintf "NEGINT"
    | Addint            -> Printf.sprintf "ADDINT"
    | Subint            -> Printf.sprintf "SUBINT"
    | Mulint            -> Printf.sprintf "MULINT"
    | Divint            -> Printf.sprintf "DIVINT"
    | Modint            -> Printf.sprintf "MODINT"
    | Andint            -> Printf.sprintf "ANDINT"
    | Orint             -> Printf.sprintf "ORINT"
    | Xorint            -> Printf.sprintf "XORINT"
    | Lslint            -> Printf.sprintf "LSLINT"
    | Lsrint            -> Printf.sprintf "LSRINT"
    | Asrint            -> Printf.sprintf "ASRINT"
    | Eq                -> Printf.sprintf "EQ"
    | Neq               -> Printf.sprintf "NEQ"
    | Ltint             -> Printf.sprintf "LTINT"
    | Leint             -> Printf.sprintf "LEINT"
    | Gtint             -> Printf.sprintf "GTINT"
    | Geint             -> Printf.sprintf "GEINT"
    | Offsetint ofs     -> Printf.sprintf "OFFSETINT %d" ofs
    | Offsetref ofs     -> Printf.sprintf "OFFSETREF %d" ofs
    | Isint             -> Printf.sprintf "ISINT"
    | Getmethod         -> Printf.sprintf "GETMETHOD"
    | Beq (v,ptr)       -> Printf.sprintf "BEQ %d %d" v ptr.pointed.addr
    | Bneq (v,ptr)      -> Printf.sprintf "BNEQ %d %d" v ptr.pointed.addr
    | Blint (v,ptr)     -> Printf.sprintf "BLINT %d %d" v ptr.pointed.addr
    | Bleint (v,ptr)    -> Printf.sprintf "BLEINT %d %d" v ptr.pointed.addr
    | Bgtint (v,ptr)    -> Printf.sprintf "BGTINT %d %d" v ptr.pointed.addr
    | Bgeint (v,ptr)    -> Printf.sprintf "BGEINT %d %d" v ptr.pointed.addr
    | Ultint            -> Printf.sprintf "ULTINT"
    | Ugeint            -> Printf.sprintf "UGEINT"
    | Bultint (v,ptr)   -> Printf.sprintf "BULTINT %d %d" v ptr.pointed.addr
    | Bugeint (v,ptr)   -> Printf.sprintf "BUGEINT %d %d" v ptr.pointed.addr
    | Getpubmet (v,ofs) -> Printf.sprintf "GETPUBMET %d %d" v ofs
    | Getdynmet         -> Printf.sprintf "GETDYNMET"
    | Stop              -> Printf.sprintf "STOP"
    | Event             -> Printf.sprintf "EVENT"
    | Break             -> Printf.sprintf "BREAK"
;;

let print_instr global_info oc instr =
  Printf.fprintf oc "@ = %-5d    %s" (instr.addr)
    (string_of_bc (fun req slot -> global_info req slot instr.addr) instr.bc);
;;

let name_of_opcode opcode =
  match opcode with
    | 0 ->   "ACC0"
    | 1 ->   "ACC1"
    | 2 ->   "ACC2"
    | 3 ->   "ACC3"
    | 4 ->   "ACC4"
    | 5 ->   "ACC5"
    | 6 ->   "ACC6"
    | 7 ->   "ACC7"
    | 8 ->   "ACC"
    | 9 ->   "PUSH"
    | 10 ->  "PUSHACC0"
    | 11 ->  "PUSHACC1"
    | 12 ->  "PUSHACC2"
    | 13 ->  "PUSHACC3"
    | 14 ->  "PUSHACC4"
    | 15 ->  "PUSHACC5"
    | 16 ->  "PUSHACC6"
    | 17 ->  "PUSHACC7"
    | 18 ->  "PUSHACC"
    | 19 ->  "POP"
    | 20 ->  "ASSIGN"
    | 21 ->  "ENVACC1"
    | 22 ->  "ENVACC2"
    | 23 ->  "ENVACC3"
    | 24 ->  "ENVACC4"
    | 25 ->  "ENVACC"
    | 26 ->  "PUSHENVACC1"
    | 27 ->  "PUSHENVACC2"
    | 28 ->  "PUSHENVACC3"
    | 29 ->  "PUSHENVACC4"
    | 30 ->  "PUSHENVACC"
    | 31 ->  "PUSH-RETADDR"
    | 32 ->  "APPLY"
    | 33 ->  "APPLY1"
    | 34 ->  "APPLY2"
    | 35 ->  "APPLY3"
    | 36 ->  "APPTERM"
    | 37 ->  "APPTERM1"
    | 38 ->  "APPTERM2"
    | 39 ->  "APPTERM3"
    | 40 ->  "RETURN"
    | 41 ->  "RESTART"
    | 42 ->  "GRAB"
    | 43 ->  "CLOSURE"
    | 44 ->  "CLOSUREREC"
    | 45 ->  "OFFSETCLOSUREM2"
    | 46 ->  "OFFSETCLOSURE0"
    | 47 ->  "OFFSETCLOSURE2"
    | 48 ->  "OFFSETCLOSURE"
    | 49 ->  "PUSHOFFSETCLOSUREM2"
    | 50 ->  "PUSHOFFSETCLOSURE0"
    | 51 ->  "PUSHOFFSETCLOSURE2"
    | 52 ->  "PUSHOFFSETCLOSURE"
    | 53 ->  "GETGLOBAL"
    | 54 ->  "PUSHGETGLOBAL"
    | 55 ->  "GETGLOBALFIELD"
    | 56 ->  "PUSHGETGLOBALFIELD"
    | 57 ->  "SETGLOBAL"
    | 58 ->  "ATOM0"
    | 59 ->  "ATOM"
    | 60 ->  "PUSHATOM0"
    | 61 ->  "PUSHATOM"
    | 62 ->  "MAKEBLOCK"
    | 63 ->  "MAKEBLOCK1"
    | 64 ->  "MAKEBLOCK2"
    | 65 ->  "MAKEBLOCK3"
    | 66 ->  "MAKEFLOATBLOCK"
    | 67 ->  "GETFIELD0"
    | 68 ->  "GETFIELD1"
    | 69 ->  "GETFIELD2"
    | 70 ->  "GETFIELD3"
    | 71 ->  "GETFIELD"
    | 72 ->  "GETFLOATFIELD"
    | 73 ->  "SETFIELD0"
    | 74 ->  "SETFIELD1"
    | 75 ->  "SETFIELD2"
    | 76 ->  "SETFIELD3"
    | 77 ->  "SETFIELD"
    | 78 ->  "SETFLOATFIELD"
    | 79 ->  "VECTLENGTH"
    | 80 ->  "GETVECTITEM"
    | 81 ->  "SETVECTITEM"
    | 82 ->  "GETSTRINGCHAR"
    | 83 ->  "SETSTRINGCHAR"
    | 84 ->  "BRANCH"
    | 85 ->  "BRANCHIF"
    | 86 ->  "BRANCHIFNOT"
    | 87 ->  "SWITCH"
    | 88 ->  "BOOLNOT"
    | 89 ->  "PUSHTRAP"
    | 90 ->  "POPTRAP"
    | 91 ->  "RAISE"
    | 92 ->  "CHECK-SIGNALS"
    | 93 ->  "C-CALL1"
    | 94 ->  "C-CALL2"
    | 95 ->  "C-CALL3"
    | 96 ->  "C-CALL4"
    | 97 ->  "C-CALL5"
    | 98 ->  "C-CALLN"
    | 99 ->  "CONST0"
    | 100 -> "CONST1"
    | 101 -> "CONST2"
    | 102 -> "CONST3"
    | 103 -> "CONSTINT"
    | 104 -> "PUSHCONST0"
    | 105 -> "PUSHCONST1"
    | 106 -> "PUSHCONST2"
    | 107 -> "PUSHCONST3"
    | 108 -> "PUSHCONSTINT"
    | 109 -> "NEGINT"
    | 110 -> "ADDINT"
    | 111 -> "SUBINT"
    | 112 -> "MULINT"
    | 113 -> "DIVINT"
    | 114 -> "MODINT"
    | 115 -> "ANDINT"
    | 116 -> "ORINT"
    | 117 -> "XORINT"
    | 118 -> "LSLINT"
    | 119 -> "LSRINT"
    | 120 -> "ASRINT"
    | 121 -> "EQ"
    | 122 -> "NEQ"
    | 123 -> "LTINT"
    | 124 -> "LEINT"
    | 125 -> "GTINT"
    | 126 -> "GEINT"
    | 127 -> "OFFSETINT"
    | 128 -> "OFFSETREF"
    | 129 -> "ISINT"
    | 130 -> "GETMETHOD"
    | 131 -> "BEQ"
    | 132 -> "BNEQ"
    | 133 -> "BLINT"
    | 134 -> "BLEINT"
    | 135 -> "BGTINT"
    | 136 -> "BGEINT"
    | 137 -> "ULTINT"
    | 138 -> "UGEINT"
    | 139 -> "BULTINT"
    | 140 -> "BUGEINT"
    | 141 -> "GETPUBMET"
    | 142 -> "GETDYNMET"
    | 143 -> "STOP"
    | 144 -> "EVENT"
    | 145 -> "BREAK"
    | _ ->   failwith (Printf.sprintf "invalid opcode: %d" opcode)
;;
