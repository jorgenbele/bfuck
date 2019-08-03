type linecol = int*int

datatype token = TIncPtr of linecol | TDecPtr of linecol
                 | TInc of linecol | TDec of linecol
                 | TOutputChr of linecol | TInputChr of linecol
                 | TLoopBegin of linecol | TLoopEnd of linecol

fun lex (l, c) nil = nil
  | lex (l, c) (#">" :: cs)  = TIncPtr    (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"<" :: cs)  = TDecPtr    (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"+" :: cs)  = TInc       (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"-" :: cs)  = TDec       (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"." :: cs)  = TOutputChr (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"," :: cs)  = TInputChr  (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"[" :: cs)  = TLoopBegin (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"]" :: cs)  = TLoopEnd   (l, c) :: lex (l, c+1) cs
  | lex (l, c) (#"\n" :: cs) = lex (l+1, c) cs
  | lex (l, c) (_ :: cs)     = lex (l, c+1) cs


datatype node = IncPtr of int | DecPtr of int
                | Inc of int | Dec of int
                | OutputChr of int | InputChr of int
                | Set of int

                (* Increase or decrease the cell at a given offset from cell_ptr *)
                | IncPtrOffset of int*int | DecPtrOffset of int*int (* (offset, n) *)
                | Loop of node list

type ast = node list

exception SyntaxError of string
exception ParseError of string


fun parse nil = nil
  | parse (TLoopBegin lc :: ts) =
    let
        val (loop, ts') = parse_loop lc nil ts
    in
        loop :: parse ts'
    end
  | parse (TLoopEnd lc :: ts) = raise SyntaxError "Unexpected token in parse, TLoopEnd, not in a loop"
  | parse (t :: ts) = parse_single t :: parse ts

and parse_single (TIncPtr _) = IncPtr 1
  | parse_single (TDecPtr _) = DecPtr 1
  | parse_single (TInc _) = Inc 1
  | parse_single (TDec _)  = Dec 1
  | parse_single (TOutputChr _)  = OutputChr 1
  | parse_single (TInputChr _)   = InputChr 1
  | parse_single _ = raise ParseError "Unexpected token"

and parse_loop lbegin acc nil = raise SyntaxError ("Missing TLoopEnd for loop starting at " ^ (Int.toString 10))
  | parse_loop lbegin acc (TLoopBegin lc :: ts) =
    let
        val (loop, ts') = parse_loop lc nil ts
    in
        parse_loop lbegin (acc @ [loop]) ts'
    end
  | parse_loop lbegin acc (TLoopEnd lc :: ts) = (Loop acc, ts)
  | parse_loop lbegin acc (t :: ts) = parse_loop lbegin (acc @ [parse_single t]) ts

(* Combine repeated commands: [Inc 1, Inc 1, Inc 1] => [Inc 3] *)
fun ast_combine_repeated nil = nil
    | ast_combine_repeated (IncPtr n1 :: IncPtr n2 :: ns) = ast_combine_repeated (IncPtr (n1 + n2) :: ns)
    | ast_combine_repeated (DecPtr n1 :: DecPtr n2 :: ns) = ast_combine_repeated (DecPtr (n1 + n2) :: ns)
    | ast_combine_repeated (Inc n1 :: Inc n2 :: ns) = ast_combine_repeated (Inc (n1 + n2) :: ns)
    | ast_combine_repeated (Dec n1 :: Dec n2 :: ns) = ast_combine_repeated (Dec (n1 + n2) :: ns)

    | ast_combine_repeated (Inc n1 :: Dec n2 :: ns) =
      ast_combine_repeated (if n1 = n2 then
                                ns
                            else
                                (if n1 > n2 then
                                     Inc (n1 - n2)
                                 else
                                     Dec (n2 - n1)) :: ns)

    | ast_combine_repeated (OutputChr n1 :: OutputChr n2 :: ns) = ast_combine_repeated (OutputChr (n1 + n2) :: ns)
    | ast_combine_repeated (InputChr n1 :: InputChr n2 :: ns) = ast_combine_repeated (InputChr (n1 + n2) :: ns)
    | ast_combine_repeated (Loop [Dec 1] :: ns) = Set 0 :: ast_combine_repeated ns
    | ast_combine_repeated (Loop l :: ns) = Loop (ast_combine_repeated l) :: ast_combine_repeated ns
    | ast_combine_repeated (n :: ns) = n :: ast_combine_repeated ns

fun ptroffset n =
  if n < 0 then
      DecPtr n
  else
      IncPtr n

(* fun ast_optimize_loop nil = nil *)
(*   | ast_optimize_loop (IncPtr n1 :: Inc n2 :: DecPtr n3 :: ns) = IncPtrOffset (n1, n2) :: ptroffset (n1-n3) :: ast_optimize_loop ns *)
(*   | ast_optimize_loop (DecPtr n1 :: Inc n2 :: IncPtr n3 :: ns) = IncPtrOffset (~n1, n2) :: ptroffset (n3-n1) :: ast_optimize_loop ns *)

(*   | ast_optimize_loop (IncPtr n1 :: Dec n2 :: DecPtr n3 :: ns) = DecPtrOffset (n1, n2) :: ptroffset (n1-n3) :: ast_optimize_loop ns *)
(*   | ast_optimize_loop (DecPtr n1 :: Dec n2 :: IncPtr n3 :: ns) = DecPtrOffset (~n1, n2) :: ptroffset (n3-n1) :: ast_optimize_loop ns *)

(*   | ast_optimize_loop (Loop l :: ns) = Loop (ast_optimize_loop l) :: ast_optimize_loop ns *)
(*   | ast_optimize_loop (n :: ns) = n :: ast_optimize_loop ns *)

exception InterpreterError of string

fun output_chars cell_ptr cells 0 = ()
  | output_chars cell_ptr cells n = (print (String.str (chr (Array.sub (cells, cell_ptr)))); output_chars cell_ptr cells (n - 1))

fun input_chars cell_ptr cells 0 = ()
  | input_chars cell_ptr cells n =
    let
        val inp_opt : char option = (TextIO.input1 TextIO.stdIn)
    in
        (* EOF sets the cell to 0 *)
        (case inp_opt of
             SOME c => ((Array.update (cells, cell_ptr, (ord c))); input_chars cell_ptr cells (n - 1))
           | NONE => (Array.update (cells, cell_ptr, 0); input_chars cell_ptr cells (n - 1)))
             (* raise InterpreterError "Expected input, got EOF") *)
    end

fun transpile_to_c ast = concatstrs ("#include<stdio.h>\nint main(int argc,char *argv[]){char m[1048576]={0}; char *p=m;" :: bf2c ast @ ["return 0;}"])

and concatstrs nil = ""
  | concatstrs (s :: nil) = s
  | concatstrs (s :: s') = s ^ concatstrs s'

and cexpr left optindex assign_op right =
    (case optindex of
         SOME (index:int) => left ^ "[" ^ (Int.toString index) ^ "]"
      | NONE => left) ^ assign_op ^ (Int.toString right) ^ ";"

and repeatstr str 0 = ""
  | repeatstr str n = str ^ (repeatstr str (n - 1))

and bf2c nil = nil
  | bf2c (IncPtr n :: ns) = cexpr "p" NONE "+=" n :: bf2c ns
  | bf2c (DecPtr n :: ns) = cexpr "p" NONE "-=" n :: bf2c ns

  | bf2c (Set n :: ns) = cexpr "p" (SOME 0) "=" n :: bf2c ns
  | bf2c (IncPtrOffset (offset, n) :: ns) = cexpr "p" (SOME offset) "+=" n :: bf2c ns
  | bf2c (DecPtrOffset (offset, n) :: ns) = cexpr "p" (SOME offset) "-=" n :: bf2c ns

  | bf2c (Inc n :: ns) = cexpr "p" (SOME 0) "+=" n :: bf2c ns
  | bf2c (Dec n :: ns) = cexpr "p" (SOME 0) "-=" n :: bf2c ns

  | bf2c (OutputChr n :: ns) = (repeatstr "putchar(*p);" n) :: bf2c ns
  | bf2c (InputChr n :: ns) = (repeatstr  "*p=getchar();if(*p == EOF){*p=0;} " n) :: bf2c ns
  | bf2c (Loop l :: ns) =
    "while(*p){" :: bf2c l @ ["}"] @ bf2c ns


fun getcell cells cell_ptr = Array.sub (cells, cell_ptr)
fun setcell cells cell_ptr value = Array.update (cells, cell_ptr, value mod 256)
fun inccell cells cell_ptr inc = Array.update (cells, cell_ptr, ((getcell cells cell_ptr) + inc) mod 256)

fun interpret nil cell_ptr cells = (cell_ptr, cells)
  | interpret (IncPtr n :: ns) cell_ptr cells = interpret ns (cell_ptr+n) cells
  | interpret (DecPtr n :: ns) cell_ptr cells = interpret ns (cell_ptr-n) cells

  | interpret (Set n :: ns) cell_ptr cells = (Array.update (cells, cell_ptr, n mod 256); interpret ns cell_ptr cells)
  | interpret (IncPtrOffset (offset, n) :: ns) cell_ptr cells = (inccell cells (cell_ptr + offset) n  ; interpret ns cell_ptr cells)
  | interpret (DecPtrOffset (offset, n) :: ns) cell_ptr cells = (inccell cells (cell_ptr + offset) (~n) ; interpret ns cell_ptr cells)

  | interpret (Inc n :: ns) cell_ptr cells = (inccell cells cell_ptr n    ; interpret ns cell_ptr cells)
  | interpret (Dec n :: ns) cell_ptr cells = (inccell cells cell_ptr (~n) ; interpret ns cell_ptr cells)

  | interpret (OutputChr n :: ns) cell_ptr cells = (output_chars cell_ptr cells n; interpret ns cell_ptr cells)
  | interpret (InputChr n :: ns) cell_ptr cells = (input_chars cell_ptr cells n; interpret ns cell_ptr cells)
  | interpret (Loop l :: ns) cell_ptr cells =
    let
        val v = Array.sub (cells, cell_ptr)
    in
        if v = 0 then
            (* continue past end of loop *)
            interpret ns cell_ptr cells
        else
            (* evaluate loop *)
            let
                val (cell_ptr', cells') = interpret l cell_ptr cells
                val v = Array.sub (cells', cell_ptr')
            in
                (* evaluated loop, check if it should be repeated *)
                if v = 0 then
                    (* break out of loop *)
                    interpret ns cell_ptr' cells'
                else
                    (* evaluate loop again *)
                    interpret (Loop l :: ns) cell_ptr' cells'
            end

    end

fun time_it (action, arg) = let
  val timer = Timer.startCPUTimer ()
  val _ = action arg
  val times = Timer.checkCPUTimer timer
in
  Time.+ (#usr times, #sys times)
end

fun main () =
    let
        val prog = hd (CommandLine.arguments ())
        (* val inp = TextIO.inputAll TextIO.stdIn *)
        val inp = TextIO.inputAll (TextIO.openIn prog)
        val tokens = (lex (0,0) (explode inp))
        val ast = parse tokens
        val optimized_reps = ast_combine_repeated ast
        (* val optimized_ast = ast_optimize_loop optimized_reps *)
        val cells = Array.array (100000, 0)
        (* val interpret_ = interpret optimized_ast 0 *)
        val interpret_ = interpret optimized_reps 0
    in
        print ("interpret takes " ^ Time.toString (time_it (interpret_, cells)) ^ " seconds.\n")
    end

(* fun main () = *)
(*     let *)
(*         val prog = hd (CommandLine.arguments ()) *)
(*         (* val inp = TextIO.inputAll TextIO.stdIn *) *)
(*         val inp = TextIO.inputAll (TextIO.openIn prog) *)
(*         val tokens = (lex (0,0) (explode inp)) *)
(*         val ast = parse tokens *)
(*         val optimized_reps = ast_combine_repeated ast *)
(*         val optimized_ast = ast_optimize_loop optimized_reps *)
(*     in *)
(*         print (transpile_to_c optimized_ast) *)
(*     end *)

val _ = main ()
