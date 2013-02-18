type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
       | AssignStm of id * exp
       | PrintStm of exp list

     and exp = IdExp of id
       | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val prog2 = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b", NumExp 3, NumExp 5]))

fun interp(stm) =
  let
    fun interpStm(stm, tbl) =
      case stm of
        CompoundStm(stm1, stm2) => interpStm(stm2, interpStm(stm1, tbl))
        | AssignStm(id, exp) => (id, (#2 interpExp(exp,tbl)))::tbl
        | PrintStm exps => foldl (fn(e, i)=> (#2 interpExp(e,i))) tbl exps
        (* exp * (string * int) list -> int * (string * int) list *)
    and interpExp(exp, tbl) =
      case exp of
        NumExp i => (i, tbl)
        | OpExp(exp1, bop, exp2) =>
          let 
            val x=interpExp(exp1, tbl)
            val y=interpExp(exp2, #2 x)
          in
            case bop of
              Plus => ((#1 x) + (#1 y), (#2 y))
              | Minus => ((#1 x) + (#1 y), (#2 y))
              | Times => ((#1 x) + (#1 y), (#2 y))
              | Div => ((#1 x) + (#1 y), (#2 y))
          end
        | EseqExp(stm1, exp1) => interpExp(exp1, interpStm(stm1, tbl))


  in
    interpStm(stm, [])
  end