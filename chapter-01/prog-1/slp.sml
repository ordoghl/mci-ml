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

fun maxargs(stm) =
  let 
    fun count_in_exp(exp, acc) =
      case exp of
        OpExp(exp1, _, exp2) => Int.max(count_in_exp(exp1,acc), count_in_exp(exp2,acc))
        | EseqExp(stm1, exp1) => Int.max(count_in_stmt(stm1, acc), count_in_exp(exp1,acc))
        | _ => acc
    and count_in_stmt(stm, acc) =
      case stm of
        CompoundStm(stm1, stm2) => Int.max(count_in_stmt(stm1, acc), count_in_stmt(stm2, acc))
        | AssignStm(_, exp) => count_in_exp(exp, acc)
        | PrintStm(exps) => Int.max(List.length(exps),  (foldl (fn(exp, i) => Int.max(i, count_in_exp(exp, i))) acc exps))
  in
    count_in_stmt(stm, 0)
  end
