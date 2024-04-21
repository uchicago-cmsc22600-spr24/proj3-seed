(* chk-exp.sml
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *)

structure ChkExp : sig

    (* convert a binding-tree expression to an AST expression, while checkinga
     * that it is well typed.s
     *)
    val check : Context.t * BindTree.exp -> AST.exp

    (* type check a value binding while converting it to AST *)
    val chkValBind : Context.t * BindTree.bind -> AST.bind

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Cov = Coverage
    structure Ty = Type
    structure TU = TypeUtil
    structure U = Unify

    (* an expression/type pair for when there is an error *)
    val bogusExp = AST.E(AST.TupleExp[], Ty.ErrorTy)

    fun check (cxt, exp) = (case exp
           of BT.MarkExp m => check (C.withMark (cxt, m))
            | _ => raise Fail "FIXME"
          (* end case *))

    (* typecheck a list of case-expression rules (including coverage checking) *)
    and chkRules (cxt, argTy, rules) = raise Fail "FIXME"

    and chkValBind (cxt, BT.MarkBind m) = chkValBind (C.withMark (cxt, m))
      | chkValBind _ = raise Fail "FIXME"

  end
